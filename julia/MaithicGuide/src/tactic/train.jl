# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Training the tactic classifier.

"""
    TrainConfig(; model = ModelConfig(), optimizer = Adam(weight_decay = 1e-4), epochs = 30, batch = 16,
                  min_token_count = 2, min_template_count = 2, k = 5, seed = 0)

Everything a training run is decided by. `k` is the `k` of the top-k accuracy
that is reported; `seed` is the one seed of the run.
"""
Base.@kwdef struct TrainConfig
    model::ModelConfig = ModelConfig()
    optimizer::Adam = Adam(weight_decay = 1e-4)
    epochs::Int = 30
    batch::Int = 16
    min_token_count::Int = 2
    min_template_count::Int = 2
    k::Int = 5
    seed::Int = 0
end

"""
    EpochLog(epoch, train_loss, validation)

One epoch of a run: the mean loss over the training examples as they were
seen, and the model on the validation part after the epoch.
"""
struct EpochLog
    epoch::Int
    train_loss::Float64
    validation::Evaluation
end

"""
    TrainResult(model, log, best_epoch)

The model of the epoch with the lowest validation loss, the log of every
epoch, and which epoch that was.
"""
struct TrainResult
    model::GuideModel
    log::Vector{EpochLog}
    best_epoch::Int
end

copy_model(model::GuideModel) =
    GuideModel(model.config, model.vocabulary, model.templates, Embedding(copy(model.embedding.tokens), copy(model.embedding.positions)),
               model.attention === nothing ? nothing : Attention(copy(model.attention.query), copy(model.attention.key), copy(model.attention.value)),
               Head(copy(model.head.weights), copy(model.head.bias)))

"""
    train(train_examples, validation_examples; config = TrainConfig())

Trains a tactic classifier. The vocabulary and the templates are built from
the training examples alone. In each epoch the examples are seen once, in an
order drawn from the seed, in batches of `config.batch`, each followed by one
step of the optimizer; after each epoch the model is evaluated on the
validation examples, and the model that did best there is returned. Without
validation examples the model of the last epoch is returned.

The run is decided by the examples and the configuration: the same call
gives the same model, weight for weight.
"""
function train(train_examples::AbstractVector{Example}, validation_examples::AbstractVector{Example} = Example[]; config::TrainConfig = TrainConfig())
    isempty(train_examples) && throw(ArgumentError("there are no examples to train on"))
    config.epochs >= 1 || throw(ArgumentError("epochs must be at least 1"))
    config.batch >= 1 || throw(ArgumentError("batch must be at least 1"))
    check(config.optimizer)
    vocabulary = build_vocabulary((e.state for e in train_examples); min_count = config.min_token_count)
    templates = build_templates(train_examples; min_count = config.min_template_count)
    model = GuideModel(check(config.model), vocabulary, templates; seed = config.seed)
    encoded = [state_ids(vocabulary, e.state; max_length = config.model.max_length) for e in train_examples]
    targets = [class_of(templates, e.tactic) for e in train_examples]
    arrays = [array for (_, array) in parameters(model)]
    state = AdamState(arrays)
    order_stream = stream(config.seed, "order")
    order = collect(1:length(train_examples))
    log = EpochLog[]
    best, best_epoch, best_loss = copy_model(model), 0, Inf
    for epoch in 1:config.epochs
        MaithicCore.shuffle!(order_stream, order)
        total = 0.0
        for start in 1:config.batch:length(order)
            batch = @view order[start:min(start + config.batch - 1, length(order))]
            gradients = zero_gradients(model)
            for i in batch
                total += accumulate!(gradients, model, encoded[i], targets[i])
            end
            for g in gradients
                g ./= length(batch)
            end
            step!(config.optimizer, state, arrays, gradients)
        end
        validation = evaluate(model, validation_examples; k = config.k)
        push!(log, EpochLog(epoch, total / length(order), validation))
        if isempty(validation_examples) || validation.loss < best_loss
            best, best_epoch, best_loss = copy_model(model), epoch, validation.loss
        end
    end
    TrainResult(best, log, best_epoch)
end
