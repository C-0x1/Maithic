# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Models and the records of training runs, as JSON documents.

const MODEL_SCHEMA = "maithic.guide.model/1"
const RUN_SCHEMA = "maithic.guide.run/1"

columns(m::Matrix{Float64}) = [m[:, j] for j in axes(m, 2)]
rows(m::Matrix{Float64}) = [m[i, :] for i in axes(m, 1)]

function matrix_of(vectors, by::Symbol, what::String)
    isempty(vectors) && throw(ArgumentError("the model has no $(what)"))
    width = length(vectors[1])
    all(v -> v isa AbstractVector && length(v) == width && all(x -> x isa Real, v), vectors) ||
        throw(ArgumentError("the $(what) of the model are not vectors of one length"))
    m = Matrix{Float64}(undef, by === :columns ? (width, length(vectors)) : (length(vectors), width))
    for (k, v) in enumerate(vectors)
        by === :columns ? (m[:, k] .= v) : (m[k, :] .= v)
    end
    m
end

"""
    model_document(model)

A model as a document of the schema `maithic.guide.model/1`: its shape, its
vocabulary, its templates and every weight, written so that the numbers read
back exactly.
"""
function model_document(model::GuideModel)
    weights = Object("tokens" => columns(model.embedding.tokens), "positions" => columns(model.embedding.positions))
    if model.attention !== nothing
        weights["query"] = rows(model.attention.query)
        weights["key"] = rows(model.attention.key)
        weights["value"] = rows(model.attention.value)
    end
    weights["head"] = rows(model.head.weights)
    weights["bias"] = model.head.bias
    document(MODEL_SCHEMA,
             "config" => Object("dim" => model.config.dim, "max_length" => model.config.max_length, "pooling" => String(model.config.pooling),
                                "mixer" => String(model.config.mixer)),
             "vocabulary" => Object("tokens" => model.vocabulary.tokens, "counts" => model.vocabulary.counts),
             "templates" => Object("templates" => model.templates.templates, "counts" => model.templates.counts, "closes" => model.templates.closes),
             "weights" => weights)
end

"""
    model_of(document)

The model a document describes. The shapes are checked against each other:
a vector for every token, a row of the classifier for every template.
"""
function model_of(doc)
    expect_schema(doc, MODEL_SCHEMA)
    try
        c = doc["config"]
        config = check(ModelConfig(dim = c["dim"], max_length = c["max_length"], pooling = Symbol(c["pooling"]), mixer = Symbol(c["mixer"])))
        vocabulary = Vocabulary(String[String(t) for t in doc["vocabulary"]["tokens"]], Int[n for n in doc["vocabulary"]["counts"]])
        t = doc["templates"]
        templates = TemplateVocabulary(String[String(x) for x in t["templates"]], Int[n for n in t["counts"]], Float64[x for x in t["closes"]])
        w = doc["weights"]
        embedding = Embedding(matrix_of(w["tokens"], :columns, "token vectors"), matrix_of(w["positions"], :columns, "position vectors"))
        attention = config.mixer === :attention ?
            Attention(matrix_of(w["query"], :rows, "query weights"), matrix_of(w["key"], :rows, "key weights"), matrix_of(w["value"], :rows, "value weights")) : nothing
        head = Head(matrix_of(w["head"], :rows, "classifier weights"), Float64[x for x in w["bias"]])
        size(embedding.tokens) == (config.dim, length(vocabulary)) || throw(ArgumentError("the model does not have one vector of $(config.dim) numbers for each of its $(length(vocabulary)) tokens"))
        size(embedding.positions) == (config.dim, config.max_length) || throw(ArgumentError("the model does not have one vector for each of its $(config.max_length) positions"))
        size(head.weights) == (length(templates), config.dim) && length(head.bias) == length(templates) ||
            throw(ArgumentError("the classifier of the model does not have one row for each of its $(length(templates)) templates"))
        attention === nothing || all(m -> size(m) == (config.dim, config.dim), (attention.query, attention.key, attention.value)) ||
            throw(ArgumentError("the attention weights of the model are not square matrices of size $(config.dim)"))
        GuideModel(config, vocabulary, templates, embedding, attention, head)
    catch e
        e isa Union{KeyError,MethodError,TypeError} || rethrow()
        throw(ArgumentError("the document is not a complete model: a member is missing or of the wrong type"))
    end
end

"Writes a model to a file."
save_model(path::AbstractString, model::GuideModel) = write(path, write_json(model_document(model)), "\n")

"""
    load_model(path)

The model in a file written by [`save_model`](@ref).
"""
function load_model(path::AbstractString)
    isfile(path) || throw(ArgumentError("no model at $(path)"))
    doc = try
        read_json(read(path, String))
    catch e
        e isa JSONError || rethrow()
        throw(ArgumentError("$(path) is not JSON"))
    end
    model_of(doc)
end

finite_or_nothing(x::Real) = isfinite(x) ? x : nothing

evaluation_document(e::Evaluation) =
    Object("examples" => e.examples, "loss" => finite_or_nothing(e.loss), "top1" => finite_or_nothing(e.top1), "topk" => finite_or_nothing(e.topk), "k" => e.k)

parameter_count(model::GuideModel) = sum(length(array) for (_, array) in parameters(model))

"""
    run_document(name, config, examples, result)

The record of a training run, of the schema `maithic.guide.run/1`: what was
configured, what data went in, the loss and accuracy after every epoch, and
the chosen model on the test part beside the baseline that knows no state.
The record holds no times and no paths: the same run gives the same record.
"""
function run_document(name::AbstractString, config::TrainConfig, examples::Examples, result::TrainResult)
    model = result.model
    document(RUN_SCHEMA,
             "name" => String(name),
             "seed" => config.seed,
             "config" => Object("dim" => config.model.dim, "max_length" => config.model.max_length, "pooling" => String(config.model.pooling),
                                "mixer" => String(config.model.mixer), "epochs" => config.epochs, "batch" => config.batch,
                                "rate" => config.optimizer.rate, "weight_decay" => config.optimizer.weight_decay,
                                "decoupled" => config.optimizer.decoupled, "min_token_count" => config.min_token_count,
                                "min_template_count" => config.min_template_count, "k" => config.k),
             "dataset" => Object("name" => examples.dataset, "version" => examples.version, "pairs" => examples.pairs, "theorems" => examples.theorems,
                                 "train" => length(examples.train), "validation" => length(examples.validation), "test" => length(examples.test)),
             "vocabulary" => length(model.vocabulary),
             "classes" => length(model.templates),
             "parameters" => parameter_count(model),
             "epochs" => [Object("epoch" => e.epoch, "train_loss" => e.train_loss, "validation_loss" => finite_or_nothing(e.validation.loss),
                                 "validation_top1" => finite_or_nothing(e.validation.top1), "validation_topk" => finite_or_nothing(e.validation.topk))
                          for e in result.log],
             "best_epoch" => result.best_epoch,
             "test" => evaluation_document(evaluate(model, examples.test; k = config.k)),
             "baseline" => evaluation_document(baseline(model.templates, examples.test; k = config.k)))
end
