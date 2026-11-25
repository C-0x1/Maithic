# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# What a first request needs, run once while the package is compiled.
#
# A search starts the service and waits for its first answer. Julia compiles
# a function the first time it runs, and a service that compiled its reader,
# its model and its answers on the first request would keep the search
# waiting for seconds. The work below goes through the same code on a model
# of a few weights; Julia keeps what it compiled with the package, and the
# first request of a real service finds it there.

function warm_up()
    examples = [Example("t$(i)", i % 2 == 0 ? "n : Nat\nh : 0 < n\n⊢ n - 1 < n" : "xs : List Nat\n⊢ xs ++ [] = xs",
                        i % 2 == 0 ? "omega" : "simp [List.append_nil]", true) for i in 1:8]
    for (mixer, pooling) in ((:none, :mean), (:attention, :first))
        config = TrainConfig(model = ModelConfig(dim = 4, max_length = 16, pooling = pooling, mixer = mixer), epochs = 1, batch = 4,
                             min_token_count = 1, min_template_count = 1)
        model = train(examples, examples; config = config).model
        model = model_of(read_json(write_json(model_document(model))))
        requests = IOBuffer(join([write_json(Object("id" => 1, "method" => "handshake")),
                                  write_json(Object("id" => 2, "method" => "tactic", "state" => examples[1].state, "candidates" => Any["omega", "simp [h]", "rfl"])),
                                  write_json(Object("id" => 3, "method" => "value", "state" => examples[1].state)),
                                  write_json(Object("id" => 4, "method" => "premise", "state" => examples[1].state, "candidates" => Any["Nat.sub_lt"])),
                                  "not json", write_json(Object("id" => 5, "method" => "shutdown"))], "\n"))
        serve(model; input = requests, output = IOBuffer())
        evaluate(model, examples)
        baseline(model.templates, examples)
    end
    nothing
end

# Only while Julia writes the package's compiled form, never when it is loaded.
ccall(:jl_generating_output, Cint, ()) == 1 && warm_up()
