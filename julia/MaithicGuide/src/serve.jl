# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The model as a service: one request on a line, one answer on a line.

const SERVICE = "maithic.guide.service/1"

text_member(request, name) = (v = get(request, name, nothing); v isa AbstractString ? String(v) : throw(ArgumentError("the request needs the text member \"$(name)\"")))

function texts_member(request, name)
    v = get(request, name, nothing)
    v isa AbstractVector && all(x -> x isa AbstractString, v) || throw(ArgumentError("the request needs \"$(name)\", a list of texts"))
    String[String(x) for x in v]
end

"""
    answer(model, request)

The answer to one request, as an object with the `id` of the request. The
methods are

- `handshake`: the name of the protocol and the shape of the model;
- `tactic`, with `state` and `candidates`: a probability for each candidate,
  and the template each was read as;
- `value`, with `state`: an estimate of how near the state is to closed;
- `premise`, with `state` and `candidates`: a score for each premise name;
- `shutdown`.

A request that cannot be answered gets an answer with the member `error`;
the service goes on.
"""
function answer(model::GuideModel, request)
    id = request isa Object ? get(request, "id", nothing) : nothing
    try
        request isa Object || throw(ArgumentError("a request is a JSON object"))
        method = get(request, "method", nothing)
        if method == "handshake"
            Object("id" => id, "service" => SERVICE,
                   "model" => Object("dim" => model.config.dim, "max_length" => model.config.max_length, "pooling" => String(model.config.pooling),
                                     "mixer" => String(model.config.mixer), "vocabulary" => length(model.vocabulary), "classes" => length(model.templates)))
        elseif method == "tactic"
            candidates = texts_member(request, "candidates")
            Object("id" => id, "priors" => tactic_priors(model, text_member(request, "state"), candidates), "templates" => [template(c) for c in candidates])
        elseif method == "value"
            Object("id" => id, "value" => state_value(model, text_member(request, "state")))
        elseif method == "premise"
            Object("id" => id, "scores" => premise_scores(model, text_member(request, "state"), texts_member(request, "candidates")))
        elseif method == "shutdown"
            Object("id" => id, "ok" => true)
        else
            throw(ArgumentError(method isa AbstractString ? "there is no method \"$(method)\"" : "the request names no method"))
        end
    catch e
        e isa ArgumentError || rethrow()
        Object("id" => id, "error" => e.msg)
    end
end

"""
    serve(model; input = stdin, output = stdout)

Answers requests until the input ends or a `shutdown` arrives. Each line of
input is a JSON object and gets one line of output.
"""
function serve(model::GuideModel; input::IO = stdin, output::IO = stdout)
    for line in eachline(input)
        isempty(strip(line)) && continue
        request = try
            read_json(line)
        catch e
            e isa JSONError || rethrow()
            println(output, write_json(Object("id" => nothing, "error" => "the request is not JSON")))
            flush(output)
            continue
        end
        println(output, write_json(answer(model, request)))
        flush(output)
        request isa Object && get(request, "method", nothing) == "shutdown" && break
    end
    nothing
end
