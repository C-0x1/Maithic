# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

"""
    MaithicGuide

Small models that guide a proof search.

A search over tactics tries candidates on a goal. Which to try first is worth
knowing, and proofs that already exist say something about it: on goals that
look like this one, which moves were made? This package trains a classifier
on the state-and-tactic pairs that `maithic corpus harvest` writes. It reads
a proof state as tokens of Lean and predicts the template of the next
tactic: the tactic with its arguments taken out. For the candidates of a
search it returns a probability each.

The models are small on purpose. They train in seconds on a laptop, they run
on the machine that runs the search, and they cost nothing per call.
Everything is written out here, the layers with their gradients and the
optimizer included, so the package needs nothing beyond Julia itself, and a
seed decides a training run completely.

What a model says is advice about the order of the search. It can make the
search faster or slower, never wrong: every proof is checked by Lean.
"""
module MaithicGuide

using MaithicCore
using MaithicData

# `evaluate` is the word of MaithicCore for the value of an expression. The
# evaluation of a model on examples is a method of the same function, so that
# a program that uses both packages has one `evaluate` and not a clash.
import MaithicCore: evaluate

export Token, lex, isliteral
export Vocabulary, build_vocabulary, token_id, PAD, UNKNOWN, UNKNOWN_NAME, UNKNOWN_NUMBER, UNKNOWN_STRING
export encode, decode, main_goal, conclusion_first, state_ids
export Embedding, embed, pool, Attention, attend, Head, ModelConfig, GuideModel, parameters
export template_scores, template_probabilities
export template, TemplateVocabulary, build_templates, class_of, OTHER
export Example, Examples, load_examples
export log_softmax, softmax, cross_entropy, cross_entropy_gradient
export Adam, AdamState, step!
export stream, normal!
export TrainConfig, EpochLog, TrainResult, train
export Evaluation, evaluate, topk_accuracy, top_templates, baseline
export tactic_priors, state_value, premise_scores
export save_model, load_model, model_document, model_of, run_document, MODEL_SCHEMA, RUN_SCHEMA
export serve, answer, SERVICE

const VERSION = v"0.1.0"

include("train/seed.jl")
include("tokenize/lexer.jl")
include("tokenize/vocabulary.jl")
include("tokenize/encode.jl")
include("tokenize/state.jl")
include("models/linear.jl")
include("models/embedding.jl")
include("models/pooling.jl")
include("models/attention.jl")
include("tactic/loss.jl")
include("tactic/templates.jl")
include("models/classifier.jl")
include("train/optimizer.jl")
include("tactic/dataset.jl")
include("tactic/accuracy.jl")
include("tactic/train.jl")
include("tactic/prior.jl")
include("models/store.jl")
include("serve.jl")
include("precompile.jl")

end # module
