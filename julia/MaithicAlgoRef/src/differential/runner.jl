# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The differential runner: the same requests to an implementation and to the
# reference, and every answer that the reference does not accept, reported.

"An answer that was not accepted."
struct Mismatch
    case::Case
    answer::String
end

"What a run found."
struct Report
    seed::Int
    size::Int
    cases::Int
    by_algorithm::Vector{Tuple{String,Int,Int}}   # name, cases, mismatches
    mismatches::Vector{Mismatch}
end

agrees(report::Report) = isempty(report.mismatches)

"""
    process_transport(command)

A transport that runs an executable, writes it the requests one to a line
and reads one line back for each.
"""
function process_transport(command::Cmd)
    function (batch::Vector{Case})
        isempty(batch) && return String[]
        input = join((c.request for c in batch), "\n") * "\n"
        output = read(pipeline(command; stdin = IOBuffer(input), stderr = devnull), String)
        answers = split(chomp(output), '\n')
        length(answers) == length(batch) ||
            throw(ArgumentError("the implementation gave $(length(answers)) answer(s) to $(length(batch)) request(s)"))
        String.(strip.(answers))
    end
end

"A transport that answers with the reference itself: every case agrees."
reference_transport() = (batch::Vector{Case}) -> String[c.sample for c in batch]

"""
    run_differential(transport; seed = 1, count = 190, size = 12, algorithms = reference_names())

Draws cases, hands them to the transport, and judges each answer. A
transport takes the cases and returns one answer for each;
[`process_transport`](@ref) makes one of an executable.
"""
function run_differential(transport; seed::Integer = 1, count::Integer = 190, size::Integer = 12, algorithms = reference_names())
    batch = cases(; seed, count, size, algorithms)
    answers = transport(batch)
    length(answers) == length(batch) || throw(ArgumentError("the transport gave $(length(answers)) answer(s) to $(length(batch)) case(s)"))
    mismatches = Mismatch[Mismatch(c, a) for (c, a) in zip(batch, answers) if !c.accept(a)]
    by_algorithm = Tuple{String,Int,Int}[(name, Base.count(c -> c.algorithm == name, batch), Base.count(m -> m.case.algorithm == name, mismatches))
                                         for name in algorithms]
    Report(Int(seed), Int(size), length(batch), by_algorithm, mismatches)
end

"""
    report_document(report; implementation = "")

The report in the schema `maithic.algolab.differential/1`.
"""
function report_document(report::Report; implementation::AbstractString = "")
    document("maithic.algolab.differential/1",
             "implementation" => String(implementation), "seed" => report.seed, "size" => report.size, "cases" => report.cases,
             "mismatches" => length(report.mismatches), "agrees" => agrees(report),
             "algorithms" => Any[Object("name" => name, "cases" => n, "mismatches" => bad) for (name, n, bad) in report.by_algorithm],
             "failures" => Any[Object("algorithm" => m.case.algorithm, "request" => m.case.request, "expected" => m.case.expected, "answer" => m.answer)
                               for m in report.mismatches])
end
