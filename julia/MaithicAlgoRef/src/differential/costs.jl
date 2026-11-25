# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Measuring: how many comparisons the Lean algorithms make as their input
# grows, next to the bounds that are proved of them.
#
# The Lean library counts the comparisons of four algorithms and proves a
# bound for each (`Maithic.Algo.Cost`). The executable answers a request
# `cost NAME ...` with the count. Here the requests are built for a series
# of sizes, on inputs that make each algorithm work, and the answers are
# laid in a table beside the proved bound for the same size.

"The algorithms whose comparisons the Lean library counts and bounds."
cost_names() = ["linear_search", "binary_search", "insertion_sort", "merge_sort"]

"The largest `k` with `2^k <= n`, and 0 for 0: what Lean calls `Nat.log2`."
floor_log2(n::Integer) = n <= 0 ? 0 : ndigits(n; base = 2) - 1

"""
    proven_bound(algorithm, n)

The number of comparisons that `Maithic.Algo.Cost` proves the algorithm
never exceeds on an input of `n` elements.
"""
function proven_bound(algorithm::AbstractString, n::Integer)
    n >= 0 || throw(ArgumentError("a size is not negative"))
    algorithm == "linear_search" && return Int(n)
    algorithm == "binary_search" && return floor_log2(n) + 1
    algorithm == "insertion_sort" && return div(Int(n) * (Int(n) - 1), 2)
    algorithm == "merge_sort" && return Int(n) * (floor_log2(n) + 1)
    throw(ArgumentError("the comparisons of '$algorithm' are not counted"))
end

"""
    scrambled(n)

The numbers 1 to `n` in an order without runs that a merge could profit
from. The order is fixed: no seed, and the same on every machine.
"""
scrambled(n::Integer) = sortperm([(i * 7919) % 104729 for i in 1:n])

"""
    cost_request(algorithm, n)

The request for the cost of an algorithm on `n` elements. The input makes
the algorithm work: a search for what is not there, insertion sort on a
falling list, merge sort on a scrambled one.
"""
function cost_request(algorithm::AbstractString, n::Integer)
    numbers(xs) = join(xs, " ")
    algorithm == "linear_search" && return "cost linear_search 0 | " * numbers(1:n)
    algorithm == "binary_search" && return "cost binary_search $(n + 1) | " * numbers(1:n)
    algorithm == "insertion_sort" && return "cost insertion_sort " * numbers(n:-1:1)
    algorithm == "merge_sort" && return "cost merge_sort " * numbers(scrambled(n))
    throw(ArgumentError("the comparisons of '$algorithm' are not counted"))
end

"One measurement: an algorithm, a size, the comparisons counted and the bound proved."
struct CostRow
    algorithm::String
    n::Int
    comparisons::Int
    bound::Int
end

"""
    line_transport(command)

A function that writes lines to an executable and returns the lines it
answers with, one for each.
"""
function line_transport(command::Cmd)
    function (lines::Vector{String})
        isempty(lines) && return String[]
        output = read(pipeline(command; stdin = IOBuffer(join(lines, "\n") * "\n"), stderr = devnull), String)
        String.(strip.(split(chomp(output), '\n')))
    end
end

const COST_SIZES = [8, 16, 32, 64, 128, 256, 512]

"""
    cost_table(ask; sizes = COST_SIZES, algorithms = cost_names())

Asks for the cost of every algorithm at every size and returns the rows.
`ask` takes the requests and returns one answer for each;
[`line_transport`](@ref) makes one of an executable.
"""
function cost_table(ask; sizes = COST_SIZES, algorithms = cost_names())
    wanted = [(String(a), Int(n)) for a in algorithms for n in sizes]
    answers = ask(String[cost_request(a, n) for (a, n) in wanted])
    length(answers) == length(wanted) || throw(ArgumentError("$(length(answers)) answer(s) came back for $(length(wanted)) request(s)"))
    map(zip(wanted, answers)) do ((a, n), answer)
        counted = tryparse(Int, strip(answer))
        counted === nothing && throw(ArgumentError("the cost of $a on $n element(s) was answered with: $answer"))
        CostRow(a, n, counted, proven_bound(a, n))
    end
end

"Whether no count exceeds the bound that is proved for it."
within_bounds(rows) = all(r -> r.comparisons <= r.bound, rows)

"The table as CSV: `algorithm,n,comparisons,bound`."
function cost_csv(rows)
    io = IOBuffer()
    println(io, "algorithm,n,comparisons,bound")
    for r in rows
        println(io, r.algorithm, ",", r.n, ",", r.comparisons, ",", r.bound)
    end
    String(take!(io))
end
