# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The simplex method in exact rational arithmetic.
#
# Small problems only: the tableau is dense and every entry is a ratio of big
# integers. In exchange there is no tolerance anywhere, and the dual values
# that come out are exactly the multipliers of a certificate.

"""
    SimplexResult

The outcome of [`simplex`](@ref) on `minimize c . y subject to M y = r, y >= 0`.

- `status`: `:optimal`, `:infeasible` or `:unbounded`.
- `solution`: an optimal vertex, when optimal.
- `value`: the optimal value, when optimal.
- `duals`: one number per row. When optimal they solve the dual problem:
  `duals' M <= c` column by column and `duals . r = value`. When infeasible
  they prove it: `duals' M <= 0` and `duals . r > 0`, which no `y >= 0` with
  `M y = r` could survive.
- `pivots`: the number of pivot steps taken.
"""
struct SimplexResult
    status::Symbol
    solution::Vector{Exact}
    value::Exact
    duals::Vector{Exact}
    pivots::Int
end

mutable struct Tableau
    entries::Matrix{Exact}      # rows by columns, the artificial columns last
    rhs::Vector{Exact}
    basis::Vector{Int}          # the basic column of each row
    reduced::Vector{Exact}      # reduced cost of each column
    pivots::Int
end

function price!(t::Tableau, cost::Vector{Exact})
    rows, columns = size(t.entries)
    for j in 1:columns
        t.reduced[j] = cost[j] - sum(cost[t.basis[i]] * t.entries[i, j] for i in 1:rows; init = Exact(0))
    end
end

function pivot!(t::Tableau, row::Int, column::Int)
    rows, columns = size(t.entries)
    scale = t.entries[row, column]
    for j in 1:columns
        t.entries[row, j] /= scale
    end
    t.rhs[row] /= scale
    for i in 1:rows
        factor = t.entries[i, column]
        (i == row || iszero(factor)) && continue
        for j in 1:columns
            t.entries[i, j] -= factor * t.entries[row, j]
        end
        t.rhs[i] -= factor * t.rhs[row]
    end
    factor = t.reduced[column]
    if !iszero(factor)
        for j in 1:columns
            t.reduced[j] -= factor * t.entries[row, j]
        end
    end
    t.basis[row] = column
    t.pivots += 1
end

"""
Pivots until no column among the first `allowed` has a negative reduced cost
(`:optimal`) or one of them can decrease the objective for ever
(`:unbounded`). Bland's rule, the smallest eligible index both entering and
leaving, rules out cycling, so the loop ends.
"""
function improve!(t::Tableau, allowed::Int, max_pivots::Int)
    rows = size(t.entries, 1)
    while true
        entering = findfirst(j -> t.reduced[j] < 0, 1:allowed)
        entering === nothing && return :optimal
        leaving, best = 0, Exact(0)
        for i in 1:rows
            t.entries[i, entering] > 0 || continue
            ratio = t.rhs[i] / t.entries[i, entering]
            if leaving == 0 || ratio < best || (ratio == best && t.basis[i] < t.basis[leaving])
                leaving, best = i, ratio
            end
        end
        leaving == 0 && return :unbounded
        t.pivots < max_pivots || throw(ErrorException("the simplex method exceeded $(max_pivots) pivots"))
        pivot!(t, leaving, entering)
    end
end

"""
    simplex(M, r, c; max_pivots = 100_000)

Solves `minimize c . y subject to M y = r, y >= 0` exactly by the two-phase
simplex method with Bland's rule and returns a [`SimplexResult`](@ref).

Phase one adds an artificial variable to every row and minimizes their sum;
a positive minimum means there is no feasible point, and the dual values of
that phase are the proof. Phase two minimizes `c` from the vertex phase one
found.
"""
function simplex(M::AbstractMatrix, r::AbstractVector, c::AbstractVector; max_pivots::Int = 100_000)
    rows, columns = size(M)
    (length(r) == rows && length(c) == columns) || throw(DimensionMismatch("M, r and c do not fit together"))
    sign = [r[i] < 0 ? -1 : 1 for i in 1:rows]
    entries = zeros(Exact, rows, columns + rows)
    for i in 1:rows
        for j in 1:columns
            entries[i, j] = sign[i] * M[i, j]
        end
        entries[i, columns + i] = 1
    end
    t = Tableau(entries, Exact[sign[i] * r[i] for i in 1:rows], collect((columns + 1):(columns + rows)),
                zeros(Exact, columns + rows), 0)
    artificial_cost = vcat(zeros(Exact, columns), ones(Exact, rows))
    price!(t, artificial_cost)
    improve!(t, columns, max_pivots)
    infeasibility = sum((t.rhs[i] for i in 1:rows if t.basis[i] > columns); init = Exact(0))
    if infeasibility > 0
        duals = Exact[sign[i] * (1 - t.reduced[columns + i]) for i in 1:rows]
        return SimplexResult(:infeasible, Exact[], Exact(0), duals, t.pivots)
    end
    # Artificial variables left in the basis sit at zero. Exchange each for a
    # real column where the row allows; a row that allows none is redundant
    # and no later pivot touches it.
    for i in 1:rows
        t.basis[i] > columns || continue
        j = findfirst(j -> !iszero(t.entries[i, j]), 1:columns)
        j === nothing || pivot!(t, i, j)
    end
    price!(t, vcat(Exact.(c), zeros(Exact, rows)))
    status = improve!(t, columns, max_pivots)
    status === :unbounded && return SimplexResult(:unbounded, Exact[], Exact(0), Exact[], t.pivots)
    solution = zeros(Exact, columns)
    for i in 1:rows
        t.basis[i] <= columns && (solution[t.basis[i]] = t.rhs[i])
    end
    duals = Exact[-sign[i] * t.reduced[columns + i] for i in 1:rows]
    SimplexResult(:optimal, solution, sum(c[j] * solution[j] for j in 1:columns; init = Exact(0)), duals, t.pivots)
end

"""
    feasibility(rows, width)

Decides whether a system of rows over `width` rational variables has a
solution. Returns `(:feasible, point)` or `(:infeasible, multipliers)`,
where the multipliers satisfy [`refutes`](@ref).

Free variables are split into differences of nonnegative ones and each
inequality gets a slack. Strict rows share one extra variable `e`, added to
their left sides and capped at 1, and `e` is maximized: the strict system
has a solution exactly when the maximum is positive. Otherwise the optimal
dual values are the multipliers; when even the weak system has no solution,
the dual values of phase one are.
"""
function feasibility(rows::AbstractVector{Row}, width::Int)
    count_rows = length(rows)
    strict = any(r.op === :lt for r in rows)
    slack_of = cumsum([r.op === :eq ? 0 : 1 for r in rows])
    slack_start = 2 * width + (strict ? 1 : 0)
    columns = slack_start + (count_rows == 0 ? 0 : last(slack_of)) + (strict ? 1 : 0)
    M = zeros(Exact, count_rows + (strict ? 1 : 0), columns)
    r = zeros(Exact, size(M, 1))
    c = zeros(Exact, columns)
    for (i, row) in enumerate(rows)
        for j in 1:width
            M[i, j] = row.coefficients[j]
            M[i, width + j] = -row.coefficients[j]
        end
        row.op === :eq || (M[i, slack_start + slack_of[i]] = 1)
        row.op === :lt && (M[i, 2 * width + 1] = 1)
        r[i] = row.bound
    end
    if strict
        M[end, 2 * width + 1] = 1
        M[end, columns] = 1
        r[end] = 1
        c[2 * width + 1] = -1
    end
    result = simplex(M, r, c)
    if result.status === :infeasible || (result.status === :optimal && strict && iszero(result.value))
        return (:infeasible, Exact[-result.duals[i] for i in 1:count_rows])
    end
    result.status === :optimal || throw(ErrorException("a bounded problem was reported unbounded"))
    (:feasible, Exact[result.solution[j] - result.solution[width + j] for j in 1:width])
end
