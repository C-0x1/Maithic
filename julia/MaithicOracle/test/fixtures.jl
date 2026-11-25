# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The oracle fixtures: goals, the outcomes this package computes for them, a
# certificate of every discrete kind, and two Lean files that the kernel
# checks: Checks.lean restates the claims for a decision procedure of Lean,
# and Replay.lean proves them from the certificates with the verified checker
# of the Lean library. The files live in fixtures/oracle and are shared with
# the other languages: the schemas are validated against them and
# `maithic check` runs the Lean files. The test suite fails when a file
# differs from what the code computes; `make julia-fixtures` rewrites them.

using MaithicCore
using MaithicOracle

const FIXTURE_ROOT = normpath(joinpath(@__DIR__, "..", "..", "..", "fixtures", "oracle"))

"The directories under the root that this file writes; others belong to other writers."
const WRITTEN_DIRECTORIES = ("goals", "outcomes", "certificates", "lean")

goal_fixture(kind, hypotheses, goal; domain = kind == :real ? :real : :rat) = Goal(kind, hypotheses, goal; domain)

const GOAL_FIXTURES = [
    "linear-budget" => goal_fixture(:linear, ["2*x + 3*y <= 6", "x >= 0", "y >= 0"], "x + y <= 3"),
    "linear-contradiction" => goal_fixture(:linear, ["x + y <= 1", "x >= 1", "y >= 1/2"], "false"),
    "linear-equation" => goal_fixture(:linear, ["x + y = 10", "x - y = 2"], "x = 6"),
    "linear-integer-parity" => goal_fixture(:linear, ["2*x + 4*y < 7", "x + 2*y > 2"], "x + 2*y = 3"; domain = :int),
    "linear-integer-gap" => goal_fixture(:linear, ["2*x + 4*y < 7", "x + 2*y >= 4"], "false"; domain = :int),
    "linear-integer-bound" => goal_fixture(:linear, ["3*x >= 4", "y >= x"], "y >= 2"; domain = :int),
    "linear-integer-cycle" => goal_fixture(:linear, ["x - y < 3", "y - z < -1", "z - x <= -1"], "false"; domain = :int),
    "linear-integer-system" =>
        goal_fixture(:linear, ["7*a + 12*b + 31*c = 17", "3*a + 5*b + 14*c = 7", "1 <= a", "a <= 40", "-50 <= b", "b <= 50"],
                     "c >= -1000"; domain = :int),
    "linear-integer-distinct" => goal_fixture(:linear, ["2*x + 1 = 2*y"], "x != y"; domain = :int),
    "linear-refuted" => goal_fixture(:linear, ["2*x + 3*y <= 6", "x >= 0", "y >= 0"], "x + y <= 2"),
    "linear-unknown" => goal_fixture(:linear, ["x + y = 1", "x - y = 0"], "false"; domain = :int),
    "polynomial-identity" => goal_fixture(:polynomial, String[], "(a^2 + b^2) * (c^2 + d^2) = (a*c - b*d)^2 + (a*d + b*c)^2"),
    "polynomial-symmetric" => goal_fixture(:polynomial, ["x + y = 3", "x * y = 2"], "x^2 + y^2 = 5"),
    "polynomial-integer" => goal_fixture(:polynomial, ["2*x = 3*y + 1", "y = 1"], "x = 2"; domain = :int),
    "polynomial-refuted" => goal_fixture(:polynomial, String[], "(x + y)^2 = x^2 + y^2"),
    "polynomial-unknown" => goal_fixture(:polynomial, ["x^2 = 0"], "x = 0"),
    "real-parabola" => goal_fixture(:real, ["0 <= x", "x <= 1"], "x * (1 - x) <= 1/4 + 1/100"),
    "real-elementary" => goal_fixture(:real, ["1/4 <= x", "x <= 4"], "sqrt(x) + 1 / x > 1.8"),
    "real-disc" => goal_fixture(:real, ["-1 <= x", "x <= 1", "-1 <= y", "y <= 1", "x^2 + y^2 <= 1/2"], "x + y <= 1.2"),
    "real-refuted" => goal_fixture(:real, ["0 <= x", "x <= 1"], "x * (1 - x) <= 1/5"),
    "real-unknown" => goal_fixture(:real, ["0 <= x", "x <= 2"], "x^2 - 2*x + 1 >= 0"),
]

discrete_fixtures() = [
    "discrete-prime" => prove_prime(97),
    "discrete-prime-mersenne" => prove_prime(big(2)^61 - 1),
    "discrete-factorization" => factorization(360),
    "discrete-factorization-fermat" => factorization(big(2)^64 + 1),
    "discrete-inverse" => modular_inverse(3, 7),
    "discrete-order" => multiplicative_order(10, 1_000_003),
    "discrete-logarithm" => discrete_logarithm(5, powermod(5, 123456, 1_000_003), 1_000_003),
    "discrete-square-root" => modular_square_root(5, big(2)^61 - 1),
    "discrete-bezout" => bezout(big(2)^200 - 1, -(big(2)^120 - 1)),
    "discrete-witness" => find_witness("x^3 + y^3 = 1729 ∧ 0 < x ∧ x < 9 ∧ x < y"),
    "discrete-witness-pell" => find_witness("x^2 = 2*y^2 + 1 ∧ y > 0"; domain = :nat),
]

"Every fixture file as `relative path => content`, computed from scratch."
function oracle_fixtures()
    files = Pair{String,String}[]
    lean = String["-- Statements that restate oracle certificates for the Lean kernel.\n",
                  "-- Written by `make julia-fixtures` from julia/MaithicOracle; do not edit.\n\n",
                  "set_option linter.unusedVariables false\n"]
    replay = String["-- Oracle certificates replayed through the verified checkers of the Lean library.\n",
                    "-- Written by `make julia-fixtures` from julia/MaithicOracle; do not edit.\n",
                    LEAN_REPLAY_IMPORT, "\n\n",
                    "set_option linter.unusedVariables false\n"]
    function restate(name, certificate)
        text = lean_check(certificate; name = replace(name, "-" => "_"))
        text === nothing || push!(lean, "\n-- $(claim(certificate))\n", text)
        text = lean_replay(certificate; name = "replay_" * replace(name, "-" => "_"))
        text === nothing || push!(replay, "\n-- $(claim(certificate))\n", text)
    end
    for (name, goal) in GOAL_FIXTURES
        outcome = solve(goal; (goal.kind == :real ? (max_boxes = 2000,) : (;))...)
        push!(files, "goals/$(name).json" => write_json(goal_document(goal); pretty = true) * "\n")
        push!(files, "outcomes/$(name).json" => write_json(outcome_document(outcome); pretty = true) * "\n")
        outcome isa Proved && restate(name, outcome.certificate)
    end
    for (name, certificate) in discrete_fixtures()
        push!(files, "certificates/$(name).json" => write_json(certificate_document(certificate); pretty = true) * "\n")
        restate(name, certificate)
    end
    push!(files, "lean/Checks.lean" => join(lean))
    push!(files, "lean/Replay.lean" => join(replay))
    files
end

if abspath(PROGRAM_FILE) == @__FILE__
    for (path, content) in oracle_fixtures()
        target = joinpath(FIXTURE_ROOT, path)
        mkpath(dirname(target))
        write(target, content)
    end
    println("wrote ", length(oracle_fixtures()), " files under ", FIXTURE_ROOT)
end
