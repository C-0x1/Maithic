# MaithicNNV

Neural-network verification, the numeric side. The package computes bounds on the outputs of ReLU networks
quickly, in floating point with outward rounding, and writes them down as certificates with exact rational
numbers. A certificate is not trusted because this package produced it: it is checked in Lean by
`Maithic.NN.checkCertificate`, a checker that is proved sound against the exact semantics of the network.

<p align="center">
  <img src="../../assets/readme/nnv-intervals.svg" alt="The network that computes the absolute value, as relu of x plus relu of minus x, on inputs from minus 2 to 3. After the first layer, relu of x lies between 0 and 3 and relu of minus x between 0 and 2. Their sum is bounded by 0 and 5, although the network never outputs more than 3, because the two are never large together. Below, on a number line from 0 to 6: the true outputs cover 0 to 3, the proved box covers 0 to 5. The claim 0 to 6 is verified, because the proved box lies inside it. The claim 0 to 2 is refuted, because the input 3 gives 3. The claim 0 to 4 is true and stays unknown, because 5 is more than 4." width="900">
</p>

## What it provides

- `Network`, `Layer`, `Neuron`, `Interval`, `Box`: networks with exact rational weights, and boxes of inputs.
- `evaluate`: the exact semantics, the same as `Maithic.NN.Network.eval` in Lean.
- `propagate`, `propagate_exact`, `enclose`: interval bounds layer by layer, in floating point with outward rounding or exactly.
- `certify`, `check_certificate`, `certificate_document`: a box for every layer, with the check that Lean repeats.
- `decide_property`: one of three answers for a property, each with what the kernel needs to judge it: verified with a certificate, violated with an input, or unknown with nothing.

## An example

Run from the root of the repository:

```julia
using MaithicNNV

# The absolute value as a network: relu(x) + relu(-x).
net = Network(Layer([1 // 1; -1 // 1;;], [0 // 1, 0 // 1], ReLU), Layer([1 // 1 1 // 1], [0 // 1], Identity))
println("at 3: ", evaluate(net, [3 // 1])[1], "   at -2: ", evaluate(net, [-2 // 1])[1])

input = [Interval(-2, 3)]
exact_bound = propagate_exact(net, input)[end][1]
println("proved bound: [", exact_bound.lo, ", ", exact_bound.hi, "]")

for claimed in ([Interval(0, 6)], [Interval(0, 2)], [Interval(0, 4)])
    verdict, evidence = decide_property(net, input, claimed)
    println("[", claimed[1].lo, ", ", claimed[1].hi, "]  ", verdict, "  ",
            evidence isa Certificate ? "a certificate that checks: $(check_certificate(evidence))" :
            evidence === nothing ? "nothing to hand over" : "the input $(evidence[1])")
end
```

It prints:

```text
at 3: 3//1   at -2: 2//1
proved bound: [0//1, 5//1]
[0, 6]  Verified  a certificate that checks: true
[0, 2]  Violated  the input 3//1
[0, 4]  Unknown  nothing to hand over
```

## Where Maithic uses it

`maithic nnv verify` runs `bin/verify.jl` and writes the claim as a Lean theorem that the kernel accepts or
rejects. The example above is the network of [`examples/nnv/absolute-value.json`](../../examples/nnv/absolute-value.json),
and its three answers are theorems of the Lean library in
[`Maithic.NN.Examples.AbsoluteValue`](../../lean/Maithic/NN/Examples/AbsoluteValue.lean), with a fourth: the
claim [0, 4] is true, and out of the reach of interval bounds.
[`docs/guides/verifying-networks.md`](../../docs/guides/verifying-networks.md) says what the method shows and what
it does not.

## Tests

```sh
julia --startup-file=no --project=julia/MaithicNNV -e 'using Pkg; Pkg.test()'
```

`make test-julia` runs the tests of all eight packages.

## Dependencies

The standard library of Julia 1.11 or later (built and tested with 1.13), and `MaithicCore`. Nothing is downloaded: the packages use each other by
path, and the Makefile runs the package manager offline.
