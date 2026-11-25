# MaithicEmbed

Embeddings and vector search for mathematical statements. The package turns declarations and queries into unit
vectors and finds the nearest ones. The embedder that ships here needs no model and no download: it hashes the
character and token n-grams of a text into a fixed number of dimensions, and it gives the same vector for the
same text on every machine.

## What it provides

- `Embedder`, `HashedNgramEmbedder`, `embed`, `dimension`, `tokenize`: the interface of an embedder and the one that needs no model, with separate modes for a query and for a document.
- `FlatIndex`, `add!`, `search`, `Hit`, `cosine`: an exact index, and the nearest vectors to a query.
- `vector_json`, `parse_vector_json`, `vector_bytes`, `parse_vector_bytes`: vectors as text and as bytes, read back exactly.

## An example

Run from the root of the repository:

```julia
using MaithicEmbed

embedder = HashedNgramEmbedder(dimension = 256)
index = FlatIndex(256)
for (name, statement) in ["Nat.add_comm" => "theorem Nat.add_comm (a b : Nat) : a + b = b + a",
                          "Nat.mul_comm" => "theorem Nat.mul_comm (a b : Nat) : a * b = b * a",
                          "List.reverse_reverse" => "theorem List.reverse_reverse (l : List α) : l.reverse.reverse = l"]
    add!(index, name, embed(embedder, statement, Document))
end

for hit in search(index, embed(embedder, "add comm", Query); k = 2)
    println(rpad(hit.id, 16), round(hit.score; digits = 3))
end
```

It prints:

```text
Nat.add_comm    0.258
Nat.mul_comm    0.16
```

## Where Maithic uses it

It is a library for use from Julia. The command `maithic search` ranks declarations by their words and names,
and does not call this package; the hashed embedder is the baseline that any other embedder would have to beat
on the query set under [`fixtures/search`](../../fixtures/search).
[`docs/guides/finding-declarations.md`](../../docs/guides/finding-declarations.md) explains the ranking and the
evaluation.

## Tests

```sh
julia --startup-file=no --project=julia/MaithicEmbed -e 'using Pkg; Pkg.test()'
```

`make test-julia` runs the tests of all eight packages.

## Dependencies

The standard library of Julia 1.11 or later (built and tested with 1.13), and `MaithicCore`. Nothing is downloaded: the packages use each other by
path, and the Makefile runs the package manager offline.
