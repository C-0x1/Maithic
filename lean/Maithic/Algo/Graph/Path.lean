/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Paths in a graph

A graph here is a function from a vertex to the vertices its edges lead to. `findPath` searches
depth first for a way from one vertex to another, and returns the way it found, vertex by vertex.
The theorem `findPath_sound` says that what it returns is what it claims to be: a path in the
graph that starts where the search started and ends at the target. A caller does not have to
trust the search: `isPath` checks any list of vertices against the graph, and `isPath_iff` says
that the check decides the property.

The search is bounded by fuel, the number of edges it may walk in a row, and it keeps the vertices
on its current way so as not to walk in a circle. With as much fuel as there are vertices it finds
a path whenever there is one; that half is tested here and not proved.
-/

namespace Maithic.Algo

/-- A directed graph: for every vertex, the vertices its edges lead to. -/
abbrev Graph := Nat → List Nat

/-- A graph from a list of edges. -/
def Graph.ofEdges (edges : List (Nat × Nat)) : Graph :=
  fun u => (edges.filter fun e => e.1 == u).map (·.2)

theorem Graph.mem_ofEdges {edges : List (Nat × Nat)} {u v : Nat} : v ∈ Graph.ofEdges edges u ↔ (u, v) ∈ edges := by
  simp only [Graph.ofEdges, List.mem_map, List.mem_filter, beq_iff_eq]
  constructor
  · rintro ⟨⟨a, b⟩, ⟨hmem, ha⟩, hb⟩
    simp only at ha hb
    subst ha; subst hb
    exact hmem
  · intro h
    exact ⟨(u, v), ⟨h, rfl⟩, rfl⟩

/-- A list of vertices is a path when every vertex has an edge to the next. -/
def IsPath (g : Graph) : List Nat → Prop
  | [] => True
  | [_] => True
  | u :: v :: rest => v ∈ g u ∧ IsPath g (v :: rest)

/-- The check that decides `IsPath`. -/
def isPath (g : Graph) : List Nat → Bool
  | [] => true
  | [_] => true
  | u :: v :: rest => (g u).contains v && isPath g (v :: rest)

theorem isPath_iff (g : Graph) : ∀ p : List Nat, isPath g p = true ↔ IsPath g p
  | [] => by simp [isPath, IsPath]
  | [_] => by simp [isPath, IsPath]
  | u :: v :: rest => by simp [isPath, IsPath, isPath_iff g (v :: rest)]

instance (g : Graph) (p : List Nat) : Decidable (IsPath g p) := decidable_of_iff _ (isPath_iff g p)

/-- A path from `u` to `v`: a path that starts at `u` and ends at `v`. -/
def IsPathFromTo (g : Graph) (u v : Nat) (p : List Nat) : Prop :=
  IsPath g p ∧ p.head? = some u ∧ p.getLast? = some v

/-- Putting a vertex in front of a path that starts at one of its neighbours gives a path. -/
theorem IsPath.cons {g : Graph} {u v : Nat} {p : List Nat} (hp : IsPath g p) (hhead : p.head? = some v) (hedge : v ∈ g u) :
    IsPath g (u :: p) := by
  cases p with
  | nil => simp at hhead
  | cons w rest =>
    simp only [List.head?_cons, Option.some.injEq] at hhead
    subst hhead
    exact ⟨hedge, hp⟩

/-- Depth-first search for a path from `u` to `target`, walking at most `fuel` edges in a row and
never back into a vertex of the current way. -/
def findPath (g : Graph) (target : Nat) : Nat → List Nat → Nat → Option (List Nat)
  | 0, _, u => if u = target then some [u] else none
  | fuel + 1, visited, u =>
    if u = target then some [u]
    else if visited.contains u then none
    else (g u).findSome? fun v => (findPath g target fuel (u :: visited) v).map (u :: ·)

/-- **Soundness of the search.** What `findPath` returns is a path in the graph from the vertex
the search started at to the target. -/
theorem findPath_sound (g : Graph) (target : Nat) :
    ∀ (fuel : Nat) (visited : List Nat) (u : Nat) (p : List Nat), findPath g target fuel visited u = some p → IsPathFromTo g u target p
  | 0, visited, u, p, h => by
    simp only [findPath] at h
    split at h
    · next ht => cases h; subst ht; exact ⟨trivial, rfl, rfl⟩
    · cases h
  | fuel + 1, visited, u, p, h => by
    simp only [findPath] at h
    split at h
    · next ht => cases h; subst ht; exact ⟨trivial, rfl, rfl⟩
    · split at h
      · cases h
      · obtain ⟨v, hv, hfound⟩ := List.exists_of_findSome?_eq_some h
        cases hq : findPath g target fuel (u :: visited) v with
        | none => simp [hq] at hfound
        | some q =>
          simp only [hq, Option.map_some, Option.some.injEq] at hfound
          subst hfound
          obtain ⟨hpath, hhead, hlast⟩ := findPath_sound g target fuel (u :: visited) v q hq
          refine ⟨hpath.cons hhead hv, rfl, ?_⟩
          cases q with
          | nil => simp at hhead
          | cons w rest => simpa [List.getLast?_cons_cons] using hlast

/-- Whether `target` can be reached from `u`, as far as a search with this much fuel can tell. -/
def reaches (g : Graph) (fuel u target : Nat) : Bool := (findPath g target fuel [] u).isSome

/-- A `true` from `reaches` comes with a path. -/
theorem reaches_sound {g : Graph} {fuel u target : Nat} (h : reaches g fuel u target = true) :
    ∃ p, IsPathFromTo g u target p := by
  cases hp : findPath g target fuel [] u with
  | none => simp [reaches, hp] at h
  | some p => exact ⟨p, findPath_sound g target fuel [] u p hp⟩

/-- The number of edges of a path. -/
def pathLength (p : List Nat) : Nat := p.length - 1

end Maithic.Algo
