# The 4x4, as TPR does it

Tsai's method, by way of
[TPR-4x4x4-Solver](https://github.com/cs0x7f/TPR-4x4x4-Solver). TPR reports
44.39 moves on average over 2000 random cubes, 40 to 47, with about 20MB of
tables. This is what it takes to get there, written down so the work can be
picked up where it stopped.

## The chain

Three phases reduce the cube to one a 3x3 solver can finish. Tsai's steps 3
and 4 are merged into one, and where TPR hands off to min2phase we hand off to
`CubeSolvers.Rubik`.

| Phase | Objective | Moves | Coordinate | Size |
| --- | --- | --- | --- | --- |
| 1 | R and L centres onto R and L | 36 | which 8 of 24 centre slots hold an R/L colour | 735,471 raw, 15,582 by symmetry |
| 2 | U/D and F/B centres home; R/L centres into one of 12 finishable positions; edge parities matched | 28 | `ct * 70 + rl` | 6435 × 35 × 2 = 450,450 |
| 3 | centres solved, edges paired | 20 | centres 35 × 35 × 12 × 2, edges 1538 × 20,160 | 29,400 and 31,006,080 |

The move sets, in this package's terms — a wide turn here is Tsai's outer turn
and slice together, and the two generate the same group:

| Phase | Tsai | ours |
| --- | --- | --- |
| 1 | ⟨R,L,F,B,U,D,r,l,f,b,u,d⟩ | all 18 outer, all 18 wide |
| 2 | ⟨R,L,F,B,U,D,r,l,f2,b2,u2,d2⟩ | 18 outer, Rw/Lw any turn, Fw2 Bw2 Uw2 Dw2 |
| 3 | ⟨R2,L2,F,B,U,D,r2,l2,f2,b2,u2,d2⟩ | R2 L2, F/B/U/D any turn, all six wide half turns |

## The coordinates

**Phase 1** is a subset: which eight of the twenty-four centre slots hold one
colour pair. `CubeSolvers.Revenge.Centers` already computes it, for U/D rather
than R/L, and the two are the same coordinate under a relabelling. TPR folds
the 48 cube symmetries in and stores 15,582 classes instead of 735,471 states;
we store the raw table, which is 3MB and needs no symmetry machinery.

**Phase 2** is two coordinates and a parity bit, multiplied:

- `rl`, 0 to 69: rank which four of the first seven R/L slots differ from the
  eighth, then double it and add the edge permutation parity. This is where
  Tsai's "make the low-edge and high-edge permutation parities match" lives —
  it rides along in the same number.
- `ct`, 0 to 6434: the sixteen non-R/L slots as U/D versus F/B. Half of
  C(16,8) = 12,870, the halving being a canonical choice between a state and
  its complement.

Index is `ct * 70 + rl`. Six states count as solved rather than one — 0, 18,
28, 46, 54 and 56 — because the R/L centres may finish in any of several
arrangements that phase 3 can still fix.

**Phase 3** splits into centres and edges. Centres are 35 × 35 × 12 × 2: the
U/D and F/B halves as before, the twelve R/L positions phase 2 was allowed to
leave behind, and parity. Edges are 1538 symmetry classes against 20,160 raw
states, stored two bits per entry — 31M entries in under 8MB, and the reason
TPR's tables are 20MB rather than 200.

## Telling a wing from its mirror

The edge coordinates are about which wing is where, and two wings of one edge
show the same two colours. TPR tells them apart with a table of ordered
sticker pairs: the U-F edge appears once as `{U, F}` and once as `{F, U}`, and
a wing is identified by matching the colours it shows against that order.

The same thing falls out of the geometry, so no table is needed. Two faces
meet along a direction — the cross product of their outward normals — and the
two wings sit at opposite ends of it. Take the faces in the order that
direction points, and the pair is ordered. Turning the cube turns the
direction with it, so the rule survives every move.

`CubeSolvers.Revenge.Wings.identity` does that, and `parity` counts the
inversions of the result, which is the bit Tsai's second step has to leave
matching the corners'.

## What is done

Phases 1, 2 and the centre half of phase 3 are built, complete, and solve the
centres of real cubes. `CubeSolvers.Revenge.Wings` models the pieces, knows
when a place holds a matching pair, identifies which wing is where, and gives
the permutation's parity.

`CubeSolvers.Distances.bytesInto` writes a table as bytes in a region, which
is what the 31M-state edge table needs: as a `Vector` it would be a couple of
hundred megabytes, as bytes about a tenth of that, and it never has to be
copied out.

## What the edge coordinate turned out to be

Not a placement and a permutation, as the table sizes suggested from outside,
but one permutation of twelve. Read each place's two wings, ask which place
each came from, and compose one answer with the other's inverse. The identity
means every place holds a matching pair, which is what reduction is. Where a
pair *belongs* never enters into it — a pair in the wrong place is still a
pair, and the 3x3 will move it.

That reading only works because of a fact about the third phase rather than
about the cube. Under all 36 moves a wing can reach all 24 places, so there is
no way to speak of "the two wings of a place" as an ordered thing. Under the
third phase's twenty moves the wings split into two orbits of twelve, one wing
of every place in each — and *that* is what lets twenty-four wings be read as
a permutation of twelve. It is computed here, not tabulated.

Two consequences, both found the hard way:

- **The second phase does not guarantee the split.** A cube leaving it may
  have both wings of some place in the same orbit, and then the coordinate is
  not even a permutation. TPR checks this after the fact, in `EdgeCube`, and
  discards the phase-2 answers that fail — which is why it generates up to
  five hundred of them. Tsai lists the same thing as an objective of his
  second step. Either way it is a condition on the *answer*, not on the
  coordinate.
- **So the driver cannot walk one phase-2 solution and continue.** It has to
  enumerate them, keep those that separate the wings, and try each. The
  downhill walk that serves phases 1 and 2 is not enough on its own.

## What is next

1. Enumerate phase-2 solutions rather than walking one, and keep those that
   separate the wings. The 3x3's `manyExactly` is the shape to copy.
2. Phase 3's edge tables. Measured, and the cheap routes are closed:

   **Projections do not work.** A move acts on the pairing permutation as
   `R -> b . R . a'`, where `a` and `b` are its actions on the two wing
   orbits. That relabels the domain *and* the codomain, so "where do these
   four places go" is not closed under the moves and cannot be a coordinate.
   Tracking `L` or `H` alone is closed -- the positions holding a fixed set of
   pieces transform by `a` -- but neither says anything about `R = H' . L`,
   which is the only thing the phase is trying to fix.

   **A table-free heuristic is far too weak.** The most a single phase-3 move
   was seen to improve the pairing, over 1,200 tries on 60 states, is four
   places. So `ceil(unpaired / 4)` is admissible and gives at most 3 for a
   fully unpaired cube, against phase-3 solutions of ten to fifteen moves.
   With twenty moves branching, that prunes nothing.

   **The exact table does not fit.** 12!/2 is 239,500,800 states: 239MB as
   bytes is affordable, but the two frontier arrays are Int32 and come to
   1.9GB, and the build is 4.8 billion transitions -- half an hour at the
   rate measured here.

   So this needs what TPR uses: eight symmetries folding 239.5M to 31M.

   **Built.** `CubeSolvers.Revenge.Edges` carries the state, its twenty
   moves, the three rotations, the ranking, the symmetry fold and the table.
   The fold gives 1,538 classes, which is the reference's figure. The table
   fills all 31,006,080 entries, deepest 13, in **79 seconds** -- against
   TPR's 6 to 7 in optimised Java, which is a fair ratio for a first pass.

   Two bits an entry, holding the depth modulo three: a neighbour is nearer,
   level, or further, and only one of those is one less than the present
   depth, so the true distance is recovered by walking down. Sweeping the
   whole table per depth rather than keeping a frontier, because a frontier
   of thirty-one million indices would cost more than the table it fills.

   It is a build step, not a test. The suite checks the first five rings --
   1,496 entries, under a second -- and the packing.
3. The search that uses them: the centre table and the edge tables together,
   as the 3x3 already does with its two.
4. The driver: phase-1 solutions feed phase 2, phase 2 feeds phase 3, and the
   result goes to `CubeSolvers.Rubik`. TPR tries 10,000 phase-1 solutions, 500
   phase-2 attempts and 100 phase-3 attempts to reach 44.39.
