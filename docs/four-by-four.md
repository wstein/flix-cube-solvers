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

## Joining the two numberings

The ported edge tables number the twelve places their own way, and this
package numbers them geometrically. Getting a cube into the tables means
knowing the correspondence, and it took two steps and one false start.

The false start is worth recording. Comparing the two numberings move by
move reported fourteen of twenty agreeing, which looked like near
confirmation. It was worthless: fourteen of the phase's moves are outer
turns, which move both wings of a place together and therefore leave the
pairing permutation alone. They agree under *any* numbering. Only the six
wide turns carry information, and none of them agreed.

The correspondence itself is two maps composed:

1. Match each place to the reference's by the colours it shows when solved.
   Its `EdgeColor` names the twelve places that way, and its faces are
   numbered as this package numbers them.
2. Follow `FullEdgeMap`. `Edge3.set` reads its cube through that map, so its
   places are not the colour-pair places. Missing this step is what left four
   of twenty moves disagreeing while everything else looked right.

`Pairs.placeCorrespondence` derives it and `Pairs.asEdgeState` applies it.
All twenty moves agree, which is asserted rather than assumed.

## The search, and where it stands

`CubeSolvers.Revenge.Pairing` holds the third phase's search: both tables,
the twenty moves in the order the edge tables number them, the walk down the
two-bit table to recover a true distance, and an iterative-deepening search
bounded by the larger of the two.

It works. The fault it had is worth keeping written down, because it was the
subtlest of the three false positives in this work.

The search carried the edge index along, updating it move by move. That is
faster and wrong. A move sends the pairing permutation `std` to
`b . std . a` -- a conjugation -- so a carried index tracks the pairing in a
frame that the moves are themselves turning, while the cube's pairing is
absolute. The two agree exactly when the cube is paired, and disagree in
between:

    r2 r2        tracked 0          recomputed 0          paired
    r2 U r2      tracked 22,484,305 recomputed 22,544,790 not paired
    L2 r2 L2 r2  tracked 0          recomputed 0          paired

So the goal test fired on cubes that were not paired, and the search returned
confident wrong answers: solutions that left 7 or 9 of the 12 places unpaired
while reporting success. Reading the index from the cube at every node costs
more and is right.

On cubes scrambled within the phase, it now finishes the centres and pairs
every edge: three cubes scrambled 3, 5 and 7 moves deep came back in 2, 3 and
6 moves, each with twelve places paired and the centres home.

Building the tables costs about eighty seconds, so the search is not in the
test suite -- the same decision as the table itself.

## The driver

`CubeSolvers.Revenge.Engine` chains all of it: phase one, then phase two's
answers enumerated and sifted for wing separation, then phase three, then the
reduced cube handed to `CubeSolvers.Rubik` -- and `Revenge.Reduced.asRubik`
reads a reduced 4x4 as the 3x3 it has become. That last step is verified: a
4x4 scrambled with outer turns only was read as a 3x3, solved in seven moves,
and those moves solved the 4x4.

It gathers before it searches. Every first-phase answer is taken through the
second phase, every second-phase answer that separates the wings is kept along
with what the third phase must at least cost from there, and only then is
anything searched, cheapest first. This is the reference's shape: it sorts its
hundred second-phase results by `length1 + length2 + max(edgePrun, ctPrun)`
before trying any of them. The reason is in the measurements below -- a
third-phase search whose budget is too small still walks the whole tree before
saying so, so it is worth two table lookups to not start one.

## Sifting, and the trap in it

Roughly one state in seven hundred separates the wings: the property picks one
wing from each of twelve places, so `2^12 / C(24, 12)` is about `1/660`.
Measured on states this phase actually produces, the rate is **1 in 459**.

That rate is only available to *independent* tries, and enumerated answers are
not independent. Answers come out in the order the moves are tried, so they
share long prefixes -- and what happens to the wings along a shared prefix is
shared with it. Read that way:

- 5,792 answers, off 60 different first-phase answers: **none** separated.
- 29,352 answers, off the same 60, with the move order turned and two moves of
  slack allowed: **64** separated.

Same phase, same predicate, same cube. The first number cost most of a day to
explain, because every part of it looks like a bug: the predicate was checked
against the reference's `checkEdge`, the numbering against the cube, and the
coordinate against the cube move by move -- all correct. The enumeration was
the problem. `Separate.manyExactlyFrom` and `Walk.manyWith` exist for this.

## The split as a bound, which did not pay

`CubeSolvers.Revenge.Orbits` makes the split a coordinate rather than a sift:
which twelve of the twenty-four slots hold a first-orbit wing, `C(24, 12)` =
2,704,156 states, complete in 32 seconds, deepest 8. Every transition is
checked against the cube, and reaching its goal implies the separation the
third phase needs.

It is not what the driver uses. Searching the second phase under both bounds
at once -- the larger of the settling distance and the split distance --
found nothing at depths six through eight in 477 seconds. Each bound alone
says six; jointly the answer is much deeper, and the maximum of two weak
independent bounds is still weak. The module is kept: it is the version that
guarantees rather than sifts, and it is what proved the predicate was sound.

## Where the third phase stands

It works, and its cost is set by how tight the bound is. On cubes scrambled a
known number of third-phase moves, searching to a limit of thirteen:

| Bound | Answer | |
| --- | --- | --- |
| 2 | 6 | fast |
| 7 | 8 | fast |
| 7 | 9 | fast |
| 9 | 13 | slow, finishes |
| 6 | -- | did not finish |

The last row is the whole problem. A loose bound means the search exhausts
every depth from the bound up to the true answer, and each of those is a full
tree over twenty moves. This is why the driver scores candidates first and
spends its budget on the tight ones.

What remains is node throughput. The transition is now twelve lookups and a
ranking, with no allocation; the reference does the same work in optimised
Java at a rate this does not approach. Whether that gap closes by a better
bound or by cheaper nodes is the open question.

## What is next

1. Make the third phase finish on real cubes inside a sensible budget. Either
   a tighter bound or cheaper nodes; the two tables are built and correct.
2. Tune towards 44.5 moves once it finishes at all: more first-phase answers,
   more second-phase slack, more third-phase attempts. TPR uses 10,000, 500
   and 100 to reach 44.39.
3. 4x4 scrambling, which needs the solver: a random state is scrambled by
   solving it and reversing.
