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

## What is done

`CubeSolvers.Revenge.Centers` has phase 1 and two further centre stages of its
own devising, which solve centres completely in 15 to 23 moves. Those two
stages are not Tsai's and will be replaced: solving centres before touching
edges is exactly what costs the twenty moves between 44 and the mid sixties.

`CubeSolvers.Revenge.Wings` models the pieces and knows when a place holds a
matching pair.

## What is next

1. Renumber phase 1 to R/L, so the chain matches without translation.
2. Phase 2: the `rl` and `ct` coordinates, their move tables, the six goals.
3. Phase 3 centres: 29,400 states, 20 moves.
4. Phase 3 edges: 31M states at two bits. `CubeSolvers.Distances` returns
   `Vector[Int32]`, which would be 124MB here, so it needs a byte-wide or
   packed variant first.
5. The driver: phase 1 solutions feed phase 2, phase 2 feeds phase 3, and the
   result goes to `CubeSolvers.Rubik`. TPR tries 10,000 phase-1 solutions, 500
   phase-2 attempts and 100 phase-3 attempts to reach its average.
