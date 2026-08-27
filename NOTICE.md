# Notice

`flix-cube-solvers`, Copyright 2026 Werner Stein, and licensed
**GPL-3.0-or-later**. See [LICENSE](LICENSE).

## Where the code comes from

| Part | Origin | Taken under |
| --- | --- | --- |
| `CubeSolvers`, `CubeSolvers.Solver`, `CubeSolvers.Turning`, `CubeSolvers.Distances` | original work | GPL-3.0-or-later |
| `CubeSolvers.Pocket` | original work, from the published cubie model | GPL-3.0-or-later |
| `CubeSolvers.Rubik` | original work, from the published description of the two-phase algorithm | GPL-3.0-or-later |
| `CubeSolvers.Revenge` | [TPR-4x4x4-Solver](https://github.com/cs0x7f/TPR-4x4x4-Solver), `cs.threephase`, Copyright 2023 Chen Shuang | GPL-3.0-or-later |

TPR-4x4x4-Solver is offered as `MIT OR GPL-3.0-or-later`, so its MIT option was
available and this project did not take it. The 4x4 coordinates are involved
enough that reading the reference implementation is the honest way to get them
right, and GPL is the licence that asks the least interpretation of what
counts as derivation.

That choice sets the licence for the whole package: a Flix package is one
artifact and a consumer links all of it.

## What is not here

The 3x3 engine is not min2phase. It is written from the published description
of Kociemba's two-phase algorithm, and shares no code with any implementation
of it.
