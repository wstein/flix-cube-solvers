///
/// The 4x4's third phase, as far as the centres go.
///
/// After the second phase the U/D and F/B colours are on their own faces but
/// not necessarily the right one of the pair, and the R and L centres are in
/// one of twelve arrangements. This finishes all of that -- with a move set
/// small enough that it cannot disturb what the earlier phases settled.
///
/// The edges are paired in the same phase, against their own table. This is
/// only the centre half of it.
///
mod CubeSolvers.Revenge.Reduce {

    use CubeSolvers.{Kind, Move}
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Centers.{slots, binomials, neighbours, rank}
    use CubeSolvers.Revenge.Separate.{finishing, sideGoals, side}

    ///
    /// How many arrangements each pair of faces has.
    ///
    pub def pairCount(): Int32 = 70

    ///
    /// How many arrangements the R and L centres can arrive in.
    ///
    pub def sideCount(): Int32 = 12

    ///
    /// How many states this phase's centres have.
    ///
    /// Times two, for the parity bit. This phase may turn F, B, U and D a
    /// quarter, and a quarter turn flips the agreement between the edge and
    /// corner permutations that the second phase established. Carrying the
    /// bit here is what makes the phase put it back.
    ///
    pub def count(): Int32 = pairCount() * pairCount() * sideCount() * 2

    ///
    /// The twenty moves this phase may use.
    ///
    pub def moves(): Vector[Move] = finishing()

    ///
    /// The eight centre slots on U and D.
    ///
    pub def upSlots(): Vector[Int32] = Vector#{0, 1, 2, 3, 12, 13, 14, 15}

    ///
    /// The eight centre slots on F and B.
    ///
    pub def frontSlots(): Vector[Int32] = Vector#{8, 9, 10, 11, 20, 21, 22, 23}

    ///
    /// The eight centre slots on R and L.
    ///
    pub def sideSlots(): Vector[Int32] = Vector#{4, 5, 6, 7, 16, 17, 18, 19}

    ///
    /// Which of the U and D slots hold the U colour.
    ///
    pub def up(cube: Facelets): Int32 = held(cube, upSlots(), CubeSolvers.Face.U)

    ///
    /// Which of the F and B slots hold the F colour.
    ///
    pub def front(cube: Facelets): Int32 = held(cube, frontSlots(), CubeSolvers.Face.F)

    ///
    /// Which of a group's slots hold a given colour, as one number.
    ///
    def held(cube: Facelets, group: Vector[Int32], colour: CubeSolvers.Face): Int32 =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let all = slots();
        rank(group |> Vector.mapWithIndex(i -> slot ->
            if (Vector.get(Vector.get(slot, all), stickers) == colour) i else -1)
            |> Vector.filter(i -> i >= 0))

    ///
    /// Which of the twelve arrangements the R and L centres are in.
    ///
    /// The second phase guarantees one of them, so anything else is a cube
    /// that did not come through it.
    ///
    pub def sideIndex(cube: Facelets): Int32 =
        match Vector.indexOf(side(cube), sideGoals()) {
            case Some(i) => i
            case None    => -1
        }

    ///
    /// All three, as one number.
    ///
    pub def coordinate(cube: Facelets): Int32 =
        ((up(cube) * pairCount() + front(cube)) * sideCount() + sideIndex(cube)) * 2
            + CubeSolvers.Revenge.Separate.mismatch(cube)

    ///
    /// The number a solved cube's centres have.
    ///
    pub def solved(): Int32 =
        let home = rank(Vector#{0, 1, 2, 3});
        let atSolved = match Vector.indexOf(CubeSolvers.Revenge.Separate.sideSolved(), sideGoals()) {
            case Some(i) => i
            case None    => 0
        };
        ((home * pairCount() + home) * sideCount() + atSolved) * 2

    ///
    /// Where each slot of a group goes, for each of this phase's moves.
    ///
    def group(chosen: Vector[Int32]): Vector[Vector[Int32]] = groupWith(chosen, moves())

    ///
    /// Where each slot of a group goes, for each of the given moves.
    ///
    def groupWith(chosen: Vector[Int32], ms: Vector[Move]): Vector[Vector[Int32]] =
        let all = slots();
        let ordinalOfSlot = all |> Vector.mapWithIndex(i -> slot -> (slot, i)) |> Vector.toList |> List.toMap;
        let ordinalIn = chosen |> Vector.mapWithIndex(i -> slot -> (slot, i)) |> Vector.toList |> List.toMap;
        ms |> Vector.map(m ->
            match CubeSolvers.Turning.permutation(Kind.Cube4x4, m) {
                case Err(_)   => Vector.range(0, Vector.length(chosen))
                case Ok(perm) => chosen |> Vector.map(slot ->
                    let landed = Map.getWithDefault(Vector.get(Vector.get(slot, all), perm), 0, ordinalOfSlot);
                    Map.getWithDefault(landed, 0, ordinalIn))
            })

    ///
    /// Where each of the twelve R and L arrangements goes, for each move.
    ///
    /// They are closed under these moves by construction: the twelve are
    /// exactly the arrangements these moves reach from solved.
    ///
    def sideMoves(): Vector[Vector[Int32]] = sideMovesWith(moves())

    ///
    /// Where each of the twelve R and L arrangements goes, for given moves.
    ///
    def sideMovesWith(ms: Vector[Move]): Vector[Vector[Int32]] =
        let binoms = binomials();
        let steps = groupWith(sideSlots(), ms);
        let goalsOf = sideGoals();
        Vector.range(0, Vector.length(steps)) |> Vector.map(m ->
            goalsOf |> Vector.map(g ->
                let to = Vector.get(m, neighbours(binoms, steps, 8, 4, g));
                match Vector.indexOf(to, goalsOf) {
                    case Some(i) => i
                    case None    => -1
                }))

    ///
    /// The centre transitions, for a given move order.
    ///
    /// The table's distances do not depend on the order its moves were listed
    /// in, but a transition looked up by move number does -- so the search
    /// builds these against its own list.
    ///
    pub def stepsFor(ms: Vector[Move]): {
        up = Vector[Vector[Int32]],
        front = Vector[Vector[Int32]],
        side = Vector[Vector[Int32]],
        flips = Vector[Int32]
    } =
        let solvedCube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        { up = groupWith(upSlots(), ms),
          front = groupWith(frontSlots(), ms),
          side = sideMovesWith(ms),
          // Against the same move list, or the parity bit would be read from
          // the wrong move.
          flips = ms |> Vector.map(m ->
            match CubeSolvers.Turning.apply(solvedCube, m) {
                case Err(_)    => 0
                case Ok(after) => CubeSolvers.Revenge.Separate.mismatch(after)
            }) }

    ///
    /// Where a move takes a centre coordinate.
    ///
    pub def after(
        steps: {
            up = Vector[Vector[Int32]],
            front = Vector[Vector[Int32]],
            side = Vector[Vector[Int32]],
            flips = Vector[Int32]
        },
        binoms: Vector[Vector[Int32]],
        at: Int32,
        m: Int32
    ): Int32 =
        let upAt = at / (pairCount() * sideCount() * 2);
        let frontAt = (at / (sideCount() * 2)) `Int32.modulo` pairCount();
        let sideAt = (at / 2) `Int32.modulo` sideCount();
        let parityAt = at `Int32.modulo` 2;
        let upTo = Vector.get(m, neighbours(binoms, steps#up, 8, 4, upAt));
        let frontTo = Vector.get(m, neighbours(binoms, steps#front, 8, 4, frontAt));
        let sideTo = Vector.get(sideAt, Vector.get(m, steps#side));
        let flipped = Vector.get(m, steps#flips);
        ((upTo * pairCount() + frontTo) * sideCount() + sideTo) * 2
            + ((parityAt + flipped) `Int32.modulo` 2)

    ///
    /// The exact number of moves each arrangement is from solved.
    ///
    pub def table(): Vector[Int32] =
        let binoms = binomials();
        let upSteps = group(upSlots());
        let frontSteps = group(frontSlots());
        let sideSteps = sideMoves();
        let flipped = flips();
        CubeSolvers.Distances.of(count(), solved(), at ->
            let upAt = at / (pairCount() * sideCount() * 2);
            let frontAt = (at / (sideCount() * 2)) `Int32.modulo` pairCount();
            let sideAt = (at / 2) `Int32.modulo` sideCount();
            let parityAt = at `Int32.modulo` 2;
            let upTo = neighbours(binoms, upSteps, 8, 4, upAt);
            let frontTo = neighbours(binoms, frontSteps, 8, 4, frontAt);
            Vector.range(0, Vector.length(upTo)) |> Vector.map(m ->
                ((Vector.get(m, upTo) * pairCount() + Vector.get(m, frontTo)) * sideCount()
                    + Vector.get(sideAt, Vector.get(m, sideSteps))) * 2
                    + ((parityAt + Vector.get(m, flipped)) `Int32.modulo` 2)))

    ///
    /// Whether each of this phase's moves flips the parity mismatch.
    ///
    pub def flips(): Vector[Int32] =
        let solvedCube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        moves() |> Vector.map(m ->
            match CubeSolvers.Turning.apply(solvedCube, m) {
                case Err(_)    => 0
                case Ok(after) => CubeSolvers.Revenge.Separate.mismatch(after)
            })

}
