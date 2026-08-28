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
    pub def count(): Int32 = pairCount() * pairCount() * sideCount()

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
        (up(cube) * pairCount() + front(cube)) * sideCount() + sideIndex(cube)

    ///
    /// The number a solved cube's centres have.
    ///
    pub def solved(): Int32 =
        let home = rank(Vector#{0, 1, 2, 3});
        let atSolved = match Vector.indexOf(CubeSolvers.Revenge.Separate.sideSolved(), sideGoals()) {
            case Some(i) => i
            case None    => 0
        };
        (home * pairCount() + home) * sideCount() + atSolved

    ///
    /// Where each slot of a group goes, for each of this phase's moves.
    ///
    def group(chosen: Vector[Int32]): Vector[Vector[Int32]] =
        let all = slots();
        let ordinalOfSlot = all |> Vector.mapWithIndex(i -> slot -> (slot, i)) |> Vector.toList |> List.toMap;
        let ordinalIn = chosen |> Vector.mapWithIndex(i -> slot -> (slot, i)) |> Vector.toList |> List.toMap;
        moves() |> Vector.map(m ->
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
    def sideMoves(): Vector[Vector[Int32]] =
        let binoms = binomials();
        let steps = group(sideSlots());
        let goalsOf = sideGoals();
        Vector.range(0, Vector.length(steps)) |> Vector.map(m ->
            goalsOf |> Vector.map(g ->
                let to = Vector.get(m, neighbours(binoms, steps, 8, 4, g));
                match Vector.indexOf(to, goalsOf) {
                    case Some(i) => i
                    case None    => -1
                }))

    ///
    /// The exact number of moves each arrangement is from solved.
    ///
    pub def table(): Vector[Int32] =
        let binoms = binomials();
        let upSteps = group(upSlots());
        let frontSteps = group(frontSlots());
        let sideSteps = sideMoves();
        CubeSolvers.Distances.of(count(), solved(), at ->
            let upAt = at / (pairCount() * sideCount());
            let frontAt = (at / sideCount()) `Int32.modulo` pairCount();
            let sideAt = at `Int32.modulo` sideCount();
            let upTo = neighbours(binoms, upSteps, 8, 4, upAt);
            let frontTo = neighbours(binoms, frontSteps, 8, 4, frontAt);
            Vector.range(0, Vector.length(upTo)) |> Vector.map(m ->
                (Vector.get(m, upTo) * pairCount() + Vector.get(m, frontTo)) * sideCount()
                    + Vector.get(sideAt, Vector.get(m, sideSteps))))

}
