///
/// The 4x4's second phase: the other four centres, and where the R and L ones
/// are allowed to stop.
///
/// The first phase put the R and L colours somewhere on the R and L faces.
/// This one puts U and D onto U and D, F and B onto F and B, and leaves the R
/// and L centres in any arrangement the third phase can still repair -- twelve
/// of the seventy, and finding out which twelve is done here rather than
/// written down.
///
/// Following Tsai's second step, as implemented in TPR-4x4x4-Solver. The move
/// set is his: every outer turn, any turn of the R and L slices, and half
/// turns only of the others. Slices there are wide turns here, which generate
/// the same group.
///
mod CubeSolvers.Revenge.Separate {

    use CubeSolvers.{Face, Kind, Move, Turn}
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Centers.{slots, binomials, neighbours}

    ///
    /// How many arrangements of the sixteen centres that are not R or L.
    ///
    pub def outerCount(): Int32 = 12870

    ///
    /// How many arrangements of the eight R and L centres.
    ///
    pub def sideCount(): Int32 = 70

    ///
    /// How many states this phase distinguishes.
    ///
    pub def count(): Int32 = outerCount() * sideCount()

    ///
    /// The twenty-eight moves this phase may use.
    ///
    /// Every outer turn keeps each centre on its face. `Rw` and `Lw` move
    /// centres only among U, F, D and B, which hold no R or L colour once the
    /// first phase is done. The half turns `Fw2`, `Bw2`, `Uw2` and `Dw2` swap
    /// R with L wholesale, which keeps those colours on those two faces. A
    /// wide *quarter* turn of F, B, U or D would not, and is left out.
    ///
    pub def moves(): Vector[Move] =
        let outer = Vector#{Face.U, Face.R, Face.F, Face.D, Face.L, Face.B} |> Vector.flatMap(f ->
            Vector#{Turn.Cw, Turn.Half, Turn.Ccw} |> Vector.map(t -> CubeSolvers.move(f, 1, t)));
        let sides = Vector#{Face.R, Face.L} |> Vector.flatMap(f ->
            Vector#{Turn.Cw, Turn.Half, Turn.Ccw} |> Vector.map(t -> CubeSolvers.move(f, 2, t)));
        let halves = Vector#{Face.U, Face.F, Face.D, Face.B} |> Vector.map(f ->
            CubeSolvers.move(f, 2, Turn.Half));
        Vector.append(outer, Vector.append(sides, halves))

    ///
    /// The sixteen centre slots that are not on R or L.
    ///
    pub def outerSlots(): Vector[Int32] =
        Vector.range(0, 24) |> Vector.filter(i -> not (isSide(i)))

    ///
    /// The eight centre slots on R and L.
    ///
    pub def sideSlots(): Vector[Int32] =
        Vector.range(0, 24) |> Vector.filter(isSide)

    ///
    /// Whether a centre slot is on R or L.
    ///
    def isSide(i: Int32): Bool = (i >= 4 and i < 8) or (i >= 16 and i < 20)

    ///
    /// Which of the sixteen outer slots hold a U or D colour.
    ///
    pub def outer(cube: Facelets): Int32 =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let all = slots();
        CubeSolvers.Revenge.Centers.rank(outerSlots() |> Vector.mapWithIndex(i -> slot ->
            match Vector.get(Vector.get(slot, all), stickers) {
                case Face.U => i
                case Face.D => i
                case _      => -1
            }) |> Vector.filter(i -> i >= 0))

    ///
    /// Which of the eight R and L slots hold an R colour.
    ///
    pub def side(cube: Facelets): Int32 =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let all = slots();
        CubeSolvers.Revenge.Centers.rank(sideSlots() |> Vector.mapWithIndex(i -> slot ->
            if (Vector.get(Vector.get(slot, all), stickers) == Face.R) i else -1)
            |> Vector.filter(i -> i >= 0))

    ///
    /// Both together, as one number.
    ///
    pub def coordinate(cube: Facelets): Int32 = outer(cube) * sideCount() + side(cube)

    ///
    /// Where each of the sixteen outer slots goes, for each move.
    ///
    pub def outerMoves(): Vector[Vector[Int32]] = group(outerSlots())

    ///
    /// Where each of the eight R and L slots goes, for each move.
    ///
    pub def sideMoves(): Vector[Vector[Int32]] = group(sideSlots())

    ///
    /// Where each slot of a group goes, for each move of this phase.
    ///
    /// Both groups are closed under these moves, which is what makes them
    /// coordinates at all: a slot of the group always lands on a slot of the
    /// group.
    ///
    def group(chosen: Vector[Int32]): Vector[Vector[Int32]] = groupWith(chosen, moves())

    ///
    /// The twenty moves the third phase may use.
    ///
    /// Needed here to find out where this phase is allowed to stop: an R and
    /// L arrangement is acceptable exactly when the third phase can still
    /// repair it.
    ///
    pub def finishing(): Vector[Move] =
        let sides = Vector#{Face.R, Face.L} |> Vector.map(f -> CubeSolvers.move(f, 1, Turn.Half));
        let others = Vector#{Face.U, Face.F, Face.D, Face.B} |> Vector.flatMap(f ->
            Vector#{Turn.Cw, Turn.Half, Turn.Ccw} |> Vector.map(t -> CubeSolvers.move(f, 1, t)));
        let wides = Vector#{Face.U, Face.R, Face.F, Face.D, Face.L, Face.B} |> Vector.map(f ->
            CubeSolvers.move(f, 2, Turn.Half));
        Vector.append(sides, Vector.append(others, wides))

    ///
    /// The arrangement of the outer centres this phase is aiming at: U and D
    /// colours on the U and D faces.
    ///
    pub def outerSolved(): Int32 =
        CubeSolvers.Revenge.Centers.rank(Vector#{0, 1, 2, 3, 8, 9, 10, 11})

    ///
    /// The arrangement the R and L centres have when solved.
    ///
    pub def sideSolved(): Int32 = CubeSolvers.Revenge.Centers.rank(Vector#{0, 1, 2, 3})

    ///
    /// The R and L arrangements this phase may stop at.
    ///
    /// Not a list to be looked up: an arrangement is acceptable exactly when
    /// the third phase's moves can still carry it home, so it is found by
    /// walking those moves from solved and seeing where they reach.
    ///
    pub def sideGoals(): Vector[Int32] =
        let binoms = binomials();
        let steps = groupWith(sideSlots(), finishing());
        let reach = CubeSolvers.Distances.of(sideCount(), sideSolved(), at ->
            neighbours(binoms, steps, 8, 4, at));
        Vector.range(0, sideCount()) |> Vector.filter(i -> Vector.get(i, reach) >= 0)

    ///
    /// Every state this phase counts as finished.
    ///
    pub def goals(): Vector[Int32] =
        sideGoals() |> Vector.map(s -> outerSolved() * sideCount() + s)

    ///
    /// The exact number of moves each state is from the nearest finish.
    ///
    pub def table(): Vector[Int32] =
        let binoms = binomials();
        let outerSteps = outerMoves();
        let sideSteps = sideMoves();
        CubeSolvers.Distances.ofMany(count(), goals(), at ->
            let outerAt = at / sideCount();
            let sideAt = at `Int32.modulo` sideCount();
            let outerTo = neighbours(binoms, outerSteps, 16, 8, outerAt);
            let sideTo = neighbours(binoms, sideSteps, 8, 4, sideAt);
            Vector.range(0, Vector.length(outerTo)) |> Vector.map(m ->
                Vector.get(m, outerTo) * sideCount() + Vector.get(m, sideTo)))

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

}
