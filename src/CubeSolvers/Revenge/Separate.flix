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
    /// The centres, times the R and L arrangements, times one bit: whether
    /// the edge permutation's parity matches the corners'.
    ///
    pub def count(): Int32 = outerCount() * sideCount() * 2

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
    /// Whether the edge and corner permutations disagree about parity.
    ///
    /// A reduced 4x4 can show a position no 3x3 ever does -- two edges
    /// swapped and nothing else wrong -- and this bit is what rules it out.
    /// Tsai's second step has to leave it at zero, so it is carried as part
    /// of the coordinate rather than checked afterwards.
    ///
    pub def mismatch(cube: Facelets): Int32 =
        (CubeSolvers.Revenge.Wings.parity(cube) + CubeSolvers.Revenge.Corners.parity(cube)) `Int32.modulo` 2

    ///
    /// All three together, as one number.
    ///
    pub def coordinate(cube: Facelets): Int32 =
        (outer(cube) * sideCount() + side(cube)) * 2 + mismatch(cube)

    ///
    /// Whether each move flips the parity mismatch.
    ///
    /// A move's own permutation is what a solved cube shows after it, so
    /// this asks each move what it does and remembers the answer.
    ///
    pub def flips(): Vector[Int32] =
        let solvedCube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        moves() |> Vector.map(m ->
            match CubeSolvers.Turning.apply(solvedCube, m) {
                case Err(_)    => 0
                case Ok(after) => mismatch(after)
            })

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
        sideGoals() |> Vector.map(s -> (outerSolved() * sideCount() + s) * 2)

    ///
    /// The move tables, built once and handed to whatever walks them.
    ///
    pub type alias Steps = {
        outer = Vector[Vector[Int32]],
        side = Vector[Vector[Int32]],
        flips = Vector[Int32],
        binoms = Vector[Vector[Int32]]
    }

    ///
    /// Builds them.
    ///
    pub def steps(): Steps =
        { outer = outerMoves(), side = sideMoves(), flips = flips(), binoms = binomials() }

    ///
    /// Where move `m` takes state `at`.
    ///
    pub def after(t: Steps, at: Int32, m: Int32): Int32 =
        let outerAt = at / (sideCount() * 2);
        let sideAt = (at / 2) `Int32.modulo` sideCount();
        let parityAt = at `Int32.modulo` 2;
        let outerTo = Vector.get(m, neighbours(t#binoms, t#outer, 16, 8, outerAt));
        let sideTo = Vector.get(m, neighbours(t#binoms, t#side, 8, 4, sideAt));
        (outerTo * sideCount() + sideTo) * 2 + ((parityAt + Vector.get(m, t#flips)) `Int32.modulo` 2)

    ///
    /// Up to `cap` different solutions of exactly `depth` moves, as move
    /// numbers.
    ///
    /// One solution is not enough here. The third phase needs the wings split
    /// between its two orbits, and the second phase does not promise that --
    /// it is a property of the answer, so the answers have to be sifted.
    ///
    pub def manyExactly(
        t: Steps,
        dist: Vector[Int32],
        at: Int32,
        depth: Int32,
        cap: Int32
    ): List[List[Int32]] =
        collect(t, dist, at, depth, -1, cap, 0)

    ///
    /// The same, starting the move order at `spin`.
    ///
    /// Answers come out in the order the moves are tried, so a plain search
    /// returns a family that shares long prefixes -- and what happens to the
    /// wings along the way is shared with it. Sifting such a family for a
    /// property one state in seven hundred has is not the same as sifting
    /// that many states: six thousand of them yielded none. Turning the move
    /// order gives a family that starts elsewhere.
    ///
    pub def manyExactlyFrom(
        t: Steps,
        dist: Vector[Int32],
        at: Int32,
        depth: Int32,
        cap: Int32,
        spin: Int32
    ): List[List[Int32]] =
        collect(t, dist, at, depth, -1, cap, spin)

    ///
    /// Every solution of exactly `depth` moves from here, up to `cap`.
    ///
    def collect(
        t: Steps,
        dist: Vector[Int32],
        at: Int32,
        depth: Int32,
        last: Int32,
        cap: Int32,
        spin: Int32
    ): List[List[Int32]] =
        let lower = Vector.get(at, dist);
        let width = Vector.length(moves());
        if (lower > depth or cap <= 0 or lower < 0)
            Nil
        else if (depth == 0)
            if (lower == 0) (Nil :: Nil) else Nil
        else
            List.range(0, width)
                |> List.map(i -> (i + spin) `Int32.modulo` width)
                |> List.filter(m -> allowed(m, last))
                |> List.foldLeft(acc -> m ->
                    let room = cap - List.length(acc);
                    if (room <= 0)
                        acc
                    else
                        let deeper = collect(t, dist, after(t, at, m), depth - 1, m, room, spin + 1);
                        acc ::: List.map(rest -> m :: rest, deeper),
                    Nil)

    ///
    /// Everything the wing split contributes to the search.
    ///
    pub type alias Splitting = {
        orbits = Vector[Int32],
        binoms = Vector[Vector[Int32]],
        steps = Vector[Vector[Int32]]
    }

    ///
    /// Every answer of exactly `depth` moves that also splits the wings.
    ///
    /// The split is a bound here rather than a test afterwards. Enumerating
    /// answers and keeping the ones that happen to split does not work: the
    /// property picks twelve slots of twenty-four, so it almost never holds
    /// by luck, and six thousand answers produced none.
    ///
    pub def manySplitting(
        t: Steps,
        dist: Vector[Int32],
        s: Splitting,
        at: Int32,
        orbitAt: Int32,
        depth: Int32,
        cap: Int32
    ): List[List[Int32]] =
        gather(t, dist, s, at, orbitAt, depth, -1, cap)

    def gather(
        t: Steps,
        dist: Vector[Int32],
        s: Splitting,
        at: Int32,
        orbitAt: Int32,
        depth: Int32,
        last: Int32,
        cap: Int32
    ): List[List[Int32]] =
        let settled = Vector.get(at, dist);
        let split = Vector.get(orbitAt, s#orbits);
        let lower = Int32.max(settled, split);
        if (lower > depth or cap <= 0 or settled < 0 or split < 0)
            Nil
        else if (depth == 0)
            if (lower == 0) (Nil :: Nil) else Nil
        else
            List.range(0, Vector.length(moves()))
                |> List.filter(m -> allowed(m, last))
                |> List.foldLeft(acc -> m ->
                    let room = cap - List.length(acc);
                    if (room <= 0)
                        acc
                    else
                        let deeper = gather(t, dist, s,
                            after(t, at, m),
                            CubeSolvers.Revenge.Orbits.after(s#binoms, s#steps, orbitAt, m),
                            depth - 1, m, room);
                        acc ::: List.map(rest -> m :: rest, deeper),
                    Nil)

    ///
    /// Whether move `m` may follow move `last`.
    ///
    /// Two turns of one face in a row are one turn or none. Opposite faces
    /// commute, so only one of the two orders is searched.
    ///
    def allowed(m: Int32, last: Int32): Bool =
        if (last < 0)
            true
        else
            let f = faceNumberOf(m);
            let g = faceNumberOf(last);
            if (f == g)
                Vector.get(m, depths()) != Vector.get(last, depths())
            else if (f == (g + 3) `Int32.modulo` 6)
                f > g
            else
                true

    ///
    /// Which face each move turns, as a number.
    ///
    def faceNumberOf(m: Int32): Int32 = match CubeSolvers.face(Vector.get(m, moves())) {
        case Face.U => 0
        case Face.R => 1
        case Face.F => 2
        case Face.D => 3
        case Face.L => 4
        case _      => 5
    }

    ///
    /// How deep each move cuts.
    ///
    def depths(): Vector[Int32] = moves() |> Vector.map(CubeSolvers.depth)

    ///
    /// The moves a list of move numbers stands for.
    ///
    pub def sequence(chosen: List[Int32]): CubeSolvers.MoveSequence =
        chosen |> List.map(m -> Vector.get(m, moves()))

    ///
    /// The exact number of moves each state is from the nearest finish.
    ///
    pub def table(): Vector[Int32] =
        let binoms = binomials();
        let outerSteps = outerMoves();
        let sideSteps = sideMoves();
        let flipped = flips();
        CubeSolvers.Distances.ofMany(count(), goals(), at ->
            let outerAt = at / (sideCount() * 2);
            let sideAt = (at / 2) `Int32.modulo` sideCount();
            let parityAt = at `Int32.modulo` 2;
            let outerTo = neighbours(binoms, outerSteps, 16, 8, outerAt);
            let sideTo = neighbours(binoms, sideSteps, 8, 4, sideAt);
            Vector.range(0, Vector.length(outerTo)) |> Vector.map(m ->
                (Vector.get(m, outerTo) * sideCount() + Vector.get(m, sideTo)) * 2
                    + ((parityAt + Vector.get(m, flipped)) `Int32.modulo` 2)))

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
