///
/// The 4x4 centres, as one number.
///
/// A 4x4 is solved by reduction: make it behave like a 3x3, then solve that.
/// The first thing in the way is the centres, which on a 4x4 are four loose
/// stickers per face rather than one fixed piece.
///
/// This stage asks only where the U and D colours are. Eight of the
/// twenty-four centre slots hold them, and which eight is a number between 0
/// and 735,470 -- small enough to measure the exact distance to every one of
/// them.
///
mod CubeSolvers.Revenge.Centers {

    use CubeSolvers.{Face, Kind, Move, Turn}
    use CubeSolvers.Solver.{Facelets, Invalid}

    ///
    /// How many ways eight of the twenty-four centre slots can be chosen.
    ///
    pub def count(): Int32 = 735471

    ///
    /// The sticker slots that are centres, four per face in U R F D L B order.
    ///
    /// On a 4x4 the centres are the inner two-by-two of each face: rows one
    /// and two, columns one and two.
    ///
    pub def slots(): Vector[Int32] =
        Vector.range(0, 6) |> Vector.flatMap(f ->
            Vector#{5, 6, 9, 10} |> Vector.map(o -> f * 16 + o))

    ///
    /// The moves this stage searches with: every turn of every face, one
    /// layer deep and two.
    ///
    /// Both depths are needed, and the reason is worth stating. A wide turn
    /// is the outer layer and the slice beneath it, moving together. With
    /// only wide turns the two can never be separated -- the slice alone is
    /// `Rw` then `R` undone -- and the centres reach just 2,187 of their
    /// 735,471 arrangements. Adding the outer turns frees the slice and all
    /// of them become reachable.
    ///
    pub def moves(): Vector[Move] =
        Vector#{1, 2} |> Vector.flatMap(depth ->
            Vector#{Face.U, Face.R, Face.F, Face.D, Face.L, Face.B} |> Vector.flatMap(f ->
                Vector#{Turn.Cw, Turn.Half, Turn.Ccw} |> Vector.map(t -> CubeSolvers.move(f, depth, t))))

    ///
    /// Where each centre slot goes, for each move: slot ordinal to slot
    /// ordinal.
    ///
    pub def slotMoves(): Vector[Vector[Int32]] =
        let ordinalOf = slots() |> Vector.mapWithIndex(i -> slot -> (slot, i)) |> Vector.toList |> List.toMap;
        moves() |> Vector.map(m ->
            match CubeSolvers.Turning.permutation(Kind.Cube4x4, m) {
                case Err(_)   => Vector.range(0, 24)
                case Ok(perm) => slots() |> Vector.map(slot ->
                    Map.getWithDefault(Vector.get(slot, perm), 0, ordinalOf))
            })

    ///
    /// Which centre slots hold a U or D colour, as one number.
    ///
    pub def separation(cube: Facelets): Int32 =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        rank(slots() |> Vector.mapWithIndex(i -> slot ->
            match Vector.get(slot, stickers) {
                case Face.U => i
                case Face.D => i
                case _      => -1
            }) |> Vector.filter(i -> i >= 0))

    ///
    /// The number the solved separation has: the eight slots of U and D.
    ///
    pub def solved(): Int32 = rank(Vector#{0, 1, 2, 3, 12, 13, 14, 15})

    ///
    /// The place of an eight-slot subset among all of them.
    ///
    pub def rank(subset: Vector[Int32]): Int32 =
        subset |> Vector.sort |> Vector.mapWithIndex(i -> p -> binomial(p, i + 1)) |> Vector.sum

    ///
    /// The subset at the given place.
    ///
    pub def unrank(r: Int32): Vector[Int32] =
        let (_, picked) = List.foldLeft(state -> k -> take(state, k), (r, Nil), List.reverse(List.range(1, 9)));
        List.toVector(picked)

    ///
    /// Takes the largest slot whose weight still fits.
    ///
    def take(state: (Int32, List[Int32]), k: Int32): (Int32, List[Int32]) =
        let (left, found) = state;
        match List.maximum(List.filter(q -> binomial(q, k) <= left, List.range(0, 24))) {
            case Some(q) => (left - binomial(q, k), q :: found)
            case None    => (left, found)
        }

    ///
    /// How many ways to choose `k` of `n`.
    ///
    def binomial(n: Int32, k: Int32): Int32 =
        if (k < 0 or k > n)
            0
        else
            List.range(0, k) |> List.foldLeft(acc -> i -> acc * (n - i) / (i + 1), 1)

    ///
    /// The moves that leave a finished separation finished.
    ///
    /// Outer turns keep every centre on its own face. `Uw` and `Dw` move
    /// centres only among the four side faces, which after this stage hold no
    /// U or D colour at all. Everything else -- `Rw`, `Fw`, `Lw`, `Bw` --
    /// carries a U or D centre off its face and undoes the stage.
    ///
    pub def keeping(): Vector[Move] =
        moves() |> Vector.filter(m ->
            CubeSolvers.depth(m) == 1
                or CubeSolvers.face(m) == Face.U
                or CubeSolvers.face(m) == Face.D)

    ///
    /// The sixteen centre slots that are not on U or D.
    ///
    pub def sideSlots(): Vector[Int32] =
        Vector.range(0, 24) |> Vector.filter(i -> not (i < 4 or (i >= 12 and i < 16)))

    ///
    /// Which of those sixteen hold an F or B colour, as one number.
    ///
    /// The second stage of the centres, once the first has put every U and D
    /// colour on U or D. Eight of sixteen: 12,870 arrangements.
    ///
    pub def axis(cube: Facelets): Int32 =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let all = slots();
        rankOf(16, 8, sideSlots() |> Vector.mapWithIndex(i -> slot ->
            match Vector.get(Vector.get(slot, all), stickers) {
                case Face.F => i
                case Face.B => i
                case _      => -1
            }) |> Vector.filter(i -> i >= 0))

    ///
    /// How many ways eight of the sixteen side slots can be chosen.
    ///
    pub def axisCount(): Int32 = 12870

    ///
    /// The number the finished second stage has: F and B hold their own.
    ///
    pub def axisSolved(): Int32 = rankOf(16, 8, Vector#{4, 5, 6, 7, 12, 13, 14, 15})

    ///
    /// Where each side slot goes, for each move that keeps the first stage.
    ///
    pub def axisMoves(): Vector[Vector[Int32]] =
        let ordinalOf = sideSlots() |> Vector.mapWithIndex(i -> slot -> (slot, i)) |> Vector.toList |> List.toMap;
        let all = slots();
        let slotOrdinal = all |> Vector.mapWithIndex(i -> slot -> (slot, i)) |> Vector.toList |> List.toMap;
        keeping() |> Vector.map(m ->
            match CubeSolvers.Turning.permutation(Kind.Cube4x4, m) {
                case Err(_)   => Vector.range(0, 16)
                case Ok(perm) => sideSlots() |> Vector.map(slot ->
                    let sticker = Vector.get(slot, all);
                    let landed = Map.getWithDefault(Vector.get(sticker, perm), 0, slotOrdinal);
                    Map.getWithDefault(landed, 0, ordinalOf))
            })

    ///
    /// The exact number of moves each second-stage arrangement is from done.
    ///
    pub def axisTable(): Vector[Int32] = distances(axisCount(), axisSolved(), axisMoves(), 16, 8)

    ///
    /// The moves that leave both separations finished.
    ///
    /// Outer turns keep every centre on its face. A wide *half* turn swaps
    /// opposite faces in pairs -- `Rw2` trades U with D and F with B -- which
    /// moves centres without ever taking one off its axis. A wide quarter
    /// turn would.
    ///
    pub def pairing(): Vector[Move] =
        moves() |> Vector.filter(m ->
            CubeSolvers.depth(m) == 1 or CubeSolvers.turn(m) == Turn.Half)

    ///
    /// The three pairs of faces, as centre slot ordinals: U with D, R with L,
    /// F with B.
    ///
    pub def pairs(): Vector[Vector[Int32]] = Vector#{
        Vector#{0, 1, 2, 3, 12, 13, 14, 15},
        Vector#{4, 5, 6, 7, 16, 17, 18, 19},
        Vector#{8, 9, 10, 11, 20, 21, 22, 23}
    }

    ///
    /// How many ways the three pairs can be arranged: seventy each.
    ///
    pub def colourCount(): Int32 = 343000

    ///
    /// Which half of each pair holds the first face's colour, as one number.
    ///
    pub def colours(cube: Facelets): Int32 =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let all = slots();
        let firsts = Vector#{Face.U, Face.R, Face.F};
        Vector.range(0, 3) |> Vector.foldLeft(acc -> p ->
            let group = Vector.get(p, pairs());
            let wanted = Vector.get(p, firsts);
            let held = group |> Vector.mapWithIndex(i -> slot ->
                if (Vector.get(Vector.get(slot, all), stickers) == wanted) i else -1)
                |> Vector.filter(i -> i >= 0);
            acc * 70 + rankOf(8, 4, held), 0)

    ///
    /// The number a finished cube has.
    ///
    pub def coloursSolved(): Int32 =
        let one = rankOf(8, 4, Vector#{0, 1, 2, 3});
        (one * 70 + one) * 70 + one

    ///
    /// Where each slot of each pair goes, for each move that keeps both
    /// separations.
    ///
    pub def colourMoves(): Vector[Vector[Vector[Int32]]] =
        let all = slots();
        let slotOrdinal = all |> Vector.mapWithIndex(i -> slot -> (slot, i)) |> Vector.toList |> List.toMap;
        pairing() |> Vector.map(m ->
            match CubeSolvers.Turning.permutation(Kind.Cube4x4, m) {
                case Err(_)   => pairs() |> Vector.map(g -> Vector.range(0, Vector.length(g)))
                case Ok(perm) => pairs() |> Vector.map(group ->
                    let ordinalIn = group |> Vector.mapWithIndex(i -> slot -> (slot, i)) |> Vector.toList |> List.toMap;
                    group |> Vector.map(slot ->
                        let landed = Map.getWithDefault(Vector.get(Vector.get(slot, all), perm), 0, slotOrdinal);
                        Map.getWithDefault(landed, 0, ordinalIn)))
            })

    ///
    /// The exact number of moves each colour arrangement is from done.
    ///
    pub def colourTable(): Vector[Int32] =
        region rc {
            let dist = Array.repeat(rc, colourCount(), -1);
            Array.put(0, coloursSolved(), dist);
            let steps = colourMoves();
            colourWalk(dist, coloursSolved() :: Nil, steps, 0);
            Array.toVector(dist)
        }

    ///
    /// Marks everything one move past `frontier`, then recurses.
    ///
    def colourWalk(
        dist: Array[Int32, r],
        frontier: List[Int32],
        steps: Vector[Vector[Vector[Int32]]],
        d: Int32
    ): Unit \ r =
        if (List.isEmpty(frontier))
            ()
        else
            let next = List.foldLeft(acc -> at -> colourReach(dist, acc, at, steps, d), Nil, frontier);
            colourWalk(dist, next, steps, d + 1)

    ///
    /// Every unmarked neighbour of one colour arrangement.
    ///
    def colourReach(
        dist: Array[Int32, r],
        acc: List[Int32],
        at: Int32,
        steps: Vector[Vector[Vector[Int32]]],
        d: Int32
    ): List[Int32] \ r =
        let parts = Vector#{at / 4900, (at / 70) `Int32.modulo` 70, at `Int32.modulo` 70}
            |> Vector.map(r -> unrankOf(8, 4, r));
        Vector.foldLeft(seen -> step -> {
            let moved = Vector.range(0, 3) |> Vector.map(p ->
                rankOf(8, 4, Vector.map(slot -> Vector.get(slot, Vector.get(p, step)), Vector.get(p, parts))));
            let to = (Vector.get(0, moved) * 70 + Vector.get(1, moved)) * 70 + Vector.get(2, moved);
            if (Array.get(to, dist) < 0) {
                Array.put(d + 1, to, dist);
                to :: seen
            } else
                seen
        }, acc, steps)

    ///
    /// The three tables, built once.
    ///
    pub type alias Tables = {
        separating = Vector[Int32],
        axing = Vector[Int32],
        colouring = Vector[Int32]
    }

    ///
    /// Builds all three. Tens of seconds, once.
    ///
    pub def tables(): Tables =
        { separating = table(), axing = axisTable(), colouring = colourTable() }

    ///
    /// The moves that solve a 4x4's centres, or why the cube is not one.
    ///
    /// Three stages, each walked downhill: the tables hold exact distances,
    /// so from a state `d` moves out there is always a move to one `d - 1`
    /// out, and no search is needed at all.
    ///
    pub def solution(t: Tables, cube: Facelets): Result[Invalid, CubeSolvers.MoveSequence] =
        forM (one <- descend(cube, t#separating, separation, moves());
              afterOne <- CubeSolvers.Turning.replay(cube, one);
              two <- descend(afterOne, t#axing, axis, keeping());
              afterTwo <- CubeSolvers.Turning.replay(afterOne, two);
              three <- descend(afterTwo, t#colouring, colours, pairing()))
            yield CubeSolvers.simplify(one ::: two ::: three)

    ///
    /// Walks one stage downhill to its goal.
    ///
    def descend(
        cube: Facelets,
        dist: Vector[Int32],
        at: Facelets -> Int32,
        allowed: Vector[Move]
    ): Result[Invalid, CubeSolvers.MoveSequence] =
        let here = Vector.get(at(cube), dist);
        if (here == 0)
            Ok(Nil)
        else
            match downhill(cube, dist, at, allowed, here) {
                case None            => Err(Invalid.Unsolvable("no move brings the centres closer; the table and the cube disagree"))
                case Some((m, next)) => descend(next, dist, at, allowed) |> Result.map(rest -> m :: rest)
            }

    ///
    /// A move that brings this stage one step closer.
    ///
    def downhill(
        cube: Facelets,
        dist: Vector[Int32],
        at: Facelets -> Int32,
        allowed: Vector[Move],
        here: Int32
    ): Option[(Move, Facelets)] =
        allowed |> Vector.findMap(m ->
            match CubeSolvers.Turning.apply(cube, m) {
                case Err(_)   => None
                case Ok(next) => if (Vector.get(at(next), dist) == here - 1) Some((m, next)) else None
            })

    ///
    /// The place of a `k`-subset of `n` among all of them.
    ///
    def rankOf(_n: Int32, _k: Int32, subset: Vector[Int32]): Int32 =
        subset |> Vector.sort |> Vector.mapWithIndex(i -> p -> binomial(p, i + 1)) |> Vector.sum

    ///
    /// The `k`-subset of `n` at the given place.
    ///
    def unrankOf(n: Int32, k: Int32, r: Int32): Vector[Int32] =
        let (_, picked) = List.foldLeft(state -> j -> takeOf(n, state, j), (r, Nil), List.reverse(List.range(1, k + 1)));
        List.toVector(picked)

    ///
    /// Takes the largest slot whose weight still fits, for an `n`-slot world.
    ///
    def takeOf(n: Int32, state: (Int32, List[Int32]), k: Int32): (Int32, List[Int32]) =
        let (left, found) = state;
        match List.maximum(List.filter(q -> binomial(q, k) <= left, List.range(0, n))) {
            case Some(q) => (left - binomial(q, k), q :: found)
            case None    => (left, found)
        }

    ///
    /// Breadth-first distances over any subset coordinate.
    ///
    def distances(size: Int32, goal: Int32, steps: Vector[Vector[Int32]], n: Int32, k: Int32): Vector[Int32] =
        region rc {
            let dist = Array.repeat(rc, size, -1);
            Array.put(0, goal, dist);
            walk(dist, goal :: Nil, steps, n, k, 0);
            Array.toVector(dist)
        }

    ///
    /// Marks everything one move past `frontier`, then recurses.
    ///
    def walk(
        dist: Array[Int32, r],
        frontier: List[Int32],
        steps: Vector[Vector[Int32]],
        n: Int32,
        k: Int32,
        d: Int32
    ): Unit \ r =
        if (List.isEmpty(frontier))
            ()
        else
            let next = List.foldLeft(acc -> at -> reach(dist, acc, at, steps, n, k, d), Nil, frontier);
            walk(dist, next, steps, n, k, d + 1)

    ///
    /// Every unmarked neighbour of one arrangement.
    ///
    def reach(
        dist: Array[Int32, r],
        acc: List[Int32],
        at: Int32,
        steps: Vector[Vector[Int32]],
        n: Int32,
        k: Int32,
        d: Int32
    ): List[Int32] \ r =
        let subset = unrankOf(n, k, at);
        Vector.foldLeft(seen -> step -> {
            let to = rankOf(n, k, Vector.map(slot -> Vector.get(slot, step), subset));
            if (Array.get(to, dist) < 0) {
                Array.put(d + 1, to, dist);
                to :: seen
            } else
                seen
        }, acc, steps)

    ///
    /// The exact number of moves each separation is from solved.
    ///
    /// Breadth first from the solved state over the wide turns, marking as it
    /// goes. Seconds, once.
    ///
    pub def table(): Vector[Int32] =
        region rc {
            let dist = Array.repeat(rc, count(), -1);
            Array.put(0, solved(), dist);
            let steps = slotMoves();
            sweep(dist, solved() :: Nil, steps, 0);
            Array.toVector(dist)
        }

    ///
    /// Marks everything one move past `frontier`, then recurses.
    ///
    def sweep(dist: Array[Int32, r], frontier: List[Int32], steps: Vector[Vector[Int32]], d: Int32): Unit \ r =
        if (List.isEmpty(frontier))
            ()
        else
            let next = List.foldLeft(acc -> at -> expand(dist, acc, at, steps, d), Nil, frontier);
            sweep(dist, next, steps, d + 1)

    ///
    /// Every unmarked neighbour of one separation, marked and collected.
    ///
    def expand(
        dist: Array[Int32, r],
        acc: List[Int32],
        at: Int32,
        steps: Vector[Vector[Int32]],
        d: Int32
    ): List[Int32] \ r =
        let subset = unrank(at);
        Vector.foldLeft(seen -> step -> {
            let to = rank(Vector.map(slot -> Vector.get(slot, step), subset));
            if (Array.get(to, dist) < 0) {
                Array.put(d + 1, to, dist);
                to :: seen
            } else
                seen
        }, acc, steps)

}
