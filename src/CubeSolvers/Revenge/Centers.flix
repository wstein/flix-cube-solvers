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
    use CubeSolvers.Solver.Facelets

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
