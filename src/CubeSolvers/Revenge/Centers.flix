///
/// The 4x4 centres, as one number.
///
/// A 4x4 is solved by reduction: make it behave like a 3x3, then solve that.
/// The first thing in the way is the centres, which on a 4x4 are four loose
/// stickers per face rather than one fixed piece.
///
/// This stage asks only where the R and L colours are. Eight of the
/// twenty-four centre slots hold them, and which eight is a number between 0
/// and 735,470 -- small enough to measure the exact distance to every one of
/// them.
///
/// R and L rather than U and D because that is where Tsai's chain starts, and
/// every phase after this one is defined against it.
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
    /// Which centre slots hold an R or L colour, as one number.
    ///
    pub def separation(cube: Facelets): Int32 =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        rank(slots() |> Vector.mapWithIndex(i -> slot ->
            match Vector.get(slot, stickers) {
                case Face.R => i
                case Face.L => i
                case _      => -1
            }) |> Vector.filter(i -> i >= 0))

    ///
    /// The number the solved separation has: the eight slots of R and L.
    ///
    pub def solved(): Int32 = rank(Vector#{4, 5, 6, 7, 16, 17, 18, 19})

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
    /// How many ways to choose `k` of `n`, for every `n` and `k` a centre
    /// coordinate can ask for.
    ///
    /// Computed once and handed to the ranking. Recomputing it inside `rank`
    /// was most of the cost of building a table: eight of these per ranking,
    /// thirty-six rankings per state, three quarters of a million states.
    ///
    pub def binomials(): Vector[Vector[Int32]] =
        Vector.range(0, 25) |> Vector.map(n ->
            Vector.range(0, 9) |> Vector.map(k -> binomial(n, k)))

    ///
    /// How many ways to choose `k` of `n`.
    ///
    def binomial(n: Int32, k: Int32): Int32 =
        if (k < 0 or k > n)
            0
        else
            List.range(0, k) |> List.foldLeft(acc -> i -> acc * (n - i) / (i + 1), 1)

    ///
    /// Every state one move from `at`, for a subset coordinate.
    ///
    /// The subset travels as a bit per slot rather than as eight numbers.
    /// Moving it is then a bit per slot too, and ranking it is one scan --
    /// where a list of slots would have to be sorted first, thirty-six times
    /// for every state in the table.
    ///
    pub def neighbours(
        binoms: Vector[Vector[Int32]],
        steps: Vector[Vector[Int32]],
        n: Int32,
        k: Int32,
        at: Int32
    ): Vector[Int32] =
        let mask = maskOf(binoms, n, k, at);
        steps |> Vector.map(step -> rankOfMask(binoms, n, moveMask(step, mask, n, 0, 0)))

    ///
    /// The subset at a place, as one bit per slot.
    ///
    def maskOf(binoms: Vector[Vector[Int32]], n: Int32, k: Int32, r: Int32): Int32 =
        climb(binoms, n - 1, k, r, 0)

    ///
    /// Fills the mask from the top slot down.
    ///
    def climb(binoms: Vector[Vector[Int32]], p: Int32, k: Int32, left: Int32, mask: Int32): Int32 =
        if (k == 0 or p < 0)
            mask
        else
            let weight = Vector.get(k, Vector.get(p, binoms));
            if (weight <= left)
                climb(binoms, p - 1, k - 1, left - weight, Int32.setBit(pos = p, mask))
            else
                climb(binoms, p - 1, k, left, mask)

    ///
    /// The mask after a move: every slot's bit goes where the slot goes.
    ///
    def moveMask(step: Vector[Int32], mask: Int32, n: Int32, p: Int32, out: Int32): Int32 =
        if (p >= n)
            out
        else if (Int32.bitwiseAnd(Int32.rightShift(mask, p), 1) == 1)
            moveMask(step, mask, n, p + 1, Int32.setBit(pos = Vector.get(p, step), out))
        else
            moveMask(step, mask, n, p + 1, out)

    ///
    /// The place of a subset given as a mask: one scan, no sorting.
    ///
    def rankOfMask(binoms: Vector[Vector[Int32]], n: Int32, mask: Int32): Int32 =
        countUp(binoms, mask, n, 0, 0, 0)

    ///
    /// Adds each chosen slot's weight, in slot order.
    ///
    def countUp(binoms: Vector[Vector[Int32]], mask: Int32, n: Int32, p: Int32, taken: Int32, acc: Int32): Int32 =
        if (p >= n)
            acc
        else if (Int32.bitwiseAnd(Int32.rightShift(mask, p), 1) == 1)
            countUp(binoms, mask, n, p + 1, taken + 1, acc + Vector.get(taken + 1, Vector.get(p, binoms)))
        else
            countUp(binoms, mask, n, p + 1, taken, acc)


    ///
    /// The exact number of moves each separation is from solved.
    ///
    /// Breadth first from the solved state over the wide turns, marking as it
    /// goes. Seconds, once.
    ///
    pub def table(): Vector[Int32] =
        let binoms = binomials();
        let steps = slotMoves();
        CubeSolvers.Distances.of(count(), solved(), at ->
            neighbours(binoms, steps, 24, 8, at))


}
