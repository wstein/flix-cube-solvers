///
/// The second phase: finish a cube that needs no quarter turn of the side
/// faces.
///
/// Inside that subgroup nothing twists and nothing flips -- only the order of
/// the pieces is left. So the coordinates are three permutation numbers: the
/// eight corners, the eight edges of the U and D layers, and the four in the
/// middle. Ten moves are available: any turn of U or D, and half turns of the
/// other four.
///
mod Cube.Rubik.Phase2 {

    use Cube.Move
    use Cube.Rubik.{State, moves}
    use Cube.Rubik.Prune.pairTable

    ///
    /// The moves this phase may use, as indices into `Cube.Rubik.moves`.
    ///
    /// Any turn of U and D, and only half turns of R, L, F and B: a quarter
    /// turn of a side face would twist corners or flip edges and undo what the
    /// first phase achieved.
    ///
    pub def moveIndices(): Vector[Int32] = Vector#{0, 1, 2, 4, 7, 9, 10, 11, 13, 16}

    ///
    /// How many orders eight pieces can be in.
    ///
    pub def cornerCount(): Int32 = 40320

    ///
    /// How many orders the eight U and D edges can be in.
    ///
    pub def edgeCount(): Int32 = 40320

    ///
    /// How many orders the four middle edges can be in.
    ///
    pub def sliceCount(): Int32 = 24

    ///
    /// The order of the corners, as one number.
    ///
    pub def cornerPerm(s: State): Int32 = rank(Cube.Rubik.corners(s))

    ///
    /// A state whose corners are in the order the number names.
    ///
    pub def withCornerPerm(n: Int32): State =
        State.State(unrank(8, n), Vector.repeat(8, 0), Vector.range(0, 12), Vector.repeat(12, 0))

    ///
    /// The order of the U and D edges, as one number.
    ///
    pub def edgePerm(s: State): Int32 = rank(Vector.slice(start = 0, end = 8, Cube.Rubik.edges(s)))

    ///
    /// A state whose U and D edges are in the order the number names.
    ///
    pub def withEdgePerm(n: Int32): State =
        State.State(
            Vector.range(0, 8),
            Vector.repeat(8, 0),
            Vector.append(unrank(8, n), Vector#{8, 9, 10, 11}),
            Vector.repeat(12, 0))

    ///
    /// The order of the four middle edges, as one number.
    ///
    pub def slicePerm(s: State): Int32 =
        rank(Vector.slice(start = 8, end = 12, Cube.Rubik.edges(s)) |> Vector.map(e -> e - 8))

    ///
    /// A state whose middle edges are in the order the number names.
    ///
    pub def withSlicePerm(n: Int32): State =
        State.State(
            Vector.range(0, 8),
            Vector.repeat(8, 0),
            Vector.append(Vector.range(0, 8), unrank(4, n) |> Vector.map(e -> e + 8)),
            Vector.repeat(12, 0))

    ///
    /// Everything the second phase needs, computed once.
    ///
    pub type alias Tables = {
        cornerMoves = Vector[Vector[Int32]],
        edgeMoves = Vector[Vector[Int32]],
        sliceMoves = Vector[Vector[Int32]],
        cornerSlice = Vector[Int32],
        edgeSlice = Vector[Int32]
    }

    ///
    /// Builds the move and pruning tables. Seconds, once.
    ///
    pub def tables(): Tables =
        let cornerMoves = moveTable(cornerCount(), withCornerPerm, cornerPerm);
        let edgeMoves = moveTable(edgeCount(), withEdgePerm, edgePerm);
        let sliceMoves = moveTable(sliceCount(), withSlicePerm, slicePerm);
        {
            cornerMoves = cornerMoves,
            edgeMoves = edgeMoves,
            sliceMoves = sliceMoves,
            cornerSlice = pairTable(cornerMoves, sliceMoves),
            edgeSlice = pairTable(edgeMoves, sliceMoves)
        }

    ///
    /// Where each of this phase's moves takes each value of a coordinate.
    ///
    def moveTable(count: Int32, decode: Int32 -> State, encode: State -> Int32): Vector[Vector[Int32]] =
        Vector.range(0, count) |> Vector.map(c ->
            let s = decode(c);
            moveIndices() |> Vector.map(i -> match Cube.Rubik.applyMove(s, Vector.get(i, moves())) {
                case Some(t) => encode(t)
                case None    => c
            }))

    ///
    /// A shortest finish for a cube already in the subgroup, or `None` within
    /// `maxDepth`.
    ///
    pub def finish(t: Tables, s: State, maxDepth: Int32): Option[List[Move]] =
        let start = (cornerPerm(s), edgePerm(s), slicePerm(s));
        List.range(0, maxDepth + 1) |> List.findMap(depth -> search(t, start, depth, -1))

    ///
    /// A finish of exactly `depth` moves, if there is one.
    ///
    pub def exactly(t: Tables, s: State, depth: Int32): Option[List[Move]] =
        search(t, (cornerPerm(s), edgePerm(s), slicePerm(s)), depth, -1)

    ///
    /// Depth-first to exactly `depth` moves, pruned by the tables.
    ///
    def search(t: Tables, at: (Int32, Int32, Int32), depth: Int32, last: Int32): Option[List[Move]] =
        let (a, b, c) = at;
        let n = sliceCount();
        let lower = Int32.max(Vector.get(a * n + c, t#cornerSlice), Vector.get(b * n + c, t#edgeSlice));
        if (lower == 0 and depth == 0)
            Some(Nil)
        else if (lower > depth)
            None
        else
            List.range(0, 10)
                |> List.filter(m -> allowed(m, last))
                |> List.findMap(m ->
                    let next = (
                        Vector.get(m, Vector.get(a, t#cornerMoves)),
                        Vector.get(m, Vector.get(b, t#edgeMoves)),
                        Vector.get(m, Vector.get(c, t#sliceMoves)));
                    search(t, next, depth - 1, m)
                        |> Option.map(rest -> Vector.get(Vector.get(m, moveIndices()), moves()) :: rest))

    ///
    /// Whether this phase's move `m` may follow move `last`.
    ///
    /// The same two rules as the first phase, applied to the faces these
    /// indices stand for.
    ///
    def allowed(m: Int32, last: Int32): Bool =
        if (last < 0)
            true
        else
            let f = Vector.get(m, moveIndices()) / 3;
            let g = Vector.get(last, moveIndices()) / 3;
            if (f == g)
                false
            else if (f == (g + 3) `Int32.modulo` 6)
                f > g
            else
                true

    ///
    /// The place of a permutation in lexicographic order.
    ///
    def rank(p: Vector[Int32]): Int32 =
        let n = Vector.length(p);
        List.range(0, n) |> List.foldLeft(acc -> i ->
            let smaller = List.range(i + 1, n) |> List.count(j -> Vector.get(j, p) < Vector.get(i, p));
            acc + smaller * factorial(n - 1 - i), 0)

    ///
    /// The permutation at the given place in lexicographic order.
    ///
    def unrank(n: Int32, r: Int32): Vector[Int32] =
        let (_, _, picked) = List.foldLeft(state -> i -> pick(state, n, i), (r, List.range(0, n), Nil), List.range(0, n));
        picked |> List.reverse |> List.toVector

    ///
    /// Takes the next element of a permutation being decoded.
    ///
    def pick(state: (Int32, List[Int32], List[Int32]), n: Int32, i: Int32): (Int32, List[Int32], List[Int32]) =
        let (left, available, picked) = state;
        let f = factorial(n - 1 - i);
        let k = left / f;
        match List.nth(k, available) {
            case None    => (left, available, picked)
            case Some(x) => (left `Int32.modulo` f, List.filter(y -> y != x, available), x :: picked)
        }

    ///
    /// `n` factorial, for the small `n` a cube needs.
    ///
    def factorial(n: Int32): Int32 =
        List.range(0, n) |> List.foldLeft(acc -> i -> acc * (i + 1), 1)

}
