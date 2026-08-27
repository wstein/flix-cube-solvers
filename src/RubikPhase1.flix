///
/// The first phase: turn any cube into one that needs no quarter turn of F,
/// B, R or L.
///
/// Those cubes -- corners untwisted, edges unflipped, the four slice edges
/// home in the middle layer -- are the ones the second phase can finish using
/// only half turns of the side faces. Reaching them is a search over three
/// small numbers rather than over the cube, which is what makes it quick.
///
mod Cube.Rubik.Phase1 {

    use Cube.{Face, Move}
    use Cube.Rubik.{State, moves}
    use Cube.Rubik.Coord.{twist, flip, slice, twistTable, flipTable, sliceTable}
    use Cube.Rubik.Prune.pairTable

    ///
    /// Everything the first phase needs, computed once.
    ///
    pub type alias Tables = {
        twistMoves = Vector[Vector[Int32]],
        flipMoves = Vector[Vector[Int32]],
        sliceMoves = Vector[Vector[Int32]],
        twistSlice = Vector[Int32],
        flipSlice = Vector[Int32]
    }

    ///
    /// Builds the move and pruning tables. Seconds, once.
    ///
    pub def tables(): Tables =
        let twistMoves = twistTable();
        let flipMoves = flipTable();
        let sliceMoves = sliceTable();
        {
            twistMoves = twistMoves,
            flipMoves = flipMoves,
            sliceMoves = sliceMoves,
            twistSlice = pairTable(twistMoves, sliceMoves),
            flipSlice = pairTable(flipMoves, sliceMoves)
        }

    ///
    /// A shortest way into that set of cubes, or `None` if there is none
    /// within `maxDepth` moves.
    ///
    /// Lengths are tried in order, so the first answer is a shortest one. It
    /// is not a shortest *solution* -- the second phase may finish a longer
    /// first phase in fewer moves overall -- which is why a real two-phase
    /// search keeps going after the first answer.
    ///
    pub def reduce(t: Tables, s: State, maxDepth: Int32): Option[List[Move]] =
        let start = (twist(s), flip(s), slice(s));
        List.range(0, maxDepth + 1) |> List.findMap(depth -> search(t, start, depth, -1))

    ///
    /// A reduction of exactly `depth` moves, if there is one.
    ///
    /// The whole solver wants this rather than `reduce`: a longer first phase
    /// often leaves a much shorter second one, so it is worth asking for each
    /// length in turn instead of stopping at the first that works.
    ///
    pub def exactly(t: Tables, s: State, depth: Int32): Option[List[Move]] =
        search(t, (twist(s), flip(s), slice(s)), depth, -1)

    ///
    /// The lower bound on how many moves this cube still needs.
    ///
    /// Two exact distances in two smaller worlds; the cube cannot be closer
    /// than the further of them.
    ///
    pub def estimate(t: Tables, s: State): Int32 =
        let sliceCount = Vector.length(t#sliceMoves);
        Int32.max(
            Vector.get(twist(s) * sliceCount + slice(s), t#twistSlice),
            Vector.get(flip(s) * sliceCount + slice(s), t#flipSlice))

    ///
    /// Depth-first to exactly `depth` moves, pruned by the tables.
    ///
    def search(t: Tables, at: (Int32, Int32, Int32), depth: Int32, last: Int32): Option[List[Move]] =
        let (a, b, c) = at;
        let sliceCount = Vector.length(t#sliceMoves);
        let lower = Int32.max(
            Vector.get(a * sliceCount + c, t#twistSlice),
            Vector.get(b * sliceCount + c, t#flipSlice));
        if (lower == 0 and depth == 0)
            Some(Nil)
        else if (lower > depth)
            None
        else
            List.range(0, 18)
                |> List.filter(m -> allowed(m, last))
                |> List.findMap(m ->
                    let next = (
                        Vector.get(m, Vector.get(a, t#twistMoves)),
                        Vector.get(m, Vector.get(b, t#flipMoves)),
                        Vector.get(m, Vector.get(c, t#sliceMoves)));
                    search(t, next, depth - 1, m)
                        |> Option.map(rest -> Vector.get(m, moves()) :: rest))

    ///
    /// Whether move `m` may follow move `last`.
    ///
    /// Two turns of one face are one turn or none, so a face never repeats.
    /// Opposite faces commute, so of the two orders only one is searched --
    /// `U D` yes, `D U` no -- which halves the tree without losing an answer.
    ///
    def allowed(m: Int32, last: Int32): Bool =
        if (last < 0)
            true
        else
            let f = m / 3;
            let g = last / 3;
            if (f == g)
                false
            else if (f == opposite(g))
                f > g
            else
                true

    ///
    /// The face across the cube: U and D, R and L, F and B.
    ///
    def opposite(f: Int32): Int32 = (f + 3) `Int32.modulo` 6

}
