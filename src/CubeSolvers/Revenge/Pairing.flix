///
/// The 4x4's third phase: finish the centres and pair the edges at once.
///
/// Two tables bound the search. The centres are held exactly, in 117,600
/// entries; the edges are held two bits deep across 31,006,080, which is
/// enough to recover the true distance by walking down. A cube cannot be
/// nearer than the further of the two says, so the larger bound prunes.
///
/// This is the same shape as the 3x3's two-phase search, which also takes the
/// larger of two exact distances in smaller worlds.
///
mod CubeSolvers.Revenge.Pairing {

    use CubeSolvers.{Move, MoveSequence}
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Edges
    use CubeSolvers.Revenge.Reduce

    ///
    /// Everything the phase searches with.
    ///
    pub type alias Tables[r: Eff] = {
        centres = Vector[Int32],
        edges = Array[Int32, r],
        classes = { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 },
        rotations = { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        scratch = Array[Int32, r]
    }

    ///
    /// The phase's moves, in the order the edge tables number them.
    ///
    /// The order matters: an edge transition is looked up by move number, so
    /// a move list in any other order would index the tables wrongly.
    ///
    pub def moves(): Vector[Move] =
        let m = CubeSolvers.move;
        let q = CubeSolvers.Turn.Cw;
        let h = CubeSolvers.Turn.Half;
        let c = CubeSolvers.Turn.Ccw;
        let u = CubeSolvers.Face.U;
        let r = CubeSolvers.Face.R;
        let f = CubeSolvers.Face.F;
        let d = CubeSolvers.Face.D;
        let l = CubeSolvers.Face.L;
        let b = CubeSolvers.Face.B;
        Vector#{
            m(u, 1, q), m(u, 1, h), m(u, 1, c), m(r, 1, h),
            m(f, 1, q), m(f, 1, h), m(f, 1, c),
            m(d, 1, q), m(d, 1, h), m(d, 1, c), m(l, 1, h),
            m(b, 1, q), m(b, 1, h), m(b, 1, c),
            m(u, 2, h), m(r, 2, h), m(f, 2, h), m(d, 2, h), m(l, 2, h), m(b, 2, h)
        }

    ///
    /// Builds both tables. Minutes, once: the edge table is 31 million
    /// states.
    ///
    pub def prepared(rc: Region[r]): Tables[r] \ r =
        let built = Edges.pruning(rc, 20);
        { centres = Reduce.table(),
          edges = built#table,
          classes = Edges.classify(),
          rotations = Edges.moveRotations(),
          scratch = Array.repeat(rc, 12, 0) }

    ///
    /// How far the edges are from paired, walked out of the two-bit table.
    ///
    /// Two bits hold the depth modulo three. Stepping to a neighbour whose
    /// stored value is one less, over and over, counts the way down.
    ///
    pub def edgeDistance(t: Tables[r], index: Int32): Int32 \ r =
        descend(t, index, 0)

    ///
    /// Counts the steps down to the paired state.
    ///
    def descend(t: Tables[r], index: Int32, taken: Int32): Int32 \ r =
        if (index == 0)
            taken
        else
            let want = wanted(Edges.get(t#edges, index));
            match nearer(t, index, want, 0) {
                case Some(next) => descend(t, next, taken + 1)
                case None       => taken
            }

    ///
    /// The stored value a step nearer than this one.
    ///
    def wanted(here: Int32): Int32 = if (here == 0) 2 else here - 1

    ///
    /// A neighbour one step nearer.
    ///
    def nearer(t: Tables[r], index: Int32, want: Int32, m: Int32): Option[Int32] \ r =
        if (m >= Vector.length(moves()))
            None
        else
            let to = Edges.after(t#classes, t#rotations, t#scratch, index, m);
            if (Edges.get(t#edges, to) == want)
                Some(to)
            else
                nearer(t, index, want, m + 1)

    ///
    /// The moves that finish the centres and pair the edges, or `None` within
    /// `limit`.
    ///
    pub def solution(t: Tables[r], cube: Facelets, limit: Int32): Option[MoveSequence] \ r =
        let edgeIndex = Edges.indexOf(t#classes, CubeSolvers.Revenge.Pairs.asEdgeState(cube));
        let start = edgeDistance(t, edgeIndex);
        List.range(0, limit + 1) |> List.findMap(depth ->
            walk(t, cube, edgeIndex, start, depth, -1))

    ///
    /// Depth-first to exactly `depth` moves, pruned by both tables.
    ///
    def walk(
        t: Tables[r],
        cube: Facelets,
        edgeIndex: Int32,
        edgeDepth: Int32,
        depth: Int32,
        last: Int32
    ): Option[MoveSequence] \ r =
        let centreDepth = Vector.get(Reduce.coordinate(cube), t#centres);
        if (centreDepth < 0)
            None
        else if (centreDepth == 0 and edgeIndex == 0 and depth == 0)
            Some(Nil)
        else if (Int32.max(centreDepth, edgeDepth) > depth)
            None
        else
            List.range(0, Vector.length(moves()))
                |> List.filter(m -> allowed(m, last))
                |> List.findMap(m ->
                    match CubeSolvers.Turning.apply(cube, Vector.get(m, moves())) {
                        case Err(_)   => None
                        case Ok(next) =>
                            let toIndex = Edges.after(t#classes, t#rotations, t#scratch, edgeIndex, m);
                            let toDepth = stepped(Edges.get(t#edges, toIndex), edgeDepth);
                            walk(t, next, toIndex, toDepth, depth - 1, m)
                                |> Option.map(rest -> Vector.get(m, moves()) :: rest)
                    })

    ///
    /// The neighbour's true distance, from its two stored bits and this one's.
    ///
    /// A neighbour is one nearer, level, or one further, and the stored value
    /// modulo three says which.
    ///
    def stepped(stored: Int32, here: Int32): Int32 =
        ((stored - here + 16) `Int32.modulo` 3) + here - 1

    ///
    /// Whether move `m` may follow move `last`.
    ///
    def allowed(m: Int32, last: Int32): Bool =
        if (last < 0)
            true
        else
            let f = CubeSolvers.face(Vector.get(m, moves()));
            let g = CubeSolvers.face(Vector.get(last, moves()));
            if (f == g)
                CubeSolvers.depth(Vector.get(m, moves())) != CubeSolvers.depth(Vector.get(last, moves()))
            else
                true

}
