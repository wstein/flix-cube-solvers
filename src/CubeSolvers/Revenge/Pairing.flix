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
        symmetries = { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        scratch = Array[Int32, r],
        layout = CubeSolvers.Revenge.Pairs.Layout,
        centreSteps = {
            up = Vector[Vector[Int32]],
            front = Vector[Vector[Int32]],
            side = Vector[Vector[Int32]],
            flips = Vector[Int32]
        },
        binomials = Vector[Vector[Int32]],
        orbits = Vector[{ lowInverse = Vector[Int32], high = Vector[Int32] }]
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
          symmetries = Edges.rotationTables(),
          scratch = Array.repeat(rc, 12, 0),
          layout = CubeSolvers.Revenge.Pairs.layout(),
          centreSteps = Reduce.stepsFor(moves()),
          binomials = CubeSolvers.Revenge.Centers.binomials(),
          orbits = CubeSolvers.Revenge.Pairs.orbitMaps() }

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
        let pairing = CubeSolvers.Revenge.Pairs.relativeIn(t#layout, cube);
        let centre = Reduce.coordinate(cube);
        // Walked once, at the root. Every node below derives its distance
        // from its parent's, which is what two bits an entry is for.
        let edgeDepth = edgeDistance(t, edgeIndexOf(t, pairing));
        List.range(0, limit + 1) |> List.findMap(depth ->
            walk(t, centre, pairing, edgeDepth, depth, -1))

    ///
    /// How far this phase must go, at least.
    ///
    /// Both tables are exact, so neither can overstate the distance and the
    /// larger of them is a bound. Worth asking before searching: a candidate
    /// whose bound already exceeds the budget cannot be finished within it,
    /// and answering that costs two lookups instead of a search.
    ///
    pub def bound(t: Tables[r], cube: Facelets): Int32 \ r =
        let centre = Reduce.coordinate(cube);
        if (centre < 0)
            -1
        else
            let pairing = CubeSolvers.Revenge.Pairs.relativeIn(t#layout, cube);
            Int32.max(Vector.get(centre, t#centres),
                      edgeDistance(t, edgeIndexOf(t, pairing)))

    ///
    /// The pairing, as the edge table indexes it, without allocating a state
    /// for the symmetry rotation.
    ///
    def edgeIndexOf(t: Tables[r], pairing: Vector[Int32]): Int32 =
        Edges.indexOfPermutation(t#classes, t#symmetries, pairing)

    ///
    /// Depth-first to exactly `depth` moves, pruned by both tables.
    /// Everything is carried in coordinates. Turning ninety-six stickers at
    /// every node, and reading a coordinate off them, costs twenty times what
    /// a table lookup does -- and this phase reaches thirteen moves deep.
    /// Nothing is turned. The centres travel as a coordinate, exact because
    /// their slots do not move with the cube; the pairing travels as itself,
    /// each move sending it to `b . R . a`. So the pairing stays absolute --
    /// `R` being the identity is the goal. The edge bound is ranked through
    /// precomputed symmetry maps, rather than by allocating a rotated state.
    def walk(
        t: Tables[r],
        centre: Int32,
        pairing: Vector[Int32],
        edgeDepth: Int32,
        depth: Int32,
        last: Int32
    ): Option[MoveSequence] \ r =
        if (centre < 0)
            None
        else
            let centreDepth = Vector.get(centre, t#centres);
            if (centreDepth == 0 and pairing == Vector.range(0, 12))
                if (depth == 0) Some(Nil) else None
            else if (depth == 0 or Int32.max(centreDepth, edgeDepth) > depth)
                None
            else
                List.range(0, Vector.length(moves()))
                    |> List.filter(m -> allowed(m, last))
                    |> List.findMap(m ->
                        let toCentre = Reduce.after(t#centreSteps, t#binomials, centre, m);
                        let toPairing = CubeSolvers.Revenge.Pairs.turnedBy(Vector.get(m, t#orbits), pairing);
                        let toDepth = stepped(Edges.get(t#edges, edgeIndexOf(t, toPairing)), edgeDepth);
                        walk(t, toCentre, toPairing, toDepth, depth - 1, m)
                            |> Option.map(rest -> Vector.get(m, moves()) :: rest))

    ///
    /// The neighbour's distance, from its two stored bits and this one's.
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
