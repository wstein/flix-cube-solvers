///
/// How far the 4x4's wings are from being paired.
///
/// Reduction means every place holding two wings that belong together. Where
/// the pair belongs is not this phase's business -- a pair in the wrong place
/// is still a pair, and the 3x3 that follows will move it.
///
/// So the state that matters is one permutation of twelve: read each place's
/// two wings, ask which place each came from, and compose one answer with the
/// other's inverse. Identity means paired.
///
/// This only makes sense inside the third phase. Under all 36 moves a wing can
/// reach any of the 24 places, but under the third phase's twenty they split
/// into two orbits of twelve, one wing of every place in each. That split is
/// what lets the twenty-four be read as a permutation of twelve, and it is
/// computed here rather than tabulated.
///
mod CubeSolvers.Revenge.Pairs {

    use CubeSolvers.Kind
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Wings.{pieces, places, identity}

    ///
    /// Where each wing position goes, for each move of the third phase.
    ///
    pub def steps(): Vector[Vector[Int32]] =
        let all = pieces();
        let firstOf = all |> Vector.mapWithIndex(i -> match (a, _) -> (a, i)) |> Vector.toList |> List.toMap;
        CubeSolvers.Revenge.Reduce.moves() |> Vector.map(m ->
            match CubeSolvers.Turning.permutation(Kind.Cube4x4, m) {
                case Err(_)   => Vector.range(0, 24)
                case Ok(perm) => all |> Vector.map(match (a, b) ->
                    match (Map.get(Vector.get(a, perm), firstOf), Map.get(Vector.get(b, perm), firstOf)) {
                        case (Some(i), _) => i
                        case (_, Some(j)) => j
                        case _            => -1
                    })
            })

    ///
    /// Which wings share the orbit of wing zero under the third phase.
    ///
    /// Twelve of the twenty-four, exactly one from each place.
    ///
    pub def firstOrbit(): Set[Int32] = orbit(steps(), Set#{0})

    ///
    /// Everything reachable from `seen`.
    ///
    def orbit(moves: Vector[Vector[Int32]], seen: Set[Int32]): Set[Int32] =
        let next = seen |> Set.flatMap(w ->
            moves |> Vector.map(s -> Vector.get(w, s)) |> Vector.toList |> List.toSet);
        let both = Set.union(seen, next);
        if (Set.size(both) == Set.size(seen)) seen else orbit(moves, both)

    ///
    /// Each place's two wings, the one from the first orbit leading.
    ///
    pub def sides(): Vector[(Int32, Int32)] =
        let first = firstOrbit();
        let all = pieces();
        let firstOf = all |> Vector.mapWithIndex(i -> match (a, _) -> (a, i)) |> Vector.toList |> List.toMap;
        places() |> Vector.map(match (a, b) ->
            let x = Map.getWithDefault(a, 0, firstOf);
            let y = Map.getWithDefault(b, 0, firstOf);
            if (Set.memberOf(x, first)) (x, y) else (y, x))

    ///
    /// Which place each wing belongs to.
    ///
    pub def homePlace(): Vector[Int32] =
        region rc {
            let out = Array.repeat(rc, 24, -1);
            foreach ((p, (x, y)) <- List.mapWithIndex(i -> pair -> (i, pair), Vector.toList(sides()))) {
                Array.put(p, x, out);
                Array.put(p, y, out)
            };
            Array.toVector(out)
        }

    ///
    /// Whether the wings are split the way the third phase needs them.
    ///
    /// Every place must hold one wing from each orbit. The third phase cannot
    /// move a wing out of its orbit, so a place holding two from the same one
    /// can never be paired -- and `relative` is not even a permutation there.
    ///
    /// The second phase does not guarantee this. It is a condition on the
    /// second phase's *answer*: generate several, keep the ones that separate
    /// the wings, hand those on. Tsai lists it as an objective of his second
    /// step; TPR checks it afterwards, which comes to the same thing.
    ///
    pub def isSeparated(cube: Facelets): Bool =
        let who = identity(cube);
        let home = homePlace();
        let places = firstOrbit()
            |> Set.toList
            |> List.map(position -> Vector.get(Vector.get(position, who), home));
        // Twelve positions, twelve different places: no place may have both
        // its wings on the same side of the split, or the pairing is not a
        // permutation and the third phase has nothing to walk.
        let distinct = List.length(List.distinct(places));
        let crossed = firstOrbit()
            |> Set.toList
            |> List.count(position -> not Set.memberOf(Vector.get(position, who), firstOrbit()));
        distinct == 12 and crossed `Int32.modulo` 2 == 0

    ///
    /// Everything about the wings that does not depend on the cube.
    ///
    /// The orbits, the pairing of places, where each wing belongs and how
    /// this package's numbering maps to the edge tables'. All of it is fixed
    /// by the geometry, and recomputing it per cube -- which meant twenty
    /// sticker permutations and an orbit walk each time -- is what made the
    /// search take minutes rather than seconds.
    ///
    pub type alias Layout = {
        sides = Vector[(Int32, Int32)],
        home = Vector[Int32],
        correspondence = Vector[Int32],
        back = Vector[Int32],
        ordered = Vector[(Int32, Int32)],
        wingOfFaces = Map[(Int32, Int32), Int32]
    }

    ///
    /// Works it out, once.
    ///
    pub def layout(): Layout =
        let tau = placeCorrespondence();
        let slots = CubeSolvers.Revenge.Wings.ordered();
        { sides = sides(),
          home = homePlace(),
          correspondence = tau,
          back = invert(tau),
          ordered = slots,
          wingOfFaces = slots
            |> Vector.mapWithIndex(i -> match (a, b) -> ((a / 16, b / 16), i))
            |> Vector.toList
            |> List.toMap }

    ///
    /// Which wing sits at each place, given the layout.
    ///
    /// The same answer as `Wings.identity`, without rebuilding the piece
    /// tables and searching them for every wing. This runs once per node of
    /// the third phase's search, so the difference is the difference between
    /// seconds and minutes.
    ///
    pub def identityIn(l: Layout, cube: Facelets): Vector[Int32] =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        l#ordered |> Vector.map(match (a, b) ->
            let shown = (faceNumber(Vector.get(a, stickers)), faceNumber(Vector.get(b, stickers)));
            Map.getWithDefault(shown, -1, l#wingOfFaces))

    ///
    /// How far each place is from holding a matching pair, given the layout.
    ///
    pub def relativeIn(l: Layout, cube: Facelets): Vector[Int32] =
        let who = identityIn(l, cube);
        let low = l#sides |> Vector.map(match (x, _) -> Vector.get(Vector.get(x, who), l#home));
        let high = l#sides |> Vector.map(match (_, y) -> Vector.get(Vector.get(y, who), l#home));
        let back = invert(high);
        low |> Vector.map(p -> Vector.get(p, back))

    ///
    /// Whether every place holds a matching pair, given the layout.
    ///
    /// Cheap enough to ask at every node of a search: two dozen lookups and
    /// a comparison, where indexing the pruning table costs a ranking and a
    /// rotation.
    ///
    pub def isPairedIn(l: Layout, cube: Facelets): Bool =
        relativeIn(l, cube) == Vector.range(0, 12)

    ///
    /// Whether the wings are split the way the third phase needs, given the
    /// layout.
    ///
    pub def isSeparatedIn(l: Layout, cube: Facelets): Bool =
        let who = identityIn(l, cube);
        let places = l#sides |> Vector.map(match (x, _) -> Vector.get(Vector.get(x, who), l#home));
        List.length(List.distinct(Vector.toList(places))) == 12

    ///
    /// The cube's pairing in the edge tables' numbering, given the layout.
    ///
    pub def asEdgeStateIn(l: Layout, cube: Facelets): CubeSolvers.Revenge.Edges.State =
        let mine = relativeIn(l, cube);
        { edge = Vector.range(0, 12) |> Vector.map(theirs ->
            Vector.get(Vector.get(Vector.get(theirs, l#back), mine), l#correspondence)),
          turned = Vector.range(0, 12) }

    ///
    /// How far each place is from holding a matching pair.
    ///
    /// The identity permutation means every place holds one, which is what
    /// the third phase is searching for.
    ///
    pub def relative(cube: Facelets): Vector[Int32] =
        let who = identity(cube);
        let home = homePlace();
        let byPlace = sides();
        let low = byPlace |> Vector.map(match (x, _) -> Vector.get(Vector.get(x, who), home));
        let high = byPlace |> Vector.map(match (_, y) -> Vector.get(Vector.get(y, who), home));
        let back = invert(high);
        low |> Vector.map(p -> Vector.get(p, back))

    ///
    /// The permutation that undoes this one.
    ///
    def invert(perm: Vector[Int32]): Vector[Int32] =
        region rc {
            let out = Array.repeat(rc, Vector.length(perm), 0);
            foreach (i <- List.range(0, Vector.length(perm)))
                Array.put(i, Vector.get(i, perm), out);
            Array.toVector(out)
        }

    ///
    /// Whether every place holds a matching pair.
    ///
    pub def isPaired(cube: Facelets): Bool = relative(cube) == Vector.range(0, 12)

    ///
    /// The reference's twelve places, as the colours they show.
    ///
    /// From `EdgeColor` in TPR-4x4x4-Solver, with faces numbered as this
    /// package numbers them -- U, R, F, D, L, B -- which is the order its own
    /// `"URFDLB"` lookup uses.
    ///
    def referenceColours(): Vector[(Int32, Int32)] =
        Vector#{(2, 0), (4, 0), (5, 0), (1, 0), (5, 3), (4, 3), (2, 3), (1, 3), (2, 4), (5, 4), (5, 1), (2, 1)}

    ///
    /// How the reference renumbers those places again for its edge tables.
    ///
    /// `Edge3.set` reads its cube through `FullEdgeMap`, so its places are not
    /// the colour-pair places. Missing this step is what left four of twenty
    /// moves disagreeing, with everything else looking right.
    ///
    def edgeRenumbering(): Vector[Int32] = Vector#{0, 2, 4, 6, 1, 3, 7, 5, 8, 9, 10, 11}

    ///
    /// This package's places, in the numbering the edge tables use.
    ///
    /// Derived rather than written down: match each place to the reference's
    /// by the colours it shows when solved, then follow its renumbering. The
    /// answer is checked against all twenty of the phase's moves, which is
    /// the only check that means anything -- the fourteen outer moves agree
    /// under any numbering at all, because they move whole pairs and leave
    /// the pairing alone.
    ///
    pub def placeCorrespondence(): Vector[Int32] =
        let cube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let pieces = CubeSolvers.Revenge.Wings.pieces();
        let theirs = referenceColours() |> Vector.map(ordered);
        let renumbering = invert(edgeRenumbering());
        sides() |> Vector.map(match (low, _) ->
            let (a, b) = Vector.get(low, pieces);
            let shown = ordered((faceNumber(Vector.get(a, stickers)), faceNumber(Vector.get(b, stickers))));
            match Vector.indexOf(shown, theirs) {
                case Some(i) => Vector.get(i, renumbering)
                case None    => -1
            })

    ///
    /// A colour pair, smaller first.
    ///
    def ordered(p: (Int32, Int32)): (Int32, Int32) =
        let (a, b) = p;
        if (a < b) (a, b) else (b, a)

    ///
    /// Which face a colour is, as a number.
    ///
    def faceNumber(f: CubeSolvers.Face): Int32 = match f {
        case CubeSolvers.Face.U => 0
        case CubeSolvers.Face.R => 1
        case CubeSolvers.Face.F => 2
        case CubeSolvers.Face.D => 3
        case CubeSolvers.Face.L => 4
        case _                  => 5
    }

    ///
    /// The cube's pairing, in the numbering the edge tables use.
    ///
    pub def asEdgeState(cube: Facelets): CubeSolvers.Revenge.Edges.State =
        let tau = placeCorrespondence();
        let back = invert(tau);
        let mine = relative(cube);
        { edge = Vector.range(0, 12) |> Vector.map(theirs ->
            Vector.get(Vector.get(Vector.get(theirs, back), mine), tau)),
          turned = Vector.range(0, 12) }

    ///
    /// How each of the third phase's moves permutes the two wing orbits.
    ///
    /// A move sends the pairing permutation `R` to `b . R . a`, where `a` and
    /// `b` are its permutations of the low and high orbits read as places.
    /// Both are fixed by the geometry, so they are worked out once and the
    /// search then costs two dozen lookups a node rather than ninety-six
    /// stickers turned.
    ///
    pub def orbitMaps(): Vector[{ lowInverse = Vector[Int32], high = Vector[Int32] }] =
        let l = layout();
        let pieces = CubeSolvers.Revenge.Wings.pieces();
        let firstOf = pieces |> Vector.mapWithIndex(i -> match (a, _) -> (a, i)) |> Vector.toList |> List.toMap;
        let placeOfLow = placeIndex(l#sides, fst);
        let placeOfHigh = placeIndex(l#sides, snd);
        CubeSolvers.Revenge.Pairing.moves() |> Vector.map(m ->
            match CubeSolvers.Turning.permutation(Kind.Cube4x4, m) {
                case Err(_)   => { lowInverse = Vector.range(0, 12), high = Vector.range(0, 12) }
                case Ok(perm) =>
                    let landed = wing -> {
                        let (a, b) = Vector.get(wing, pieces);
                        match (Map.get(Vector.get(a, perm), firstOf), Map.get(Vector.get(b, perm), firstOf)) {
                            case (Some(i), _) => i
                            case (_, Some(j)) => j
                            case _            => -1
                        }
                    };
                    let low = l#sides |> Vector.map(match (x, _) ->
                        Map.getWithDefault(landed(x), -1, placeOfLow));
                    { lowInverse = invert(low),
                      high = l#sides |> Vector.map(match (_, y) ->
                        Map.getWithDefault(landed(y), -1, placeOfHigh)) }
            })

    ///
    /// Which place each orbit's wing belongs to.
    ///
    def placeIndex(sides: Vector[(Int32, Int32)], which: ((Int32, Int32)) -> Int32): Map[Int32, Int32] =
        sides |> Vector.mapWithIndex(p -> pair -> (which(pair), p)) |> Vector.toList |> List.toMap

    ///
    /// The pairing after a move, from the pairing before it.
    ///
    pub def turnedBy(maps: { lowInverse = Vector[Int32], high = Vector[Int32] }, r: Vector[Int32]): Vector[Int32] =
        Vector.range(0, 12) |> Vector.map(p ->
            Vector.get(Vector.get(Vector.get(p, maps#lowInverse), r), maps#high))

}
