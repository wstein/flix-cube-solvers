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
        let first = firstOrbit();
        sides() |> Vector.forAll(match (x, y) ->
            Set.memberOf(Vector.get(x, who), first) != Set.memberOf(Vector.get(y, who), first))

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

}
