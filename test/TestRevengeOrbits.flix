mod TestRevengeOrbits {

    use CubeSolvers.Kind
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.{Orbits, Pairs, Separate}

    def scrambled(text: String): Option[Facelets] =
        CubeSolvers.parse(text)
            |> Result.toOption
            |> Option.flatMap(ms ->
                CubeSolvers.Turning.replay(CubeSolvers.Solver.solved(Kind.Cube4x4), ms) |> Result.toOption)

    ///
    /// Twelve slots of twenty-four, which is what the coordinate counts.
    ///
    @Test
    def theCoordinateCountsSubsetsOfTwelve(): Unit \ Assert =
        let binoms = Orbits.binomials();
        Assert.assertEq(expected = 2704156, Orbits.count());
        Assert.assertEq(expected = Orbits.count(), Vector.get(12, Vector.get(24, binoms)))

    ///
    /// Ranking and unranking are inverse across the range.
    ///
    @Test
    def rankingRoundTrips(): Unit \ Assert =
        let binoms = Orbits.binomials();
        foreach (i <- List.range(0, 400)) {
            let at = (i * 6763) `Int32.modulo` Orbits.count();
            Assert.assertEq(expected = at, Orbits.rank(binoms, Orbits.chosen(Orbits.unrank(binoms, at))))
        }

    ///
    /// A solved cube has its wings split, and so is one of the two goals: the
    /// orbits at home, or the two exchanged.
    ///
    @Test
    def solvedIsAGoal(): Unit \ Assert =
        let l = Pairs.layout();
        let binoms = Orbits.binomials();
        let at = Orbits.coordinate(l, binoms, CubeSolvers.Solver.solved(Kind.Cube4x4));
        Assert.assertEq(expected = 2, Vector.length(Orbits.goals(binoms)));
        Assert.assertEq(expected = true, Vector.memberOf(at, Orbits.goals(binoms)))

    ///
    /// Stepping the coordinate agrees with turning the cube, for every move
    /// the second phase has. A coordinate that drifts from the cube is the
    /// failure mode this phase is most prone to, so it is pinned.
    ///
    @Test
    def everyMoveAgreesWithTheCube(): Unit \ Assert =
        let l = Pairs.layout();
        let binoms = Orbits.binomials();
        let steps = Orbits.steps();
        match scrambled("Rw2 U Fw2 R2 Uw2 F D Rw B L2 Uw2") {
            case None => Assert.assertEq(expected = "a cube", "no cube")
            case Some(cube) =>
                let at = Orbits.coordinate(l, binoms, cube);
                foreach (m <- List.range(0, Vector.length(Separate.moves()))) {
                    let byTable = Orbits.after(binoms, steps, at, m);
                    let byCube = match CubeSolvers.Turning.apply(cube, Vector.get(m, Separate.moves())) {
                        case Ok(next) => Orbits.coordinate(l, binoms, next)
                        case Err(_)   => -1
                    };
                    Assert.assertEq(expected = byCube, byTable)
                }
        }

    ///
    /// Reaching a goal means the third phase can read the wings as a pairing.
    /// The bound is only worth having if it implies the property being sought.
    ///
    @Test
    def aGoalImpliesSeparation(): Unit \ Assert =
        let l = Pairs.layout();
        let binoms = Orbits.binomials();
        let goals = Orbits.goals(binoms);
        let all = Separate.moves();
        foreach (k <- List.range(0, 40)) {
            let cube = List.range(0, 12) |> List.foldLeft(c -> i ->
                let m = Vector.get(((i * 7919 + 13) * (k + 3) + k * 31) `Int32.modulo` Vector.length(all), all);
                match CubeSolvers.Turning.apply(c, m) { case Ok(x) => x  case Err(_) => c },
                CubeSolvers.Solver.solved(Kind.Cube4x4));
            if (Vector.memberOf(Orbits.coordinate(l, binoms, cube), goals))
                Assert.assertEq(expected = true, Pairs.isSeparatedIn(l, cube))
            else
                ()
        }

}
