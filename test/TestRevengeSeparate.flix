///
/// The 4x4's second phase.
///
/// One test builds both tables and does all the asserting, because building
/// them twice would cost more than the rest of the suite together.
///
mod TestRevengeSeparate {

    use CubeSolvers.Kind
    use CubeSolvers.Revenge.Separate.{count, moves, finishing, outer, side, coordinate,
        outerSolved, sideSolved, sideGoals, goals, table}

    @Test
    def thePhasesUseTheMoveSetsTsaiGivesThem(): Unit \ Assert =
        Assert.assertEq(expected = 28, Vector.length(moves()));
        Assert.assertEq(expected = 20, Vector.length(finishing()))

    @Test
    def aSolvedCubeIsAtTheGoal(): Unit \ Assert =
        let cube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        Assert.assertEq(expected = outerSolved(), outer(cube));
        Assert.assertEq(expected = sideSolved(), side(cube));
        Assert.assertEq(expected = 0, CubeSolvers.Revenge.Separate.mismatch(cube))

    ///
    /// An outer quarter turn is a four-cycle of corners and two four-cycles
    /// of wings: odd against even, so it flips the agreement between them. A
    /// half turn and a wide turn do not.
    ///
    @Test
    def onlyOuterQuarterTurnsFlipTheParityMismatch(): Unit \ Assert =
        let flipped = CubeSolvers.Revenge.Separate.flips();
        Assert.assertEq(expected = 28, Vector.length(flipped));
        Assert.assertEq(expected = 12, Vector.sum(flipped))

    ///
    /// Tsai's second step leaves the R and L centres "in one of 12 positions
    /// that can be solved in later steps". Twelve is not written down here:
    /// an arrangement qualifies exactly when the third phase's moves can
    /// still carry it home, and walking those from solved finds twelve.
    ///
    @Test
    def twelveArrangementsOfTheSideCentresAreFinishable(): Unit \ Assert =
        Assert.assertEq(expected = 12, Vector.length(sideGoals()));
        Assert.assertEq(expected = true, Vector.memberOf(sideSolved(), sideGoals()))



    @Test
    def everyGoalIsAGoal(): Unit \ Assert =
        Assert.assertEq(expected = 12, Vector.length(goals()));
        foreach (g <- Vector.toList(goals()))
            Assert.assertEq(expected = true, g >= 0 and g < count())

    ///
    /// Turning the move order gives different answers, all of them valid.
    ///
    /// This is not cosmetic. Answers read in one fixed order share long
    /// prefixes, and the next phase sifts them for a property about one in
    /// five hundred has -- so a correlated family of six thousand can yield
    /// none where a spread-out family of thirty thousand yields sixty-four.
    ///
    @Test
    def turningTheMoveOrderGivesDifferentAnswers(): Unit \ Assert =
        let dist = table();
        let steps = CubeSolvers.Revenge.Separate.steps();
        let cube = CubeSolvers.parse("Rw2 U Fw2 R2 Uw2 F")
            |> Result.toOption
            |> Option.flatMap(ms ->
                CubeSolvers.Turning.replay(CubeSolvers.Solver.solved(Kind.Cube4x4), ms) |> Result.toOption);
        match cube {
            case None => Assert.assertEq(expected = "a cube", "no cube")
            case Some(c) =>
                let at = coordinate(c);
                let depth = Vector.get(at, dist);
                // two moves deeper, where there are far more answers than the cap
                let wanted = depth + 2;
                let plain = CubeSolvers.Revenge.Separate.manyExactlyFrom(steps, dist, at, wanted, 12, 0);
                let spread = List.range(0, 6)
                    |> List.flatMap(spin ->
                        CubeSolvers.Revenge.Separate.manyExactlyFrom(steps, dist, at, wanted, 12, spin))
                    |> List.distinct;
                // the cap has to bite, or turning the order proves nothing
                let available = CubeSolvers.Revenge.Separate.manyExactlyFrom(
                    steps, dist, at, wanted, 5000, 0);
                Assert.assertEq(expected = true, List.length(available) > 12);
                Assert.assertEq(expected = 12, List.length(plain));
                // turning the order reaches answers one fixed order never returns
                Assert.assertEq(expected = true, List.length(spread) > List.length(plain));
                // every answer, however ordered, is exactly that long and lands on the goal
                foreach (answer <- plain ::: spread) {
                    Assert.assertEq(expected = wanted, List.length(answer));
                    let landed = List.foldLeft(x -> m ->
                        CubeSolvers.Revenge.Separate.after(steps, x, m), at, answer);
                    Assert.assertEq(expected = 0, Vector.get(landed, dist))
                }
        }

}
