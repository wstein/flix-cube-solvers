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

}
