///
/// The 4x4's second phase.
///
/// One test builds both tables and does all the asserting, because building
/// them twice would cost more than the rest of the suite together.
///
mod TestRevengeSeparate {

    use CubeSolvers.Kind
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Centers
    use CubeSolvers.Revenge.Separate.{count, moves, finishing, outer, side, coordinate,
        outerSolved, sideSolved, sideGoals, goals, table}
    use CubeSolvers.Revenge.Walk

    def scrambles(): List[String] =
        "D2 F' Lw2 R2 Dw2 Fw B' L' Bw' Dw2 Lw2 Bw U' R2 D" ::
        "L' R U2 F2 U' R' B Lw B2 Rw2 Fw' Dw2 Fw2 R Lw2" ::
        "Fw R Dw2 Rw2 B Lw R Bw2 F' Bw Fw U2 Rw' L2 F2" :: Nil

    def scrambled(text: String): Option[Facelets] =
        CubeSolvers.parse(text)
            |> Result.toOption
            |> Option.flatMap(ms ->
                CubeSolvers.Turning.replay(CubeSolvers.Solver.solved(Kind.Cube4x4), ms) |> Result.toOption)

    @Test
    def thePhasesUseTheMoveSetsTsaiGivesThem(): Unit \ Assert =
        Assert.assertEq(expected = 28, Vector.length(moves()));
        Assert.assertEq(expected = 20, Vector.length(finishing()))

    @Test
    def aSolvedCubeIsAtTheGoal(): Unit \ Assert =
        let cube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        Assert.assertEq(expected = outerSolved(), outer(cube));
        Assert.assertEq(expected = sideSolved(), side(cube))

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

    ///
    /// Every state reaches a goal, none further than nine moves, and real
    /// cubes come out where the third phase expects them.
    ///
    @Test
    def thePhaseIsCompleteAndLeavesCubesWhereTheThirdCanTakeThem(): Unit \ Assert =
        let one = Centers.table();
        let two = table();
        Assert.assertEq(expected = count(), Vector.length(two));
        Assert.assertEq(expected = 0, Vector.count(d -> d < 0, two));
        Assert.assertEq(expected = 9, Vector.foldLeft(Int32.max, 0, two));
        foreach (text <- scrambles()) {
            match scrambled(text) {
                case None       => Assert.assertEq(expected = "a cube", "unparsed: ${text}")
                case Some(cube) => match reduce(cube, one, two) {
                    case Err(why)  => Assert.assertEq(expected = "a reduction", "${why}")
                    case Ok(after) =>
                        Assert.assertEq(expected = outerSolved(), outer(after));
                        Assert.assertEq(expected = true, Vector.memberOf(side(after), sideGoals()))
                }
            }
        }

    ///
    /// The cube after both phases.
    ///
    def reduce(cube: Facelets, one: Vector[Int32], two: Vector[Int32]): Result[CubeSolvers.Solver.Invalid, Facelets] =
        forM (first <- Walk.down(cube, one, Centers.separation, Centers.moves());
              mid <- CubeSolvers.Turning.replay(cube, first);
              second <- Walk.down(mid, two, coordinate, moves());
              after <- CubeSolvers.Turning.replay(mid, second))
            yield after

    @Test
    def everyGoalIsAGoal(): Unit \ Assert =
        Assert.assertEq(expected = 12, Vector.length(goals()));
        foreach (g <- Vector.toList(goals()))
            Assert.assertEq(expected = true, g >= 0 and g < count())

}
