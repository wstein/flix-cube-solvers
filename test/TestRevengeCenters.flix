///
/// The 4x4 centres.
///
/// Building the three tables costs tens of seconds, so one test does all the
/// asserting: the alternative is paying for them once per assertion.
///
mod TestRevengeCenters {

    use CubeSolvers.Kind
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Centers.{count, axisCount, colourCount, slots, moves, keeping, pairing,
        slotMoves, separation, axis, colours, solved, axisSolved, coloursSolved, rank, unrank,
        tables, solution}

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
    def aSolvedCubeIsAtEveryStagesGoal(): Unit \ Assert =
        let cube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        Assert.assertEq(expected = solved(), separation(cube));
        Assert.assertEq(expected = axisSolved(), axis(cube));
        Assert.assertEq(expected = coloursSolved(), colours(cube))

    @Test
    def theMoveSetsAreTheSizesTheStagesNeed(): Unit \ Assert =
        Assert.assertEq(expected = 24, Vector.length(slots()));
        Assert.assertEq(expected = 36, Vector.length(moves()));
        Assert.assertEq(expected = 24, Vector.length(keeping()));
        Assert.assertEq(expected = 24, Vector.length(pairing()))

    @Test
    def everyMovePermutesTheCentreSlots(): Unit \ Assert =
        foreach (step <- Vector.toList(slotMoves())) {
            Assert.assertEq(expected = 24, Vector.length(step));
            Assert.assertEq(expected = 24, List.length(List.distinct(Vector.toList(step))))
        }

    @Test
    def rankingIsReversible(): Unit \ Assert =
        foreach (r <- 0 :: 1 :: 12375 :: 400000 :: 735470 :: Nil)
            Assert.assertEq(expected = r, rank(unrank(r)))

    ///
    /// Every arrangement of every stage is reachable, and scrambled centres
    /// come back solved.
    ///
    /// Reachability is what makes a stage complete: a state the table never
    /// marked is a state the walk cannot solve. It also catches a move set
    /// that is too small -- with wide turns alone only 2,187 of the first
    /// stage's 735,471 arrangements are reachable, because the outer layer
    /// and the slice beneath it can never be separated.
    ///
    @Test
    def everyStageIsCompleteAndSolvesRealCubes(): Unit \ Assert =
        let t = tables();
        Assert.assertEq(expected = (count(), 0, 8), report(t#separating));
        Assert.assertEq(expected = (axisCount(), 0, 9), report(t#axing));
        Assert.assertEq(expected = (colourCount(), 0, 11), report(t#colouring));
        foreach (text <- scrambles()) {
            match scrambled(text) {
                case None       => Assert.assertEq(expected = "a cube", "unparsed: ${text}")
                case Some(cube) => match solution(t, cube) {
                    case Err(why) => Assert.assertEq(expected = "a solution", "${why}")
                    case Ok(ms)   => match CubeSolvers.Turning.replay(cube, ms) {
                        case Err(why)  => Assert.assertEq(expected = "a replay", "${why}")
                        case Ok(after) =>
                            Assert.assertEq(expected = solved(), separation(after));
                            Assert.assertEq(expected = axisSolved(), axis(after));
                            Assert.assertEq(expected = coloursSolved(), colours(after))
                    }
                }
            }
        }

    ///
    /// A table as its size, how many states it never reached, and its depth.
    ///
    def report(dist: Vector[Int32]): (Int32, Int32, Int32) =
        (Vector.length(dist), Vector.count(d -> d < 0, dist), Vector.foldLeft(Int32.max, 0, dist))

}
