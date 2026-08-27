mod TestRevengeCenters {

    use CubeSolvers.Kind
    use CubeSolvers.Revenge.Centers.{count, slots, moves, slotMoves, separation, solved, rank, unrank, table}

    @Test
    def aSolvedCubeHasItsCentresSeparated(): Unit \ Assert =
        Assert.assertEq(expected = solved(), separation(CubeSolvers.Solver.solved(Kind.Cube4x4)))

    @Test
    def thereAreTwentyFourCentresAndThirtySixMoves(): Unit \ Assert =
        Assert.assertEq(expected = 24, Vector.length(slots()));
        Assert.assertEq(expected = 36, Vector.length(moves()))

    ///
    /// Every move must shuffle the centre slots among themselves; a move that
    /// lost or doubled one would corrupt every table built on it.
    ///
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
    /// Every arrangement is reachable and none is more than eight moves away.
    ///
    /// This is what makes the stage complete: a search that cannot reach a
    /// state cannot solve it. It also catches a move set that is too small --
    /// with wide turns alone only 2,187 of these are reachable, because the
    /// outer layer and the slice beneath it can never be separated.
    ///
    @Test
    def everyArrangementIsReachableWithinEightMoves(): Unit \ Assert =
        let tbl = table();
        Assert.assertEq(expected = count(), Vector.length(tbl));
        Assert.assertEq(expected = 0, Vector.count(d -> d < 0, tbl));
        Assert.assertEq(expected = 8, Vector.foldLeft(Int32.max, 0, tbl))

}
