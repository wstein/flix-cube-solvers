mod TestRevengeCenters {

    use CubeSolvers.Kind
    use CubeSolvers.Revenge.Centers.{slots, moves, slotMoves, separation, solved, rank, unrank}

    @Test
    def aSolvedCubeHasItsRAndLColoursSeparated(): Unit \ Assert =
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


}
