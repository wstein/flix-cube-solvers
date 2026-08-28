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

    ///
    /// Every answer the enumerator gives is a shortest one, and lands on the
    /// goal. One answer is not enough for the phases that follow -- they sift
    /// these for a rare property, and answers sharing a prefix are not
    /// independent tries -- so the enumerator has to be right about all of
    /// them, not just the first.
    ///
    @Test
    def everyEnumeratedAnswerIsAShortestOne(): Unit \ Assert =
        let dist = CubeSolvers.Revenge.Centers.table();
        let cube = CubeSolvers.parse("Rw2 B2 Uw2 Fw2 L2 D Uw2 R2")
            |> Result.toOption
            |> Option.flatMap(ms ->
                CubeSolvers.Turning.replay(
                    CubeSolvers.Solver.solved(CubeSolvers.Kind.Cube4x4), ms) |> Result.toOption);
        match cube {
            case None => Assert.assertEq(expected = "a cube", "no cube")
            case Some(c) =>
                let shortest = Vector.get(CubeSolvers.Revenge.Centers.separation(c), dist);
                let answers = CubeSolvers.Revenge.Walk.manyWith(c, dist,
                    CubeSolvers.Revenge.Centers.separation, CubeSolvers.Revenge.Centers.moves(), 8);
                Assert.assertEq(expected = true,
                    List.length(answers) >= 1 and List.length(answers) <= 8);
                foreach ((ms, leaf) <- answers) {
                    Assert.assertEq(expected = shortest, List.length(ms));
                    Assert.assertEq(expected = 0,
                        Vector.get(CubeSolvers.Revenge.Centers.separation(leaf), dist));
                    match CubeSolvers.Turning.replay(c, ms) {
                        case Ok(walked) => Assert.assertEq(expected = leaf, walked)
                        case Err(_)     => Assert.assertEq(expected = "a replay", "a failure")
                    }
                }
        }

}
