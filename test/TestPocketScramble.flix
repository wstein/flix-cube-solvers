mod TestPocketScramble {

    use CubeSolvers.Pocket.{replay, solved}
    use CubeSolvers.Pocket.Engine.standard
    use CubeSolvers.Pocket.Scramble.{sample, wca, atDistance, easy, secureEasy}
    use CubeSolvers.Pocket.Stickers.ofFacelets
    use CubeSolvers.Solver.{defaultOptions, search, Outcome}

    @Test
    def aSampleKeepsThePinnedCornerAndTwistLaw(): Unit \ Assert =
        let (state, _) = sample(42i64);
        Assert.assertEq(expected = 6, Vector.get(6, CubeSolvers.Pocket.permutation(state)));
        Assert.assertEq(expected = 0, Vector.get(6, CubeSolvers.Pocket.twist(state)));
        Assert.assertEq(expected = 0,
            CubeSolvers.Pocket.twist(state)
                |> Vector.foldLeft((sum, t) -> sum + t, 0)
                |> (sum -> Int32.modulo(sum, 3)))

    @Test
    def aWcaScrambleIsShortestAndNeverTooEasy(): Unit \ Assert =
        let engine = standard();
        let (scramble, _) = wca(engine, 42i64);
        match replay(solved(), scramble) {
            case None => Assert.assertEq(expected = "a state", "none")
            case Some(state) => match search(engine, CubeSolvers.Pocket.Stickers.toFacelets(state), defaultOptions()) {
                case Outcome.Solved(home) =>
                    Assert.assertEq(expected = List.length(scramble), List.length(home));
                    Assert.assertEq(expected = true, List.length(scramble) >= 4);
                    Assert.assertEq(expected = Some(solved()), replay(state, home))
                case other => Assert.assertEq(expected = "Solved", "${other}")
            }
        }

    @Test
    def requestedDifficultyIsTheProvenMinimum(): Unit \ Assert =
        let engine = standard();
        let (scramble, _) = atDistance(42i64, 3);
        Assert.assertEq(expected = 3, List.length(scramble));
        match replay(solved(), scramble) {
            case None => Assert.assertEq(expected = "a state", "none")
            case Some(state) => match search(engine, CubeSolvers.Pocket.Stickers.toFacelets(state), defaultOptions()) {
                case Outcome.Solved(home) => Assert.assertEq(expected = 3, List.length(home))
                case other => Assert.assertEq(expected = "Solved", "${other}")
            }
        }

    @Test
    def easyOnlyReturnsThreeToFiveMoveScrambles(): Unit \ Assert =
        let (scramble, _) = easy(42i64);
        Assert.assertEq(expected = true, List.length(scramble) >= 3 and List.length(scramble) <= 5)

    @Test
    def secureEasyHasAProvenEasyLength(): Unit \ {Assert, IO} =
        let scramble = secureEasy();
        Assert.assertEq(expected = true, List.length(scramble) >= 3 and List.length(scramble) <= 5)

}
