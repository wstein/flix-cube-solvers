mod TestPocketScramble {

    use CubeSolvers.Pocket.{replay, solved}
    use CubeSolvers.Pocket.Engine.standard
    use CubeSolvers.Pocket.Scramble.{sample, wca, atDistance, easy, secureEasy}
    use CubeSolvers.Pocket.Stickers.ofFacelets
    use CubeSolvers.Solver.{defaultOptions, search, Outcome}

    ///
    /// Every corner must appear exactly once.
    ///
    /// The sampler shuffled the wrong seven corners once, dealing the pinned
    /// one twice and corner seven never. Nothing downstream could decode the
    /// result, and the scrambler that retried until it could never returned.
    ///
    @Test
    def aSampleDealsEachCornerExactlyOnce(): Unit \ Assert =
        foreach (seed <- 42i64 :: 1i64 :: 7i64 :: 12345i64 :: 999i64 :: Nil) {
            let (state, _) = sample(seed);
            let corners = CubeSolvers.Pocket.permutation(state) |> Vector.toList;
            Assert.assertEq(expected = 8, List.length(corners));
            Assert.assertEq(expected = 8, List.length(List.distinct(corners)));
            Assert.assertEq(expected = List.range(0, 8), List.sort(corners));
            Assert.assertEq(expected = true,
                Result.isOk(ofFacelets(CubeSolvers.Pocket.Stickers.toFacelets(state))))
        }

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
        let (scramble, _) = atDistance(42i64, 3) |> Result.getWithDefault((Nil, 0i64));
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
        let (scramble, _) = easy(42i64) |> Result.getWithDefault((Nil, 0i64));
        Assert.assertEq(expected = true, List.length(scramble) >= 3 and List.length(scramble) <= 5)

    @Test
    def secureEasyHasAProvenEasyLength(): Unit \ {Assert, IO} =
        let scramble = secureEasy();
        Assert.assertEq(expected = true, List.length(scramble) >= 3 and List.length(scramble) <= 5)

}
