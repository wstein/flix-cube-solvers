mod TestRevengePairs {

    use CubeSolvers.Kind
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Pairs.{steps, firstOrbit, sides, homePlace, relative, isPaired, isSeparated}

    def scrambled(text: String): Option[Facelets] =
        CubeSolvers.parse(text)
            |> Result.toOption
            |> Option.flatMap(ms ->
                CubeSolvers.Turning.replay(CubeSolvers.Solver.solved(Kind.Cube4x4), ms) |> Result.toOption)

    ///
    /// Under all 36 moves a wing reaches all 24 places; under the third
    /// phase's twenty it reaches twelve. That split is what lets twenty-four
    /// wings be read as a permutation of twelve places, and the whole edge
    /// coordinate rests on it.
    ///
    @Test
    def theThirdPhaseSplitsTheWingsInTwo(): Unit \ Assert =
        Assert.assertEq(expected = 20, Vector.length(steps()));
        Assert.assertEq(expected = 12, Set.size(firstOrbit()))

    ///
    /// Each place holds one wing from each orbit, which is what makes the
    /// split a pairing rather than a partition.
    ///
    @Test
    def everyPlaceHoldsOneWingFromEachOrbit(): Unit \ Assert =
        let first = firstOrbit();
        Assert.assertEq(expected = 12, Vector.length(sides()));
        foreach ((x, y) <- Vector.toList(sides()))
            Assert.assertEq(expected = true, Set.memberOf(x, first) and not Set.memberOf(y, first))

    @Test
    def everyWingKnowsItsPlace(): Unit \ Assert =
        let home = homePlace();
        Assert.assertEq(expected = 24, Vector.length(home));
        Assert.assertEq(expected = false, Vector.memberOf(-1, home));
        foreach (p <- List.range(0, 12))
            Assert.assertEq(expected = 2, Vector.count(h -> h == p, home))

    @Test
    def aSolvedCubeIsPairedAndSeparated(): Unit \ Assert =
        let cube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        Assert.assertEq(expected = Vector.range(0, 12), relative(cube));
        Assert.assertEq(expected = true, isPaired(cube));
        Assert.assertEq(expected = true, isSeparated(cube))

    ///
    /// Outer turns move whole pairs, so a cube scrambled with nothing else is
    /// still paired -- and the coordinate must say so.
    ///
    @Test
    def outerTurnsAloneLeaveTheCubePaired(): Unit \ Assert =
        match scrambled("R U R' U' F2 D L2 B") {
            case None       => Assert.assertEq(expected = "a cube", "unparsed")
            case Some(cube) =>
                Assert.assertEq(expected = true, isSeparated(cube));
                Assert.assertEq(expected = true, isPaired(cube))
        }

    ///
    /// A wide turn takes a wing out of its pair, and the coordinate stops
    /// being the identity.
    ///
    @Test
    def aWideTurnBreaksThePairing(): Unit \ Assert =
        match scrambled("Rw") {
            case None       => Assert.assertEq(expected = "a cube", "unparsed")
            case Some(cube) => Assert.assertEq(expected = false, isPaired(cube))
        }

}
