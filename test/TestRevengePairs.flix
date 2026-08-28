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

    ///
    /// The cached orbit maps are the third phase's coordinate transition.
    /// Check every move against independently reading the resulting cube so
    /// the search cannot silently return to the old moving-frame error.
    ///
    @Test
    def orbitMapsCarryTheAbsolutePairing(): Unit \ Assert =
        let cube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        let maps = CubeSolvers.Revenge.Pairs.orbitMaps();
        let before = relative(cube);
        foreach ((i, m) <- List.mapWithIndex(i -> m -> (i, m), referenceOrder())) {
            match CubeSolvers.Turning.apply(cube, m) {
                case Err(_) => Assert.assertEq(expected = "a turn", "refused")
                case Ok(after) => Assert.assertEq(
                    expected = relative(after),
                    CubeSolvers.Revenge.Pairs.turnedBy(Vector.get(i, maps), before))
            }
        }

    ///
    /// The two numberings agree on every move of the phase.
    ///
    /// This is the only check worth making. Fourteen of the twenty moves are
    /// outer turns, which move whole pairs and leave the pairing alone, so
    /// they agree under any numbering whatsoever -- "fourteen of twenty" was
    /// a false positive that nearly sent a table into use with the wrong
    /// index. Only the six wide turns carry information, and all six must
    /// agree.
    ///
    @Test
    def theNumberingsAgreeOnEveryMove(): Unit \ Assert =
        let cube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        let tau = CubeSolvers.Revenge.Pairs.placeCorrespondence();
        Assert.assertEq(expected = 12, List.length(List.distinct(Vector.toList(tau))));
        Assert.assertEq(expected = false, Vector.memberOf(-1, tau));
        foreach ((i, m) <- List.mapWithIndex(i -> m -> (i, m), referenceOrder())) {
            match CubeSolvers.Turning.apply(cube, m) {
                case Err(_)    => Assert.assertEq(expected = "a turn", "refused")
                case Ok(after) =>
                    let ours = CubeSolvers.Revenge.Pairs.asEdgeState(after);
                    let theirs = CubeSolvers.Revenge.Edges.standard(
                        CubeSolvers.Revenge.Edges.move(CubeSolvers.Revenge.Edges.solved(), i));
                    Assert.assertEq(expected = theirs#edge, ours#edge)
            }
        }

    ///
    /// The phase's moves in the order the ported tables number them.
    ///
    def referenceOrder(): List[CubeSolvers.Move] =
        let m = CubeSolvers.move;
        let q = CubeSolvers.Turn.Cw;
        let h = CubeSolvers.Turn.Half;
        let c = CubeSolvers.Turn.Ccw;
        let u = CubeSolvers.Face.U;
        let r = CubeSolvers.Face.R;
        let f = CubeSolvers.Face.F;
        let d = CubeSolvers.Face.D;
        let l = CubeSolvers.Face.L;
        let b = CubeSolvers.Face.B;
        m(u, 1, q) :: m(u, 1, h) :: m(u, 1, c) :: m(r, 1, h) ::
        m(f, 1, q) :: m(f, 1, h) :: m(f, 1, c) ::
        m(d, 1, q) :: m(d, 1, h) :: m(d, 1, c) :: m(l, 1, h) ::
        m(b, 1, q) :: m(b, 1, h) :: m(b, 1, c) ::
        m(u, 2, h) :: m(r, 2, h) :: m(f, 2, h) :: m(d, 2, h) :: m(l, 2, h) :: m(b, 2, h) :: Nil

}
