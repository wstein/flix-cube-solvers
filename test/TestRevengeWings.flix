mod TestRevengeWings {

    use CubeSolvers.Kind
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Wings.{slots, pieces, places, cubie, paired, isReduced}

    def scrambled(text: String): Option[Facelets] =
        CubeSolvers.parse(text)
            |> Result.toOption
            |> Option.flatMap(ms ->
                CubeSolvers.Turning.replay(CubeSolvers.Solver.solved(Kind.Cube4x4), ms) |> Result.toOption)

    @Test
    def aCubeHasFortyEightWingStickersInTwentyFourWingsInTwelvePlaces(): Unit \ Assert =
        Assert.assertEq(expected = 48, Vector.length(slots()));
        Assert.assertEq(expected = 24, Vector.length(pieces()));
        Assert.assertEq(expected = 12, Vector.length(places()))

    ///
    /// The two stickers of a wing must sit on one cubie -- that is what makes
    /// them one piece, and it is derived rather than tabulated.
    ///
    @Test
    def eachWingsStickersShareACubie(): Unit \ Assert =
        foreach ((a, b) <- Vector.toList(pieces())) {
            Assert.assertEq(expected = cubie(a), cubie(b));
            Assert.assertEq(expected = false, a == b)
        }

    @Test
    def everyStickerBelongsToExactlyOneWing(): Unit \ Assert =
        let used = pieces() |> Vector.toList |> List.flatMap(match (a, b) -> a :: b :: Nil);
        Assert.assertEq(expected = 48, List.length(used));
        Assert.assertEq(expected = 48, List.length(List.distinct(used)));
        Assert.assertEq(expected = List.sort(Vector.toList(slots())), List.sort(used))

    @Test
    def aSolvedCubeIsAlreadyReduced(): Unit \ Assert =
        let cube = CubeSolvers.Solver.solved(Kind.Cube4x4);
        Assert.assertEq(expected = 12, paired(cube));
        Assert.assertEq(expected = true, isReduced(cube))

    ///
    /// A scramble takes the pairs apart, which is the work reduction has to
    /// undo.
    ///
    @Test
    def aScrambledCubeIsNotReduced(): Unit \ Assert =
        foreach (text <- "D2 F' Lw2 R2 Dw2 Fw B' L' Bw' Dw2 Lw2 Bw U' R2 D"
                      :: "Fw R Dw2 Rw2 B Lw R Bw2 F' Bw Fw U2 Rw' L2 F2" :: Nil) {
            match scrambled(text) {
                case None       => Assert.assertEq(expected = "a cube", "unparsed: ${text}")
                case Some(cube) =>
                    Assert.assertEq(expected = false, isReduced(cube));
                    Assert.assertEq(expected = true, paired(cube) < 12)
            }
        }

    ///
    /// Turning the outer layers can never pair or unpair anything: both wings
    /// of a place always travel together.
    ///
    @Test
    def outerTurnsLeaveThePairingAlone(): Unit \ Assert =
        match scrambled("Fw R Dw2 Rw2 B Lw R Bw2 F' Bw Fw U2 Rw' L2 F2") {
            case None       => Assert.assertEq(expected = "a cube", "unparsed")
            case Some(cube) =>
                let before = paired(cube);
                foreach (m <- Vector.toList(CubeSolvers.Rubik.moves())) {
                    match CubeSolvers.Turning.apply(cube, m) {
                        case Err(_)    => Assert.assertEq(expected = "a turn", "refused")
                        case Ok(after) => Assert.assertEq(expected = before, paired(after))
                    }
                }
        }

}
