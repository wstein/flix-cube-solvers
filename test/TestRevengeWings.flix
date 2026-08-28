mod TestRevengeWings {

    use CubeSolvers.Kind
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Wings.{slots, pieces, places, cubie, paired, isReduced, identity, parity}

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
    /// A wing is told from its mirror by the order of its faces, so on a
    /// solved cube every wing must report itself.
    ///
    @Test
    def aSolvedCubeReportsEveryWingAtHome(): Unit \ Assert =
        Assert.assertEq(expected = Vector.range(0, 24), identity(CubeSolvers.Solver.solved(Kind.Cube4x4)));
        Assert.assertEq(expected = 0, parity(CubeSolvers.Solver.solved(Kind.Cube4x4)))

    ///
    /// Whatever the scramble, every wing must be found exactly once. A cube
    /// that reported one twice would mean the mirror rule had picked the
    /// wrong end of an edge.
    ///
    @Test
    def everyScrambleIdentifiesEachWingOnce(): Unit \ Assert =
        foreach (text <- "D2 F' Lw2 R2 Dw2 Fw B' L' Bw' Dw2 Lw2 Bw U' R2 D"
                      :: "L' R U2 F2 U' R' B Lw B2 Rw2 Fw' Dw2 Fw2 R Lw2"
                      :: "Fw R Dw2 Rw2 B Lw R Bw2 F' Bw Fw U2 Rw' L2 F2" :: Nil) {
            match scrambled(text) {
                case None       => Assert.assertEq(expected = "a cube", "unparsed: ${text}")
                case Some(cube) =>
                    let seen = identity(cube);
                    Assert.assertEq(expected = false, Vector.memberOf(-1, seen));
                    Assert.assertEq(expected = 24, List.length(List.distinct(Vector.toList(seen))))
            }
        }

    ///
    /// Parity is a coordinate the second phase carries, so it has to move the
    /// way the moves do: a turn changes it by exactly its own permutation's
    /// parity, every time.
    ///
    @Test
    def parityFollowsTheMovesThatChangeIt(): Unit \ Assert =
        match scrambled("Fw R Dw2 Rw2 B Lw R Bw2 F' Bw Fw U2 Rw' L2 F2") {
            case None       => Assert.assertEq(expected = "a cube", "unparsed")
            case Some(cube) =>
                let before = parity(cube);
                foreach (m <- Vector.toList(CubeSolvers.Revenge.Centers.moves())) {
                    match CubeSolvers.Turning.apply(cube, m) {
                        case Err(_)    => Assert.assertEq(expected = "a turn", "refused")
                        case Ok(after) =>
                            let moved = CubeSolvers.Revenge.Wings.permutationParity(
                                turnOfWings(m));
                            Assert.assertEq(expected = (before + moved) `Int32.modulo` 2, parity(after))
                    }
                }
        }

    ///
    /// How a move permutes the wings, as wing numbers.
    ///
    def turnOfWings(m: CubeSolvers.Move): Vector[Int32] =
        CubeSolvers.Revenge.Wings.identity(
            CubeSolvers.Turning.apply(CubeSolvers.Solver.solved(Kind.Cube4x4), m)
                |> Result.getWithDefault(CubeSolvers.Solver.solved(Kind.Cube4x4)))

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
