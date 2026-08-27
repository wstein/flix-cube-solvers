mod TestCube {

    use CubeSolvers.{Face, Kind, Turn, move}

    @Test
    def renderMoveWritesAQuarterTurnAsTheBareFace(): Unit \ Assert =
        Assert.assertEq(expected = "U", CubeSolvers.renderMove(move(Face.U, 1, Turn.Cw)))

    @Test
    def renderMoveMarksAHalfTurnAndItsInverse(): Unit \ Assert =
        Assert.assertEq(expected = "R2", CubeSolvers.renderMove(move(Face.R, 1, Turn.Half)));
        Assert.assertEq(expected = "R'", CubeSolvers.renderMove(move(Face.R, 1, Turn.Ccw)))

    ///
    /// Two layers are written `w`, three or more carry the count as well --
    /// `Uw`, then `3Uw`. There is no `2Uw`.
    ///
    @Test
    def renderMoveWritesWideTurnsInStandardNotation(): Unit \ Assert =
        Assert.assertEq(expected = "Uw", CubeSolvers.renderMove(move(Face.U, 2, Turn.Cw)));
        Assert.assertEq(expected = "3Rw'", CubeSolvers.renderMove(move(Face.R, 3, Turn.Ccw)))

    @Test
    def inverseReversesTheOrderAndEachTurn(): Unit \ Assert =
        let ms = move(Face.R, 1, Turn.Cw) :: move(Face.U, 1, Turn.Half) :: Nil;
        Assert.assertEq(expected = "U2 R'", CubeSolvers.render(CubeSolvers.inverse(ms)))

    @Test
    def inverseTwiceIsTheOriginal(): Unit \ Assert =
        let ms = move(Face.F, 2, Turn.Ccw) :: move(Face.D, 1, Turn.Cw) :: Nil;
        Assert.assertEq(expected = ms, CubeSolvers.inverse(CubeSolvers.inverse(ms)))

    ///
    /// A turn that reaches the far face rotates the whole cube, which is not a
    /// move: `Uw` is legal on a 3x3, `3Uw` is not.
    ///
    @Test
    def validateRejectsADepthThatTurnsTheWholeCube(): Unit \ Assert =
        Assert.assertEq(expected = true, Result.isOk(CubeSolvers.validate(Kind.Cube3x3, move(Face.U, 2, Turn.Cw))));
        Assert.assertEq(expected = true, Result.isErr(CubeSolvers.validate(Kind.Cube3x3, move(Face.U, 3, Turn.Cw))));
        Assert.assertEq(expected = true, Result.isOk(CubeSolvers.validate(Kind.Cube5x5, move(Face.U, 3, Turn.Cw))))

    @Test
    def validateRejectsANonPositiveDepth(): Unit \ Assert =
        Assert.assertEq(expected = true, Result.isErr(CubeSolvers.validate(Kind.Cube3x3, move(Face.U, 0, Turn.Cw))))

    @Test
    def parseReadsWhatRenderWrites(): Unit \ Assert =
        Assert.assertEq(expected = Ok(move(Face.U, 1, Turn.Cw)), CubeSolvers.parseMove("U"));
        Assert.assertEq(expected = Ok(move(Face.R, 1, Turn.Half)), CubeSolvers.parseMove("R2"));
        Assert.assertEq(expected = Ok(move(Face.F, 1, Turn.Ccw)), CubeSolvers.parseMove("F'"));
        Assert.assertEq(expected = Ok(move(Face.U, 2, Turn.Cw)), CubeSolvers.parseMove("Uw"));
        Assert.assertEq(expected = Ok(move(Face.R, 3, Turn.Ccw)), CubeSolvers.parseMove("3Rw'"))

    ///
    /// The round trip over every move a 5x5 can make.
    ///
    @Test
    def everyMoveSurvivesBeingWrittenDown(): Unit \ Assert =
        let faces = Face.U :: Face.R :: Face.F :: Face.D :: Face.L :: Face.B :: Nil;
        let turns = Turn.Cw :: Turn.Half :: Turn.Ccw :: Nil;
        foreach (f <- faces; d <- List.range(1, 4); t <- turns) {
            let m = move(f, d, t);
            Assert.assertEq(expected = Ok(m), CubeSolvers.parseMove(CubeSolvers.renderMove(m)))
        }

    ///
    /// A depth with no `w` is refused rather than guessed at: `3R` means the
    /// third slice alone in some notations and three layers in others.
    ///
    @Test
    def aDepthWithoutAWideMarkerIsRefused(): Unit \ Assert =
        Assert.assertEq(expected = true, Result.isErr(CubeSolvers.parseMove("3R")));
        Assert.assertEq(expected = true, Result.isErr(CubeSolvers.parseMove("1Rw")));
        Assert.assertEq(expected = true, Result.isErr(CubeSolvers.parseMove("X")));
        Assert.assertEq(expected = true, Result.isErr(CubeSolvers.parseMove("R3")));
        Assert.assertEq(expected = true, Result.isErr(CubeSolvers.parseMove("")))

    @Test
    def aSequenceIsWhitespaceSeparated(): Unit \ Assert =
        Assert.assertEq(expected = Ok(Nil), CubeSolvers.parse(""));
        Assert.assertEq(
            expected = Ok(move(Face.R, 1, Turn.Cw) :: move(Face.U, 2, Turn.Half) :: Nil),
            CubeSolvers.parse("R Uw2"));
        Assert.assertEq(expected = true, Result.isErr(CubeSolvers.parse("R nonsense U")))

    @Test
    def simplifyMergesConsecutiveTurnsOfOneLayer(): Unit \ Assert =
        Assert.assertEq(expected = "F'", render(CubeSolvers.simplify(parseAll("F F2"))));
        Assert.assertEq(expected = "", render(CubeSolvers.simplify(parseAll("U U'"))));
        Assert.assertEq(expected = "", render(CubeSolvers.simplify(parseAll("R2 R2"))));
        Assert.assertEq(expected = "U R", render(CubeSolvers.simplify(parseAll("U R"))))

    ///
    /// Removing a pair can bring two more together, so one pass is not enough.
    ///
    @Test
    def simplifyWorksToAFixedPoint(): Unit \ Assert =
        Assert.assertEq(expected = "U2", render(CubeSolvers.simplify(parseAll("U R R' U"))));
        Assert.assertEq(expected = "", render(CubeSolvers.simplify(parseAll("U R R' U'"))))

    ///
    /// A wide turn is a different layer from the outer one, so they do not
    /// merge.
    ///
    @Test
    def simplifyKeepsDifferentDepthsApart(): Unit \ Assert =
        Assert.assertEq(expected = "U Uw", render(CubeSolvers.simplify(parseAll("U Uw"))))

    ///
    /// The sequence `text` names, or nothing.
    ///
    def parseAll(text: String): CubeSolvers.MoveSequence =
        CubeSolvers.parse(text) |> Result.getWithDefault(Nil)

    ///
    /// The notation for a sequence.
    ///
    def render(ms: CubeSolvers.MoveSequence): String = CubeSolvers.render(ms)

    @Test
    def faceletCountFollowsTheOrder(): Unit \ Assert =
        Assert.assertEq(expected = 24, CubeSolvers.faceletCount(Kind.Cube2x2));
        Assert.assertEq(expected = 54, CubeSolvers.faceletCount(Kind.Cube3x3));
        Assert.assertEq(expected = 150, CubeSolvers.faceletCount(Kind.Cube5x5))

    @Test
    def kindOfOrderKnowsOnlyTheModelledSizes(): Unit \ Assert =
        Assert.assertEq(expected = Some(Kind.Cube4x4), CubeSolvers.kindOfOrder(4));
        Assert.assertEq(expected = None, CubeSolvers.kindOfOrder(6))

    @Test
    def turnOfQuarterTurnsHasNoIdentity(): Unit \ Assert =
        Assert.assertEq(expected = Some(Turn.Ccw), CubeSolvers.turnOfQuarterTurns(3));
        Assert.assertEq(expected = None, CubeSolvers.turnOfQuarterTurns(4))

}
