mod TestCube {

    use Cube.{Face, Kind, Turn, move}

    @Test
    def renderMoveWritesAQuarterTurnAsTheBareFace(): Unit \ Assert =
        Assert.assertEq(expected = "U", Cube.renderMove(move(Face.U, 1, Turn.Cw)))

    @Test
    def renderMoveMarksAHalfTurnAndItsInverse(): Unit \ Assert =
        Assert.assertEq(expected = "R2", Cube.renderMove(move(Face.R, 1, Turn.Half)));
        Assert.assertEq(expected = "R'", Cube.renderMove(move(Face.R, 1, Turn.Ccw)))

    ///
    /// Two layers are written `w`, three or more carry the count as well --
    /// `Uw`, then `3Uw`. There is no `2Uw`.
    ///
    @Test
    def renderMoveWritesWideTurnsInStandardNotation(): Unit \ Assert =
        Assert.assertEq(expected = "Uw", Cube.renderMove(move(Face.U, 2, Turn.Cw)));
        Assert.assertEq(expected = "3Rw'", Cube.renderMove(move(Face.R, 3, Turn.Ccw)))

    @Test
    def inverseReversesTheOrderAndEachTurn(): Unit \ Assert =
        let ms = move(Face.R, 1, Turn.Cw) :: move(Face.U, 1, Turn.Half) :: Nil;
        Assert.assertEq(expected = "U2 R'", Cube.render(Cube.inverse(ms)))

    @Test
    def inverseTwiceIsTheOriginal(): Unit \ Assert =
        let ms = move(Face.F, 2, Turn.Ccw) :: move(Face.D, 1, Turn.Cw) :: Nil;
        Assert.assertEq(expected = ms, Cube.inverse(Cube.inverse(ms)))

    ///
    /// A turn that reaches the far face rotates the whole cube, which is not a
    /// move: `Uw` is legal on a 3x3, `3Uw` is not.
    ///
    @Test
    def validateRejectsADepthThatTurnsTheWholeCube(): Unit \ Assert =
        Assert.assertEq(expected = true, Result.isOk(Cube.validate(Kind.Cube3x3, move(Face.U, 2, Turn.Cw))));
        Assert.assertEq(expected = true, Result.isErr(Cube.validate(Kind.Cube3x3, move(Face.U, 3, Turn.Cw))));
        Assert.assertEq(expected = true, Result.isOk(Cube.validate(Kind.Cube5x5, move(Face.U, 3, Turn.Cw))))

    @Test
    def validateRejectsANonPositiveDepth(): Unit \ Assert =
        Assert.assertEq(expected = true, Result.isErr(Cube.validate(Kind.Cube3x3, move(Face.U, 0, Turn.Cw))))

    @Test
    def parseReadsWhatRenderWrites(): Unit \ Assert =
        Assert.assertEq(expected = Ok(move(Face.U, 1, Turn.Cw)), Cube.parseMove("U"));
        Assert.assertEq(expected = Ok(move(Face.R, 1, Turn.Half)), Cube.parseMove("R2"));
        Assert.assertEq(expected = Ok(move(Face.F, 1, Turn.Ccw)), Cube.parseMove("F'"));
        Assert.assertEq(expected = Ok(move(Face.U, 2, Turn.Cw)), Cube.parseMove("Uw"));
        Assert.assertEq(expected = Ok(move(Face.R, 3, Turn.Ccw)), Cube.parseMove("3Rw'"))

    ///
    /// The round trip over every move a 5x5 can make.
    ///
    @Test
    def everyMoveSurvivesBeingWrittenDown(): Unit \ Assert =
        let faces = Face.U :: Face.R :: Face.F :: Face.D :: Face.L :: Face.B :: Nil;
        let turns = Turn.Cw :: Turn.Half :: Turn.Ccw :: Nil;
        foreach (f <- faces; d <- List.range(1, 4); t <- turns) {
            let m = move(f, d, t);
            Assert.assertEq(expected = Ok(m), Cube.parseMove(Cube.renderMove(m)))
        }

    ///
    /// A depth with no `w` is refused rather than guessed at: `3R` means the
    /// third slice alone in some notations and three layers in others.
    ///
    @Test
    def aDepthWithoutAWideMarkerIsRefused(): Unit \ Assert =
        Assert.assertEq(expected = true, Result.isErr(Cube.parseMove("3R")));
        Assert.assertEq(expected = true, Result.isErr(Cube.parseMove("1Rw")));
        Assert.assertEq(expected = true, Result.isErr(Cube.parseMove("X")));
        Assert.assertEq(expected = true, Result.isErr(Cube.parseMove("R3")));
        Assert.assertEq(expected = true, Result.isErr(Cube.parseMove("")))

    @Test
    def aSequenceIsWhitespaceSeparated(): Unit \ Assert =
        Assert.assertEq(expected = Ok(Nil), Cube.parse(""));
        Assert.assertEq(
            expected = Ok(move(Face.R, 1, Turn.Cw) :: move(Face.U, 2, Turn.Half) :: Nil),
            Cube.parse("R Uw2"));
        Assert.assertEq(expected = true, Result.isErr(Cube.parse("R nonsense U")))

    @Test
    def simplifyMergesConsecutiveTurnsOfOneLayer(): Unit \ Assert =
        Assert.assertEq(expected = "F'", render(Cube.simplify(parseAll("F F2"))));
        Assert.assertEq(expected = "", render(Cube.simplify(parseAll("U U'"))));
        Assert.assertEq(expected = "", render(Cube.simplify(parseAll("R2 R2"))));
        Assert.assertEq(expected = "U R", render(Cube.simplify(parseAll("U R"))))

    ///
    /// Removing a pair can bring two more together, so one pass is not enough.
    ///
    @Test
    def simplifyWorksToAFixedPoint(): Unit \ Assert =
        Assert.assertEq(expected = "U2", render(Cube.simplify(parseAll("U R R' U"))));
        Assert.assertEq(expected = "", render(Cube.simplify(parseAll("U R R' U'"))))

    ///
    /// A wide turn is a different layer from the outer one, so they do not
    /// merge.
    ///
    @Test
    def simplifyKeepsDifferentDepthsApart(): Unit \ Assert =
        Assert.assertEq(expected = "U Uw", render(Cube.simplify(parseAll("U Uw"))))

    ///
    /// The sequence `text` names, or nothing.
    ///
    def parseAll(text: String): Cube.MoveSequence =
        Cube.parse(text) |> Result.getWithDefault(Nil)

    ///
    /// The notation for a sequence.
    ///
    def render(ms: Cube.MoveSequence): String = Cube.render(ms)

    @Test
    def faceletCountFollowsTheOrder(): Unit \ Assert =
        Assert.assertEq(expected = 24, Cube.faceletCount(Kind.Cube2x2));
        Assert.assertEq(expected = 54, Cube.faceletCount(Kind.Cube3x3));
        Assert.assertEq(expected = 150, Cube.faceletCount(Kind.Cube5x5))

    @Test
    def kindOfOrderKnowsOnlyTheModelledSizes(): Unit \ Assert =
        Assert.assertEq(expected = Some(Kind.Cube4x4), Cube.kindOfOrder(4));
        Assert.assertEq(expected = None, Cube.kindOfOrder(6))

    @Test
    def turnOfQuarterTurnsHasNoIdentity(): Unit \ Assert =
        Assert.assertEq(expected = Some(Turn.Ccw), Cube.turnOfQuarterTurns(3));
        Assert.assertEq(expected = None, Cube.turnOfQuarterTurns(4))

}
