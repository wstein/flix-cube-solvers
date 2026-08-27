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
