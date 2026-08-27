mod TestPocketStickers {

    use CubeSolvers.{Face, Kind, Turn, move}
    use CubeSolvers.Pocket.{State, solved, replay}
    use CubeSolvers.Pocket.Stickers.{toFacelets, ofFacelets}
    use CubeSolvers.Solver.{Facelets, Invalid, facelets, stickersOf}

    ///
    /// The sticker string a state shows, as letters.
    ///
    def render(f: Facelets): String =
        stickersOf(f) |> Vector.toList |> List.map(s -> Char.toString(CubeSolvers.faceToken(s))) |> List.join("")

    @Test
    def aSolvedCubeShowsEachFaceItsOwnColour(): Unit \ Assert =
        Assert.assertEq(expected = "UUUURRRRFFFFDDDDLLLLBBBB", render(toFacelets(solved())))

    ///
    /// Stickers written out of cubies are always countable, whatever the
    /// state: the counts cannot go wrong when the pieces are what produced
    /// them.
    ///
    @Test
    def writtenStickersAlwaysCount(): Unit \ Assert =
        foreach (s <- statesWithin(3))
            Assert.assertEq(expected = true, Result.isOk(facelets(Kind.Cube2x2, stickersOf(toFacelets(s)))))

    ///
    /// The round trip is the real test of the two tables: every state within
    /// three moves survives being written to stickers and read back.
    ///
    @Test
    def everyStateSurvivesTheRoundTrip(): Unit \ Assert =
        foreach (s <- statesWithin(3))
            Assert.assertEq(expected = Ok(s), ofFacelets(toFacelets(s)))

    ///
    /// A single corner rotated in place: every colour appears four times,
    /// every corner is a real corner, and no cube on earth looks like this.
    ///
    @Test
    def aLoneTwistedCornerIsUnreachable(): Unit \ Assert =
        let twisted = State.State(Vector.range(0, 8), Vector#{1, 0, 0, 0, 0, 0, 0, 0});
        Assert.assertEq(
            expected = Err(Invalid.Unsolvable("the corner twists do not sum to a whole turn")),
            ofFacelets(toFacelets(twisted)))

    ///
    /// Three stickers meeting at a corner with no U or D among them are not a
    /// corner of any cube.
    ///
    @Test
    def acornerWithoutAnUpOrDownStickerIsNoCorner(): Unit \ Assert =
        let broken = Vector.mapWithIndex(i -> s -> if (i == 3) Face.R else s, stickersOf(toFacelets(solved())));
        Assert.assertEq(
            expected = Err(Invalid.NotAPiece(0)),
            ofFacelets(Facelets.Checked(Kind.Cube2x2, broken)))

    @Test
    def aDifferentPuzzleIsRejectedByKind(): Unit \ Assert =
        Assert.assertEq(
            expected = Err(Invalid.WrongKind(Kind.Cube3x3)),
            ofFacelets(CubeSolvers.Solver.solved(Kind.Cube3x3)))

    ///
    /// Every state within `d` moves of solved.
    ///
    def statesWithin(d: Int32): List[State] = Set.toList(Map.keysOf(CubeSolvers.Pocket.Search.table(d)))

}
