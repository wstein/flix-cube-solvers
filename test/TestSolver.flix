mod TestSolver {

    use CubeSolvers.{Face, Kind}
    use CubeSolvers.Solver.{Invalid, facelets, solved, stickersOf, kindOf}

    @Test
    def solvedStateIsAcceptedByTheValidator(): Unit \ Assert =
        let cube = solved(Kind.Cube3x3);
        Assert.assertEq(expected = true, Result.isOk(facelets(Kind.Cube3x3, stickersOf(cube))));
        Assert.assertEq(expected = Kind.Cube3x3, kindOf(cube))

    @Test
    def stickerCountIsSixTimesTheFace(): Unit \ Assert =
        Assert.assertEq(expected = 24, Vector.length(stickersOf(solved(Kind.Cube2x2))))

    @Test
    def aShortStateIsRejectedByLength(): Unit \ Assert =
        let short = Vector.repeat(53, Face.U);
        Assert.assertEq(expected = Err(Invalid.WrongLength(54, 53)), facelets(Kind.Cube3x3, short))

    ///
    /// The count check is what distinguishes this from the length check: 54
    /// stickers, all white, is the right size and still not a cube.
    ///
    @Test
    def aMonochromeStateIsRejectedByCount(): Unit \ Assert =
        let allWhite = Vector.repeat(54, Face.U);
        Assert.assertEq(expected = Err(Invalid.StickerCounts(Face.U, 9, 54)), facelets(Kind.Cube3x3, allWhite))

    ///
    /// Swapping two stickers keeps every count right, so the counter passes it.
    /// Catching this needs the piece structure, which is the engine's job --
    /// and when an engine does catch it, it must say `Unsolvable`, not that it
    /// is unavailable.
    ///
    @Test
    def theCounterCannotSeeAnUnreachableState(): Unit \ Assert =
        // Trade one U sticker for one R sticker and back: every colour still
        // appears nine times, but no cube ever looks like this.
        let xs = Vector.toList(stickersOf(solved(Kind.Cube3x3)));
        let swapped = List.mapWithIndex((i, s) -> match i {
            case 0 => Face.R
            case 9 => Face.U
            case _ => s
        }, xs) |> List.toVector;
        Assert.assertEq(expected = true, Result.isOk(facelets(Kind.Cube3x3, swapped)))

    @Test
    def invalidReadsAsASentence(): Unit \ Assert =
        Assert.assertEq(expected = "Expected 54 stickers but got 53", "${Invalid.WrongLength(54, 53)}")

}
