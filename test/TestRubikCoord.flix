mod TestRubikCoord {

    use Cube.Rubik.{State, solved, replay, applyMove, moves}
    use Cube.Rubik.Coord.{twist, flip, slice, withTwist, withFlip, withSlice,
        twistCount, flipCount, sliceCount, twistTable, flipTable, sliceTable}

    ///
    /// A handful of cubes to test against: solved, and what the shared
    /// vectors' scrambles produce.
    ///
    def samples(): List[State] =
        let scrambles =
            "D2 F' L2 R2 D2 F B' L' B' D2 L2 B" ::
            "L' R U2 F2 U' R' B L B2 R2 F' D2" ::
            "F R D2 R2 B L R B2 F' B F U2" ::
            "R U R' U' R' F R2 U' R' U' R U R' F'" :: Nil;
        solved() :: (scrambles |> List.filterMap(text ->
            Cube.parse(text) |> Result.toOption |> Option.flatMap(ms -> replay(solved(), ms))))

    @Test
    def everyTwistSurvivesTheRoundTrip(): Unit \ Assert =
        foreach (n <- List.range(0, twistCount()))
            Assert.assertEq(expected = n, twist(withTwist(n)))

    @Test
    def everyFlipSurvivesTheRoundTrip(): Unit \ Assert =
        foreach (n <- List.range(0, flipCount()))
            Assert.assertEq(expected = n, flip(withFlip(n)))

    @Test
    def everySliceSurvivesTheRoundTrip(): Unit \ Assert =
        foreach (n <- List.range(0, sliceCount()))
            Assert.assertEq(expected = n, slice(withSlice(n)))

    ///
    /// The solved cube sits at the origin of all three, which is what makes
    /// "all zero" the goal of the first phase rather than a triple of magic
    /// numbers.
    ///
    @Test
    def theSolvedCubeIsTheOrigin(): Unit \ Assert =
        Assert.assertEq(expected = 0, twist(solved()));
        Assert.assertEq(expected = 0, flip(solved()));
        Assert.assertEq(expected = 0, slice(solved()))

    ///
    /// The point of the whole layer: turning the cube and reading the
    /// coordinate must give what the table says, for every move and every
    /// cube. A coordinate that fails this describes something other than the
    /// cube it came from.
    ///
    @Test
    def theTablesAgreeWithTheCube(): Unit \ Assert =
        let tt = twistTable();
        let ft = flipTable();
        let st = sliceTable();
        foreach (s <- samples(); (i, m) <- List.mapWithIndex(i -> m -> (i, m), Vector.toList(moves()))) {
            match applyMove(s, m) {
                case None    => Assert.assertEq(expected = "a move", "${m} refused")
                case Some(t) =>
                    Assert.assertEq(expected = twist(t), Vector.get(i, Vector.get(twist(s), tt)));
                    Assert.assertEq(expected = flip(t), Vector.get(i, Vector.get(flip(s), ft)));
                    Assert.assertEq(expected = slice(t), Vector.get(i, Vector.get(slice(s), st)))
            }
        }

}
