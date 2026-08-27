mod TestPocket {

    use Cube.{Face, Turn, move}
    use Cube.Pocket.{State, solved, applyMove, replay, moves, neighbours, permutation, twist}

    ///
    /// A sequence read as moves, for tests that care about the state and not
    /// the notation.
    ///
    def after(ms: List[Cube.Move]): Option[State] = replay(solved(), ms)

    @Test
    def solvedIsIdentity(): Unit \ Assert =
        Assert.assertEq(expected = Vector.range(0, 8), permutation(solved()));
        Assert.assertEq(expected = Vector.repeat(8, 0), twist(solved()))

    @Test
    def aPocketCubeTurnsThreeFacesNineWays(): Unit \ Assert =
        Assert.assertEq(expected = 9, Vector.length(moves()))

    ///
    /// `D`, `L` and `B` are the fixed corner's own faces: turning them would
    /// move the corner the model pins down.
    ///
    @Test
    def theFixedCornersFacesDoNotTurn(): Unit \ Assert =
        Assert.assertEq(expected = None, applyMove(solved(), move(Face.D, 1, Turn.Cw)));
        Assert.assertEq(expected = None, applyMove(solved(), move(Face.L, 1, Turn.Cw)));
        Assert.assertEq(expected = None, applyMove(solved(), move(Face.B, 1, Turn.Cw)))

    ///
    /// A cube two layers thick has no inner slice, so depth 2 is a rotation.
    ///
    @Test
    def aWideTurnIsNotAPocketCubeMove(): Unit \ Assert =
        Assert.assertEq(expected = None, applyMove(solved(), move(Face.U, 2, Turn.Cw)))

    @Test
    def fourQuarterTurnsOfAFaceAreTheIdentity(): Unit \ Assert =
        foreach (f <- Vector.toList(Cube.Pocket.faces())) {
            let quarter = move(f, 1, Turn.Cw);
            Assert.assertEq(expected = Some(solved()), after(List.repeat(4, quarter)))
        }

    @Test
    def aHalfTurnIsTwoQuarterTurns(): Unit \ Assert =
        let half = after(move(Face.R, 1, Turn.Half) :: Nil);
        let twice = after(List.repeat(2, move(Face.R, 1, Turn.Cw)));
        Assert.assertEq(expected = half, twice)

    @Test
    def aQuarterTurnIsUndoneByItsInverse(): Unit \ Assert =
        Assert.assertEq(
            expected = Some(solved()),
            after(move(Face.F, 1, Turn.Cw) :: move(Face.F, 1, Turn.Ccw) :: Nil))

    @Test
    def aSequenceIsUndoneByItsInverse(): Unit \ Assert =
        let scramble = move(Face.R, 1, Turn.Cw) :: move(Face.U, 1, Turn.Half) :: move(Face.F, 1, Turn.Ccw) :: Nil;
        Assert.assertEq(expected = Some(solved()), after(scramble ::: Cube.inverse(scramble)))

    @Test
    def aScrambledCubeIsNotSolved(): Unit \ Assert =
        Assert.assertEq(expected = false, after(move(Face.R, 1, Turn.Cw) :: Nil) == Some(solved()))

    @Test
    def everyMoveLeadsSomewhereElseAndSomewhereDistinct(): Unit \ Assert =
        let ns = Vector.toList(neighbours(solved()));
        Assert.assertEq(expected = 9, List.length(ns));
        Assert.assertEq(expected = 9, List.length(List.distinct(ns)));
        Assert.assertEq(expected = false, List.memberOf(solved(), ns))

}
