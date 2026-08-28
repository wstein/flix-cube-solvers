mod TestRevengeEdges {

    use CubeSolvers.Revenge.Edges.{rawCount, firstCount, classCount, solved, rank, unrank, move, rotate, classify}

    ///
    /// Ranking and unranking must invert each other, or every table built on
    /// them is nonsense. Checked on all 11,880 arrangements of the first
    /// coordinate, which is the exact set the symmetry fold walks.
    ///
    @Test
    def rankingIsReversibleEverywhere(): Unit \ Assert =
        foreach (i <- List.range(0, firstCount()))
            Assert.assertEq(expected = i, rank(unrank(i * rawCount()), 4))

    @Test
    def theSolvedStateRanksToZero(): Unit \ Assert =
        Assert.assertEq(expected = 0, rank(solved(), 4));
        Assert.assertEq(expected = 0, rank(solved(), 10))

    ///
    /// Half turns undo themselves, which is a cheap check on the move data.
    ///
    @Test
    def everyHalfTurnIsItsOwnInverse(): Unit \ Assert =
        foreach (m <- 1 :: 3 :: 5 :: 8 :: 10 :: 12 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: Nil)
            Assert.assertEq(expected = rank(solved(), 10), rank(move(move(solved(), m), m), 10))

    @Test
    def everyQuarterTurnComesBackAfterFour(): Unit \ Assert =
        foreach (m <- 0 :: 2 :: 4 :: 6 :: 7 :: 9 :: 11 :: 13 :: Nil) {
            let once = move(solved(), m);
            Assert.assertEq(expected = rank(solved(), 10), rank(move(move(move(once, m), m), m), 10))
        }

    ///
    /// A rotation turns the whole cube, and a cube with nothing wrong stays
    /// that way whichever way up it is held.
    ///
    @Test
    def rotatingASolvedStateChangesNothing(): Unit \ Assert =
        foreach (r <- List.range(0, 8))
            Assert.assertEq(expected = 0, rank(rotate(solved(), r), 4))

    ///
    /// The number the whole port answers to.
    ///
    /// Fold the 11,880 arrangements by the eight rotations and 1,538 families
    /// come out. The ranking, the twenty moves, the three rotations and the
    /// fold all feed that count, so any one of them being wrong moves it.
    ///
    @Test
    def theSymmetryFoldFindsFifteenHundredAndThirtyEightClasses(): Unit \ Assert =
        let found = classify();
        Assert.assertEq(expected = classCount(), found#classes);
        Assert.assertEq(expected = firstCount(), Vector.length(found#raw2sym));
        Assert.assertEq(expected = 0, Vector.get(0, found#sym2raw));
        Assert.assertEq(expected = 0, Vector.get(0, found#raw2sym) / 8)

}
