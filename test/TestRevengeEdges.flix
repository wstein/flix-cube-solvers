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

    ///
    /// The allocation-free indexer is the search hot path. It must be exactly
    /// the older State-based indexer for every first-coordinate arrangement;
    /// those are the complete set on which the symmetry fold is defined.
    ///
    @Test
    def directPermutationIndexingAgreesWithStateIndexing(): Unit \ Assert =
        let classes = classify();
        let rotations = CubeSolvers.Revenge.Edges.rotationTables();
        foreach (i <- List.range(0, firstCount())) {
            let state = unrank(i * rawCount());
            Assert.assertEq(
                expected = CubeSolvers.Revenge.Edges.indexOf(classes, state),
                CubeSolvers.Revenge.Edges.indexOfPermutation(classes, rotations, state#edge))
        }

    ///
    /// The packed table holds two bits per state, and reading back what was
    /// written is the whole contract.
    ///
    @Test
    def twoBitEntriesSurviveBeingWritten(): Unit \ Assert =
        region rc {
            let table = Array.repeat(rc, 4, -1);
            // Every slot starts unwritten, which is the value three.
            foreach (i <- List.range(0, 64))
                Assert.assertEq(expected = 3, CubeSolvers.Revenge.Edges.get(table, i));
            foreach ((i, v) <- (0, 0) :: (1, 2) :: (15, 1) :: (16, 0) :: (63, 2) :: Nil) {
                CubeSolvers.Revenge.Edges.put(table, i, v);
                Assert.assertEq(expected = v, CubeSolvers.Revenge.Edges.get(table, i))
            };
            // Writing one slot must leave its neighbours alone.
            Assert.assertEq(expected = 3, CubeSolvers.Revenge.Edges.get(table, 2));
            Assert.assertEq(expected = 3, CubeSolvers.Revenge.Edges.get(table, 14))
        }

    @Test
    def theTableIsAsBigAsTheTwoCoordinatesTogether(): Unit \ Assert =
        Assert.assertEq(expected = 31006080, CubeSolvers.Revenge.Edges.pruningSize());
        Assert.assertEq(expected = classCount() * rawCount(), CubeSolvers.Revenge.Edges.pruningSize())

    ///
    /// The first few rings of the table, which cost a moment rather than the
    /// eighty seconds the whole thing takes.
    ///
    /// Filling the table completely is a build step, not a test: 31,006,080
    /// entries take about 79s here. What is checked here is that the sweep
    /// starts correctly and grows at the rate it grew when the full table was
    /// verified.
    ///
    @Test
    def theSweepFillsTheFirstRingsAtTheKnownRate(): Unit \ Assert =
        region rc {
            let shallow = CubeSolvers.Revenge.Edges.pruning(rc, 5);
            Assert.assertEq(expected = 1496, shallow#filled);
            Assert.assertEq(expected = 5, shallow#depth);
            // The solved state is the one the sweep starts from.
            Assert.assertEq(expected = 0, CubeSolvers.Revenge.Edges.get(shallow#table, 0))
        }

}
