mod TestRubikScramble {

    use CubeSolvers.Rubik.Scramble.{secureState, atDistance, secureAtDistance}
    use CubeSolvers.Rubik.Easy.{prepared, distance}
    use CubeSolvers.Rubik.{replay, solved}
    use CubeSolvers.Rubik.Stickers.{toFacelets, ofFacelets}

    @Test
    def secureStatesAreLegalCubieStates(): Unit \ {Assert, IO} =
        let state = secureState();
        Assert.assertEq(expected = Ok(state), ofFacelets(toFacelets(state)))

    @Test
    def exactDistanceScramblesAreProvenShortest(): Unit \ Assert =
        let table = prepared(3);
        let (scramble, _) = atDistance(table, 42i64, 3);
        Assert.assertEq(expected = 3, List.length(scramble));
        match replay(solved(), scramble) {
            case Some(state) => Assert.assertEq(expected = Some(3), distance(table, state))
            case None        => Assert.assertEq(expected = "a state", "none")
        }

    @Test
    def secureExactScramblesHaveTheRequestedLength(): Unit \ {Assert, IO} =
        let scramble = secureAtDistance(prepared(3), 3);
        Assert.assertEq(expected = 3, List.length(scramble))

}
