mod TestRubikScramble {

    use CubeSolvers.Rubik.Scramble.{secureState, atDistance, secureAtDistance}
    use CubeSolvers.Rubik.Easy.{prepared, distance}
    use CubeSolvers.Rubik.{replay, solved}
    use CubeSolvers.Rubik.Stickers.{toFacelets, ofFacelets}

    /// The table through `depth`, which the tests always ask for within reach.
    ///
    /// Matched rather than defaulted: `Result.getWithDefault` would build the
    /// standard table eagerly on every call, and that is the whole depth-five
    /// table each time.
    def tableTo(depth: Int32): CubeSolvers.Rubik.Easy.Table =
        match CubeSolvers.Rubik.Easy.prepared(depth) {
            case Ok(t)  => t
            case Err(_) => CubeSolvers.Rubik.Easy.standard()
        }

    @Test
    def secureStatesAreLegalCubieStates(): Unit \ {Assert, IO} =
        let state = secureState();
        Assert.assertEq(expected = Ok(state), ofFacelets(toFacelets(state)))

    @Test
    def exactDistanceScramblesAreProvenShortest(): Unit \ Assert =
        let table = tableTo(3);
        let (scramble, _) = atDistance(table, 42i64, 3) |> Result.getWithDefault((Nil, 0i64));
        Assert.assertEq(expected = 3, List.length(scramble));
        match replay(solved(), scramble) {
            case Some(state) => Assert.assertEq(expected = Some(3), distance(table, state))
            case None        => Assert.assertEq(expected = "a state", "none")
        }

    @Test
    def secureExactScramblesHaveTheRequestedLength(): Unit \ {Assert, IO} =
        let scramble = secureAtDistance(tableTo(3), 3)
            |> Result.getWithDefault(Nil);
        Assert.assertEq(expected = 3, List.length(scramble))

}
