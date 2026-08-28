mod TestRubikScramble {

    use CubeSolvers.Rubik.Scramble.secureState
    use CubeSolvers.Rubik.Stickers.{toFacelets, ofFacelets}

    @Test
    def secureStatesAreLegalCubieStates(): Unit \ {Assert, IO} =
        let state = secureState();
        Assert.assertEq(expected = Ok(state), ofFacelets(toFacelets(state)))

}
