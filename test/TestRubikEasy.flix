mod TestRubikEasy {

    use CubeSolvers.Rubik.{solved, replay}
    use CubeSolvers.Rubik.Easy.{prepared, stateOf, key, distance, layer, descend}

    @Test
    def compactKeysRoundTripCubieStates(): Unit \ Assert =
        let table = prepared(3);
        foreach (stateKey <- layer(table, 3))
            Assert.assertEq(expected = stateKey, key(stateOf(stateKey)))

    @Test
    def shallowLayersHaveThePublishedSizes(): Unit \ Assert =
        let table = prepared(3);
        Assert.assertEq(expected = 1, Vector.length(layer(table, 0)));
        Assert.assertEq(expected = 18, Vector.length(layer(table, 1)));
        Assert.assertEq(expected = 243, Vector.length(layer(table, 2)));
        Assert.assertEq(expected = 3240, Vector.length(layer(table, 3)))

    @Test
    def eachTabledStateHasAShortestRouteHome(): Unit \ Assert =
        let table = prepared(3);
        foreach (stateKey <- layer(table, 3)) {
            let state = stateOf(stateKey);
            match descend(table, state) {
                case Some(path) =>
                    Assert.assertEq(expected = Some(3), distance(table, state));
                    Assert.assertEq(expected = 3, List.length(path));
                    Assert.assertEq(expected = Some(solved()), replay(state, path))
                case None => Assert.assertEq(expected = "a path", "none")
            }
        }

}
