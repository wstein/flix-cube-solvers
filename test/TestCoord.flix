mod TestCoord {

    use Cube.Pocket.{State, solved}
    use Cube.Pocket.Coord.{encode, decode}

    ///
    /// Every state within `d` moves of solved.
    ///
    def statesWithin(d: Int32): List[State] = Set.toList(Map.keysOf(Cube.Pocket.Search.table(d)))

    @Test
    def solvedEncodesToTheCubiclesInOrder(): Unit \ Assert =
        Assert.assertEq(expected = solved(), decode(encode(solved())))

    @Test
    def everyStateSurvivesEncoding(): Unit \ Assert =
        foreach (s <- statesWithin(3))
            Assert.assertEq(expected = s, decode(encode(s)))

    ///
    /// Two states must never share a code, or a table built on codes would
    /// quietly merge them.
    ///
    @Test
    def distinctStatesGetDistinctCodes(): Unit \ Assert =
        let states = statesWithin(3);
        let codes = states |> List.map(encode) |> List.distinct;
        Assert.assertEq(expected = List.length(states), List.length(codes))

    ///
    /// The two tables are the same table.
    ///
    @Test
    def theCoordinateTableAgreesWithTheCubieTable(): Unit \ Assert =
        foreach (d <- List.range(0, 4))
            Assert.assertEq(
                expected = Cube.Pocket.Search.table(d) |> Map.toList |> List.map(match (s, n) -> (encode(s), n)) |> List.toMap,
                Cube.Pocket.Coord.table(d))

}
