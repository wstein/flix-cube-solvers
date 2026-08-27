mod TestSearch {

    use Cube.{Face, Turn, move}
    use Cube.Pocket.{solved, replay}
    use Cube.Pocket.Search.{table, tableMut, descend, solution}

    ///
    /// How many states lie within `d` moves of solved, counted independently.
    ///
    /// These are the published pocket cube figures -- 1, 9, 54, 321, 1847 new
    /// states per level in the half-turn metric -- so a table that agrees with
    /// them is a table whose move mechanics are right.
    ///
    def expectedSize(d: Int32): Int32 = match d {
        case 0 => 1
        case 1 => 10
        case 2 => 64
        case 3 => 385
        case _ => 2232
    }

    @Test
    def theTableGrowsAtTheKnownRate(): Unit \ Assert =
        foreach (d <- List.range(0, 5))
            Assert.assertEq(expected = expectedSize(d), Map.size(table(d)))

    ///
    /// The fixpoint and a hand-written breadth-first walk must agree on every
    /// state and every distance; one is the other's oracle.
    ///
    @Test
    def theFixpointAgreesWithBreadthFirstSearch(): Unit \ Assert =
        Assert.assertEq(expected = tableMut(4), table(4))

    ///
    /// The table is a table of *shortest* distances only because `Down`
    /// reverses the order Flix joins `Int32` in. Pinned here at the lattice
    /// itself, because the failure it guards against is silent: the wrong
    /// direction type-checks, terminates, and answers confidently.
    ///
    @Test
    def theLatticeKeepsTheSmallerDistance(): Unit \ Assert =
        let seeded = #{ Dist(7; Down.Down(5)). Dist(7; Down.Down(2)). };
        Assert.assertEq(
            expected = Vector#{(7, Down.Down(2))},
            query seeded select (s, d) from Dist(s; d))

    ///
    /// The same property where it matters: `U` is one move from solved, and
    /// also three -- `U' U' U'`. A table joining upwards would say three.
    ///
    @Test
    def theTableKeepsTheShortRouteNotTheLongOne(): Unit \ Assert =
        match replay(solved(), move(Face.U, 1, Turn.Cw) :: Nil) {
            case None    => Assert.assertEq(expected = "a state", "none")
            case Some(s) => Assert.assertEq(expected = Some(1), Map.get(s, table(3)))
        }

    @Test
    def descendingFromSolvedTakesNoMoves(): Unit \ Assert =
        Assert.assertEq(expected = Some(Nil), descend(table(2), solved()))

    ///
    /// Every state in the table must be walkable home in exactly its own
    /// distance -- that is what makes the table a solution and not a hint.
    ///
    @Test
    def everyTabledStateWalksHomeInItsOwnDistance(): Unit \ Assert =
        let tbl = table(3);
        foreach ((s, d) <- Map.toList(tbl)) {
            match descend(tbl, s) {
                case None       => Assert.assertEq(expected = "a path", "none for ${s}")
                case Some(path) =>
                    Assert.assertEq(expected = d, List.length(path));
                    Assert.assertEq(expected = Some(solved()), replay(s, path))
            }
        }

    @Test
    def aScrambleIsSolvedByItsOwnLength(): Unit \ Assert =
        let scramble = move(Face.R, 1, Turn.Cw) :: move(Face.U, 1, Turn.Ccw) :: move(Face.F, 1, Turn.Half) :: Nil;
        match replay(solved(), scramble) {
            case None    => Assert.assertEq(expected = "a state", "none")
            case Some(s) =>
                match solution(table(4), 4, s, 8) {
                    case None      => Assert.assertEq(expected = "a solution", "none")
                    case Some(sol) =>
                        Assert.assertEq(expected = 3, List.length(sol));
                        Assert.assertEq(expected = Some(solved()), replay(s, sol))
                }
        }

    ///
    /// Lengths are tried in order, so a scramble that cancels to two moves is
    /// solved in two, not in the four it was written as.
    ///
    @Test
    def solutionsAreShortestNotLiteral(): Unit \ Assert =
        let scramble =
            move(Face.R, 1, Turn.Cw) :: move(Face.U, 1, Turn.Cw) ::
            move(Face.U, 1, Turn.Cw) :: move(Face.U, 1, Turn.Cw) :: Nil;
        match replay(solved(), scramble) {
            case None    => Assert.assertEq(expected = "a state", "none")
            case Some(s) => Assert.assertEq(expected = Some(2), Option.map(List.length, solution(table(4), 4, s, 8)))
        }

}
