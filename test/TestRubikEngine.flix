///
/// The 3x3 engine, end to end.
///
/// Building the tables costs seconds, so one test does the work of several:
/// the alternative is paying for them once per assertion.
///
mod TestRubikEngine {

    use CubeSolvers.{Kind, Move}
    use CubeSolvers.Rubik.{State, solved, replay}
    use CubeSolvers.Rubik.Engine.{prepared, defaultBound}
    use CubeSolvers.Rubik.Stickers.{toFacelets, ofFacelets}
    use CubeSolvers.Solver.{Invalid, Outcome, defaultOptions, kind, search}

    def scrambles(): List[String] =
        "D2 F' L2 R2 D2 F B' L' B' D2 L2 B" ::
        "F R D2 R2 B L R B2 F' B F U2" ::
        "R U R' U' R' F R2 U' R' U' R U R' F'" :: Nil

    def cubeOf(text: String): Option[State] =
        CubeSolvers.parse(text) |> Result.toOption |> Option.flatMap(ms -> replay(solved(), ms))

    ///
    /// Every scramble comes back solved, within the bound the engine claims,
    /// and the solution is checked by replaying it rather than by trusting the
    /// search that produced it.
    ///
    @Test
    def scramblesAreSolvedAndTheSolutionsReplay(): Unit \ Assert =
        let engine = prepared();
        Assert.assertEq(expected = Kind.Cube3x3, kind(engine));
        foreach (text <- scrambles()) {
            match cubeOf(text) {
                case None    => Assert.assertEq(expected = "a cube", "unparsed: ${text}")
                case Some(s) => match search(engine, toFacelets(s), defaultOptions()) {
                    case Outcome.Solved(ms) =>
                        Assert.assertEq(expected = true, List.length(ms) <= defaultBound());
                        Assert.assertEq(expected = Some(solved()), replay(s, ms))
                    case other => Assert.assertEq(expected = "Solved", "${other} for ${text}")
                }
            }
        };
        // A solved cube needs nothing, and a bound of one move cannot solve a
        // twelve-move scramble: the caller's limit, not the engine running out.
        Assert.assertEq(expected = Outcome.Solved(Nil), search(engine, toFacelets(solved()), defaultOptions()));
        match cubeOf(List.head(scrambles()) |> Option.getWithDefault("")) {
            case None    => Assert.assertEq(expected = "a cube", "unparsed")
            case Some(s) => Assert.assertEq(
                expected = Outcome.BudgetSpent,
                search(engine, toFacelets(s), { maxMoves = Some(1) }))
        }

    ///
    /// Every 3x3 can be solved in twenty moves, and this engine is expected
    /// to stay under that mark rather than merely somewhere near it.
    ///
    /// Two-phase search does not guarantee it -- the shortest solution need
    /// not split into "reduce, then finish" -- so it is asserted on cubes
    /// rather than argued.
    ///
    @Test
    def solutionsStayWithinTwentyMoves(): Unit \ Assert =
        let engine = prepared();
        let harder =
            "D2 F' L2 R2 D2 F B' L' B' D2 L2 B" ::
            "L' R U2 F2 U' R' B L B2 R2 F' D2" ::
            "F L R F' U L' B2 R D' L F2 L" ::
            "B' F' L' D R' L2 B U2 L D2 F2 R2" ::
            "U2 D2 L2 R2 F2 B2 U D L R F B U' D' L' R'" :: Nil;
        foreach (text <- harder) {
            match cubeOf(text) {
                case None    => Assert.assertEq(expected = "a cube", "unparsed: ${text}")
                case Some(s) => match search(engine, toFacelets(s), defaultOptions()) {
                    case Outcome.Solved(ms) =>
                        Assert.assertEq(expected = true, List.length(ms) <= 20);
                        Assert.assertEq(expected = Some(solved()), replay(s, ms))
                    case other => Assert.assertEq(expected = "Solved", "${other} for ${text}")
                }
            }
        }

    ///
    /// The kind check needs no tables, so it does not pay for any.
    ///
    @Test
    def anotherPuzzleIsRejectedAtTheBoundary(): Unit \ Assert =
        Assert.assertEq(
            expected = Err(Invalid.WrongKind(Kind.Cube2x2)),
            ofFacelets(CubeSolvers.Solver.solved(Kind.Cube2x2)))

}
