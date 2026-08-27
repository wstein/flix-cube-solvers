mod TestPocketEngine {

    use CubeSolvers.{Face, Kind, Turn, move}
    use CubeSolvers.Pocket.{solved, replay}
    use CubeSolvers.Pocket.Engine.{Engine, prepared}
    use CubeSolvers.Pocket.Stickers.toFacelets
    use CubeSolvers.Solver.{Invalid, Outcome, defaultOptions}
    use CubeSolvers.Solver.{kind, search}

    ///
    /// A scrambled cube, as stickers, the way a caller would hand one over.
    ///
    def scrambled(ms: List[CubeSolvers.Move]): CubeSolvers.Solver.Facelets =
        match replay(solved(), ms) {
            case Some(s) => toFacelets(s)
            case None    => toFacelets(solved())
        }

    def scramble(): List[CubeSolvers.Move] =
        move(Face.R, 1, Turn.Cw) :: move(Face.U, 1, Turn.Ccw) :: move(Face.F, 1, Turn.Half) :: Nil

    ///
    /// The engine declares no effect, so this function may be pure. That it
    /// compiles at all is the assertion; the equality is a bonus.
    ///
    def solvePurely(e: Engine, ms: List[CubeSolvers.Move]): Outcome = search(e, scrambled(ms), defaultOptions())

    @Test
    def theEngineAnswersForTheCubeItHandles(): Unit \ Assert =
        Assert.assertEq(expected = Kind.Cube2x2, kind(prepared(2)))

    @Test
    def aScrambleComesBackSolvedAndShortest(): Unit \ Assert =
        match solvePurely(prepared(4), scramble()) {
            case Outcome.Solved(sol) =>
                Assert.assertEq(expected = 3, List.length(sol));
                Assert.assertEq(expected = Some(solved()), replay(Option.getWithDefault(solved(), Result.toOption(CubeSolvers.Pocket.Stickers.ofFacelets(scrambled(scramble())))), sol))
            case other => Assert.assertEq(expected = "Solved", "${other}")
        }

    @Test
    def anAlreadySolvedCubeNeedsNoMoves(): Unit \ Assert =
        Assert.assertEq(expected = Outcome.Solved(Nil), solvePurely(prepared(2), Nil))

    ///
    /// A bound below the shortest solution is the caller's limit, not the
    /// engine running out of cube: `BudgetSpent`, never `Exhausted`.
    ///
    @Test
    def tooTightABoundSpendsTheBudget(): Unit \ Assert =
        Assert.assertEq(
            expected = Outcome.BudgetSpent,
            search(prepared(4), scrambled(scramble()), { maxMoves = Some(2) }))

    @Test
    def aBoundAtTheSolutionLengthIsEnough(): Unit \ Assert =
        match search(prepared(4), scrambled(scramble()), { maxMoves = Some(3) }) {
            case Outcome.Solved(sol) => Assert.assertEq(expected = 3, List.length(sol))
            case other               => Assert.assertEq(expected = "Solved", "${other}")
        }

    @Test
    def anotherPuzzleIsRejectedNotAttempted(): Unit \ Assert =
        Assert.assertEq(
            expected = Outcome.Rejected(Invalid.WrongKind(Kind.Cube3x3)),
            search(prepared(2), CubeSolvers.Solver.solved(Kind.Cube3x3), defaultOptions()))

}
