///
/// Cases checked against an independent implementation.
///
/// The round trip in `TestStickers` proves the two sticker tables agree with
/// each other, which a mirrored layout would also manage. These four cubes
/// were written out by this package, handed to the Java pocket engine in
/// `wstein/cube-solvers` -- a different implementation, of different
/// ancestry -- and it returned a solution of the length recorded here. It
/// returned the same moves too, but the length is what is asserted: move
/// order is a tie-break, and shortest is the property.
///
mod TestCrossCheck {

    use CubeSolvers.{Face, Kind}
    use CubeSolvers.Pocket.{replay, solved}
    use CubeSolvers.Pocket.Engine.prepared
    use CubeSolvers.Pocket.Stickers.ofFacelets
    use CubeSolvers.Solver.{Facelets, Outcome, defaultOptions, facelets, search}

    ///
    /// Sticker string, the shortest solution length, and what the Java engine
    /// answered.
    ///
    def cases(): List[(String, Int32, String)] =
        ("FUBRDRLBDFLDUFDLURLFBRUB", 4, "R F2 U R'") ::
        ("FFLDRLDFBBRRUFDRLULBDUUB", 8, "U R2 F2 U' R F U2 R'") ::
        ("DLBLUDUBDFBFRRDLFRLUFRUB", 5, "F2 R' F' U' F2") ::
        ("DUUDLRRLFFFFUDDURLLRBBBB", 4, "U2 R2 U2 F2") ::
        Nil

    ///
    /// A sticker string as a cube, counted on the way in.
    ///
    def read(text: String): Option[Facelets] =
        let stickers = text |> String.toList |> List.filterMap(CubeSolvers.faceOfToken) |> List.toVector;
        facelets(Kind.Cube2x2, stickers) |> Result.toOption

    ///
    /// The same for the 3x3, where the other engine is min2phase and the
    /// bar is different: this package's two-phase search is not optimal, so
    /// what is asserted is that it does not do *worse* than the engine it is
    /// checked against. On this cube both return the same eleven moves.
    ///
    @Test
    def theRubikCubeIsSolvedNoWorseThanTheOtherEngine(): Unit \ Assert =
        let text = "UUUUUUUUUBLFRRRRRRFFRFFFFFFDDDDDDDDDLRLLLLLLLRBBBBBBBB";
        let stickers = text |> String.toList |> List.filterMap(CubeSolvers.faceOfToken) |> List.toVector;
        match CubeSolvers.Solver.facelets(Kind.Cube3x3, stickers) {
            case Err(why)   => Assert.assertEq(expected = "a countable cube", "${why}")
            case Ok(cube)   => match CubeSolvers.Solver.search(CubeSolvers.Rubik.Engine.prepared(), cube, defaultOptions()) {
                case CubeSolvers.Solver.Outcome.Solved(ms) =>
                    Assert.assertEq(expected = true, List.length(ms) <= 11);
                    Assert.assertEq(
                        expected = Ok(Some(CubeSolvers.Rubik.solved())),
                        Result.map(state -> CubeSolvers.Rubik.replay(state, ms), CubeSolvers.Rubik.Stickers.ofFacelets(cube)))
                case other => Assert.assertEq(expected = "Solved", "${other}")
            }
        }

    ///
    /// A solution checked by a path that does not share the engine's model.
    ///
    /// The engines answer in cubies and are verified in cubies, which cannot
    /// catch a cubie model that is wrong in the same way twice. Replaying the
    /// solution as a plain permutation of stickers can.
    ///
    @Test
    def solutionsSurviveBeingReplayedAsStickers(): Unit \ Assert =
        let text = "UUUUUUUUUBLFRRRRRRFFRFFFFFFDDDDDDDDDLRLLLLLLLRBBBBBBBB";
        let stickers = text |> String.toList |> List.filterMap(CubeSolvers.faceOfToken) |> List.toVector;
        match CubeSolvers.Solver.facelets(Kind.Cube3x3, stickers) {
            case Err(why) => Assert.assertEq(expected = "a countable cube", "${why}")
            case Ok(cube) => match CubeSolvers.Solver.search(CubeSolvers.Rubik.Engine.prepared(), cube, defaultOptions()) {
                case CubeSolvers.Solver.Outcome.Solved(ms) => Assert.assertEq(
                    expected = Ok(true),
                    CubeSolvers.Turning.replay(cube, ms)
                        |> Result.map(CubeSolvers.Turning.isSolved)
                        |> Result.mapErr(e -> "${e}"))
                case other => Assert.assertEq(expected = "Solved", "${other}")
            }
        }

    @Test
    def everyCrossCheckedCubeSolvesInTheLengthTheOtherEngineFound(): Unit \ Assert =
        let engine = prepared(5);
        foreach ((text, length, _) <- cases()) {
            match read(text) {
                case None      => Assert.assertEq(expected = "a countable cube", "rejected: ${text}")
                case Some(cube) => match search(engine, cube, defaultOptions()) {
                    case Outcome.Solved(sol) =>
                        Assert.assertEq(expected = length, List.length(sol));
                        Assert.assertEq(
                            expected = Ok(Some(solved())),
                            Result.map(state -> replay(state, sol), ofFacelets(cube)))
                    case other => Assert.assertEq(expected = "Solved", "${other} for ${text}")
                }
            }
        }

}
