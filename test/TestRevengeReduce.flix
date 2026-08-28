///
/// The 4x4's third phase, centres only.
///
mod TestRevengeReduce {

    use CubeSolvers.{Face, Kind}
    use CubeSolvers.Solver.{Facelets, Invalid}
    use CubeSolvers.Revenge.{Centers, Separate, Walk}
    use CubeSolvers.Revenge.Reduce.{count, moves, coordinate, solved, table, up, front, sideIndex}

    def scrambles(): List[String] =
        "D2 F' Lw2 R2 Dw2 Fw B' L' Bw' Dw2 Lw2 Bw U' R2 D" ::
        "L' R U2 F2 U' R' B Lw B2 Rw2 Fw' Dw2 Fw2 R Lw2" ::
        "Fw R Dw2 Rw2 B Lw R Bw2 F' Bw Fw U2 Rw' L2 F2" :: Nil

    def scrambled(text: String): Option[Facelets] =
        CubeSolvers.parse(text)
            |> Result.toOption
            |> Option.flatMap(ms ->
                CubeSolvers.Turning.replay(CubeSolvers.Solver.solved(Kind.Cube4x4), ms) |> Result.toOption)

    ///
    /// How many centre stickers are not on their own face.
    ///
    def strayCentres(cube: Facelets): Int32 =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let faces = Vector#{Face.U, Face.R, Face.F, Face.D, Face.L, Face.B};
        Centers.slots() |> Vector.mapWithIndex(i -> slot ->
            if (Vector.get(slot, stickers) == Vector.get(i / 4, faces)) 0 else 1) |> Vector.sum

    @Test
    def aSolvedCubeIsAtTheGoal(): Unit \ Assert =
        Assert.assertEq(expected = solved(), coordinate(CubeSolvers.Solver.solved(Kind.Cube4x4)));
        Assert.assertEq(expected = 20, Vector.length(moves()))

    ///
    /// The three phases together must leave every centre sticker on its own
    /// face, and the edge and corner permutations agreeing about parity --
    /// checked by counting stickers and permutations rather than by asking
    /// the tables that produced the moves.
    ///
    @Test
    def theThreePhasesSolveTheCentres(): Unit \ Assert =
        let one = Centers.table();
        let two = Separate.table();
        let three = table();
        // Every phase's table must reach every state: one that does not is a
        // phase the walk cannot solve.
        Assert.assertEq(expected = Centers.count(), Vector.length(one));
        Assert.assertEq(expected = 0, Vector.count(d -> d < 0, one));
        Assert.assertEq(expected = 8, Vector.foldLeft(Int32.max, 0, one));
        Assert.assertEq(expected = Separate.count(), Vector.length(two));
        Assert.assertEq(expected = 0, Vector.count(d -> d < 0, two));
        Assert.assertEq(expected = 9, Vector.foldLeft(Int32.max, 0, two));
        Assert.assertEq(expected = count(), Vector.length(three));
        Assert.assertEq(expected = 0, Vector.count(d -> d < 0, three));
        // Ten rather than nine: carrying the parity bit adds a level, which
        // is the move a cube pays to put the two permutations back in step.
        Assert.assertEq(expected = 10, Vector.foldLeft(Int32.max, 0, three));
        foreach (text <- scrambles()) {
            match scrambled(text) {
                case None       => Assert.assertEq(expected = "a cube", "unparsed: ${text}")
                case Some(cube) => match centres(cube, one, two, three) {
                    case Err(why)  => Assert.assertEq(expected = "a reduction", "${why}")
                    case Ok(after) =>
                        Assert.assertEq(expected = 0, strayCentres(after));
                        // The edge and corner permutations must still agree:
                        // a quarter turn of F, B, U or D flips that, and this
                        // phase is allowed those turns.
                        Assert.assertEq(expected = 0, Separate.mismatch(after))
                }
            }
        }

    ///
    /// The cube after all three phases' centre work.
    ///
    def centres(
        cube: Facelets,
        one: Vector[Int32],
        two: Vector[Int32],
        three: Vector[Int32]
    ): Result[Invalid, Facelets] =
        forM (first <- Walk.down(cube, one, Centers.separation, Centers.moves());
              afterOne <- CubeSolvers.Turning.replay(cube, first);
              second <- Walk.down(afterOne, two, Separate.coordinate, Separate.moves());
              afterTwo <- CubeSolvers.Turning.replay(afterOne, second);
              third <- Walk.down(afterTwo, three, coordinate, moves());
              afterThree <- CubeSolvers.Turning.replay(afterTwo, third))
            yield afterThree

    @Test
    def aCubeThatSkippedTheSecondPhaseIsRefused(): Unit \ Assert =
        match scrambled("Fw R Dw2 Rw2 B Lw R Bw2 F' Bw Fw U2 Rw' L2 F2") {
            case None       => Assert.assertEq(expected = "a cube", "unparsed")
            case Some(cube) => Assert.assertEq(expected = -1, sideIndex(cube))
        }

}
