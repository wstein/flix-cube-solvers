///
/// The vectors shared with the Java implementation.
///
/// `wstein/cube-solvers` writes out what its TCK oracle does to a solved cube
/// -- a scramble, and the stickers it produces -- and this reads the same file
/// and asks the same of this package's move mechanics. Agreeing on solutions
/// is not enough: two implementations can agree on an answer and disagree on
/// what the question said, and this is where that would show.
///
mod TestVectors {

    use Cube.Pocket.{replay, solved}
    use Cube.Pocket.Stickers.toFacelets
    use Fs.FileRead.{FileRead, runWithIO}

    ///
    /// Where the shared vectors live, copied verbatim from the Java repository.
    ///
    def path(): String = "test/fixtures/replay-vectors.tsv"

    ///
    /// Reading is an effect, and the test is where it is discharged: the
    /// checking below asks for `FileRead`, and this is what answers it.
    ///
    @Test
    def everyPocketVectorScramblesToItsRecordedCube(): Unit \ Assert + IO =
        runWithIO(() -> checkVectors())

    ///
    /// Every 2x2 vector: read the scramble, apply it to a solved cube, and
    /// compare the stickers with the ones the other implementation recorded.
    ///
    def checkVectors(): Unit \ Assert + FileRead =
        match FileRead.readLines(path()) {
            case Err(_)    => Assert.assertEq(expected = "the vectors", "cannot read ${path()}")
            case Ok(lines) =>
                let pocket = lines |> List.filter(String.startsWith(prefix = "2x2\t"));
                Assert.assertEq(expected = 10, List.length(pocket));
                foreach (line <- pocket)
                    checkVector(line)
        }

    ///
    /// The same for the 3x3, where the vectors are general scrambles: all six
    /// faces, and nothing pinned.
    ///
    @Test
    def everyRubikVectorScramblesToItsRecordedCube(): Unit \ Assert + IO =
        runWithIO(() -> checkRubikVectors())

    ///
    /// Every 3x3 vector, replayed through the cubie model and compared
    /// sticker for sticker.
    ///
    def checkRubikVectors(): Unit \ Assert + FileRead =
        match FileRead.readLines(path()) {
            case Err(_)    => Assert.assertEq(expected = "the vectors", "cannot read ${path()}")
            case Ok(lines) =>
                let rubik = lines |> List.filter(String.startsWith(prefix = "3x3\t"));
                Assert.assertEq(expected = 5, List.length(rubik));
                foreach (line <- rubik)
                    checkRubikVector(line)
        }

    ///
    /// One 3x3 line: size, seed, scramble, stickers.
    ///
    def checkRubikVector(line: String): Unit \ Assert =
        match String.splitOn(substr = "\t", line) {
            case _ :: _ :: scramble :: expected :: Nil => match Cube.parse(scramble) {
                case Err(why) => Assert.assertEq(expected = "moves", "${why} in ${scramble}")
                case Ok(ms)   => Assert.assertEq(
                    expected = Some(expected),
                    Cube.Rubik.replay(Cube.Rubik.solved(), ms)
                        |> Option.map(s -> stickerString(Cube.Rubik.Stickers.toFacelets(s))))
            }
            case _ => Assert.assertEq(expected = "four fields", line)
        }

    ///
    /// One line: size, seed, scramble, stickers.
    ///
    def checkVector(line: String): Unit \ Assert =
        match String.splitOn(substr = "\t", line) {
            case _ :: _ :: scramble :: expected :: Nil => match Cube.parse(scramble) {
                case Err(why) => Assert.assertEq(expected = "moves", "${why} in ${scramble}")
                case Ok(ms)   => Assert.assertEq(
                    expected = Some(expected),
                    replay(solved(), ms) |> Option.map(s -> stickerString(toFacelets(s))))
            }
            case _ => Assert.assertEq(expected = "four fields", line)
        }

    ///
    /// The sticker string of a cube.
    ///
    def stickerString(f: Cube.Solver.Facelets): String =
        Cube.Solver.stickersOf(f)
            |> Vector.toList
            |> List.map(s -> Char.toString(Cube.faceToken(s)))
            |> List.join("")

}
