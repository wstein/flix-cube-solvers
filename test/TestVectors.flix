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

    use CubeSolvers.Kind
    use CubeSolvers.Pocket.{replay, solved}
    use CubeSolvers.Pocket.Stickers.toFacelets
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
            case _ :: _ :: scramble :: expected :: Nil => match CubeSolvers.parse(scramble) {
                case Err(why) => Assert.assertEq(expected = "moves", "${why} in ${scramble}")
                case Ok(ms)   => Assert.assertEq(
                    expected = Some(expected),
                    CubeSolvers.Rubik.replay(CubeSolvers.Rubik.solved(), ms)
                        |> Option.map(s -> stickerString(CubeSolvers.Rubik.Stickers.toFacelets(s))))
            }
            case _ => Assert.assertEq(expected = "four fields", line)
        }

    ///
    /// Every vector of every size, through the sticker engine.
    ///
    /// The cubie models cover 2x2 and 3x3 only. This one covers all four
    /// sizes with one definition, so a wrong 4x4 or 5x5 wide turn shows up
    /// here rather than in whatever is built on it later.
    ///
    @Test
    def everyVectorOfEverySizeTurnsToItsRecordedCube(): Unit \ Assert + IO =
        runWithIO(() -> checkEverySize())

    ///
    /// All twenty vectors, replayed at sticker level.
    ///
    def checkEverySize(): Unit \ Assert + FileRead =
        match FileRead.readLines(path()) {
            case Err(_)    => Assert.assertEq(expected = "the vectors", "cannot read ${path()}")
            case Ok(lines) =>
                let vectors = lines |> List.filter(l ->
                    not (String.startsWith(prefix = "#", l) or String.isEmpty(String.trim(l))));
                Assert.assertEq(expected = 20, List.length(vectors));
                foreach (line <- vectors)
                    checkAnySize(line)
        }

    ///
    /// One line of any size: replay it and compare stickers.
    ///
    def checkAnySize(line: String): Unit \ Assert =
        match String.splitOn(substr = "\t", line) {
            case size :: _ :: scramble :: expected :: Nil =>
                match (kindOfLabel(size), CubeSolvers.parse(scramble)) {
                    case (Some(k), Ok(ms)) => Assert.assertEq(
                        expected = Ok(expected),
                        CubeSolvers.Turning.replay(CubeSolvers.Solver.solved(k), ms)
                            |> Result.map(stickerString)
                            |> Result.mapErr(e -> "${e}"))
                    case _ => Assert.assertEq(expected = "a size and a scramble", line)
                }
            case _ => Assert.assertEq(expected = "four fields", line)
        }

    ///
    /// The kind a label like `4x4` names.
    ///
    def kindOfLabel(label: String): Option[Kind] =
        String.toList(label) |> List.head |> Option.flatMap(c -> Char.digit(radix = 10, c)) |> Option.flatMap(CubeSolvers.kindOfOrder)

    ///
    /// One line: size, seed, scramble, stickers.
    ///
    def checkVector(line: String): Unit \ Assert =
        match String.splitOn(substr = "\t", line) {
            case _ :: _ :: scramble :: expected :: Nil => match CubeSolvers.parse(scramble) {
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
    def stickerString(f: CubeSolvers.Solver.Facelets): String =
        CubeSolvers.Solver.stickersOf(f)
            |> Vector.toList
            |> List.map(s -> Char.toString(CubeSolvers.faceToken(s)))
            |> List.join("")

}
