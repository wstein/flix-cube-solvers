///
/// A demonstration entry point.
///
/// Two cubes, two engines, one contract: a pocket cube solved optimally from
/// a complete table, and a 3x3 solved in two phases.
///
def main(): Unit \ IO =
    let pocket = "FUBRDRLBDFLDUFDLURLFBRUB";
    match read(Cube.Kind.Cube2x2, pocket) {
        case None       => println("2x2: not a cube")
        case Some(cube) =>
            println("2x2 ${pocket}");
            match Cube.Solver.search(Cube.Pocket.Engine.standard(), cube, Cube.Solver.defaultOptions()) {
                case Cube.Solver.Outcome.Solved(ms) =>
                    println("    ${Cube.render(ms)} (${List.length(ms)} moves, shortest)")
                case other => println("    ${other}")
            }
    };
    let scramble = "R U R' U' R' F R2 U' R' U' R U R' F'";
    println("3x3 scrambled by ${scramble}");
    match Cube.parse(scramble) |> Result.toOption |> Option.flatMap(ms -> Cube.Rubik.replay(Cube.Rubik.solved(), ms)) {
        case None    => println("    not a scramble")
        case Some(s) => match Cube.Solver.search(
                Cube.Rubik.Engine.prepared(),
                Cube.Rubik.Stickers.toFacelets(s),
                Cube.Solver.defaultOptions()) {
            case Cube.Solver.Outcome.Solved(ms) =>
                println("    ${Cube.render(ms)} (${List.length(ms)} moves)")
            case other => println("    ${other}")
        }
    }

///
/// A sticker string as a cube.
///
def read(kind: Cube.Kind, text: String): Option[Cube.Solver.Facelets] =
    let stickers = text |> String.toList |> List.filterMap(Cube.faceOfToken) |> List.toVector;
    Cube.Solver.facelets(kind, stickers) |> Result.toOption
