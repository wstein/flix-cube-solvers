///
/// A demonstration entry point.
///
/// Two cubes, two engines, one contract: a pocket cube solved optimally from
/// a complete table, and a 3x3 solved in two phases.
///
def main(): Unit \ IO =
    let pocket = "FUBRDRLBDFLDUFDLURLFBRUB";
    match read(CubeSolvers.Kind.Cube2x2, pocket) {
        case None       => println("2x2: not a cube")
        case Some(cube) =>
            println("2x2 ${pocket}");
            match CubeSolvers.Solver.search(CubeSolvers.Pocket.Engine.standard(), cube, CubeSolvers.Solver.defaultOptions()) {
                case CubeSolvers.Solver.Outcome.Solved(ms) =>
                    println("    ${CubeSolvers.render(ms)} (${List.length(ms)} moves, shortest)")
                case other => println("    ${other}")
            }
    };
    let scramble = "R U R' U' R' F R2 U' R' U' R U R' F'";
    println("3x3 scrambled by ${scramble}");
    match CubeSolvers.parse(scramble) |> Result.toOption |> Option.flatMap(ms -> CubeSolvers.Rubik.replay(CubeSolvers.Rubik.solved(), ms)) {
        case None    => println("    not a scramble")
        case Some(s) => match CubeSolvers.Solver.search(
                CubeSolvers.Rubik.Engine.prepared(),
                CubeSolvers.Rubik.Stickers.toFacelets(s),
                CubeSolvers.Solver.defaultOptions()) {
            case CubeSolvers.Solver.Outcome.Solved(ms) =>
                println("    ${CubeSolvers.render(ms)} (${List.length(ms)} moves)")
            case other => println("    ${other}")
        }
    }

///
/// A sticker string as a cube.
///
def read(kind: CubeSolvers.Kind, text: String): Option[CubeSolvers.Solver.Facelets] =
    let stickers = text |> String.toList |> List.filterMap(CubeSolvers.faceOfToken) |> List.toVector;
    CubeSolvers.Solver.facelets(kind, stickers) |> Result.toOption
