///
/// A demonstration entry point.
///
/// The package is a library; this exists so `./flixw run` shows something,
/// and so the whole path -- stickers in, cubies, table, search, moves out --
/// is exercised outside its own tests.
///
def main(): Unit \ IO =
    let text = "FUBRDRLBDFLDUFDLURLFBRUB";
    let stickers = text |> String.toList |> List.filterMap(Cube.faceOfToken) |> List.toVector;
    match Cube.Solver.facelets(Cube.Kind.Cube2x2, stickers) {
        case Err(why) => println("not a cube: ${why}")
        case Ok(cube) =>
            println("cube:     ${text}");
            let engine = Cube.Pocket.Engine.standard();
            match Cube.Solver.search(engine, cube, Cube.Solver.defaultOptions()) {
                case Cube.Solver.Outcome.Solved(sol) =>
                    println("solution: ${Cube.render(sol)} (${List.length(sol)} moves, shortest)")
                case other => println("solution: ${other}")
            }
    }
