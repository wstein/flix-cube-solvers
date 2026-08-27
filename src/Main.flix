///
/// A demonstration entry point.
///
/// The package is a library; this exists so `./flixw run` shows something,
/// and so the engine is exercised outside its own tests.
///
def main(): Unit \ IO =
    let scramble =
        Cube.move(Cube.Face.R, 1, Cube.Turn.Cw) ::
        Cube.move(Cube.Face.U, 1, Cube.Turn.Ccw) ::
        Cube.move(Cube.Face.F, 1, Cube.Turn.Half) ::
        Cube.move(Cube.Face.R, 1, Cube.Turn.Ccw) :: Nil;
    println("scramble: ${Cube.render(scramble)}");
    let depth = 5;
    let tbl = Cube.Pocket.Search.table(depth);
    println("table:    ${Map.size(tbl)} states within ${depth} moves of solved");
    match Cube.Pocket.replay(Cube.Pocket.solved(), scramble) {
        case None    => println("scramble: not a pocket cube sequence")
        case Some(s) => match Cube.Pocket.Search.solution(tbl, depth, s, 11) {
            case None      => println("solution: none within 11 moves")
            case Some(sol) => println("solution: ${Cube.render(sol)} (${List.length(sol)} moves, shortest)")
        }
    }
