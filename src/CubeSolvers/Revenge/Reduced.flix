///
/// A reduced 4x4, read as the 3x3 it has become.
///
/// Once the centres are solved and every place holds a matching pair, a 4x4
/// turns like a 3x3: the outer layers are its faces and the two middle layers
/// are its middle slice. So the cube can be handed to the 3x3 engine, and the
/// moves that come back are the same moves.
///
/// Nothing here checks that the cube really is reduced. The phases that ran
/// before it are what guarantee that, including the parity bit they carried:
/// without it the reduction could present a position no 3x3 can reach, and
/// this would hand the engine a cube it must refuse.
///
mod CubeSolvers.Revenge.Reduced {

    use CubeSolvers.Kind
    use CubeSolvers.Solver.{Facelets, Invalid}

    ///
    /// The 3x3 a reduced 4x4 stands for.
    ///
    /// Each of the 3x3's fifty-four stickers is one of the 4x4's ninety-six:
    /// a corner is a corner, an edge is either wing of its pair, and a centre
    /// is any of the four. Rows and columns map 0, 1, 2 to 0, 1, 3 -- the
    /// middle two of the 4x4 having become one.
    ///
    pub def asRubik(cube: Facelets): Result[Invalid, Facelets] =
        if (CubeSolvers.Solver.kindOf(cube) != Kind.Cube4x4)
            Err(Invalid.WrongKind(CubeSolvers.Solver.kindOf(cube)))
        else
            let stickers = CubeSolvers.Solver.stickersOf(cube);
            let picked = Vector.range(0, 54) |> Vector.map(i ->
                let face = i / 9;
                let rest = i `Int32.modulo` 9;
                Vector.get(face * 16 + wider(rest / 3) * 4 + wider(rest `Int32.modulo` 3), stickers));
            CubeSolvers.Solver.facelets(Kind.Cube3x3, picked)

    ///
    /// Which row or column of the 4x4 stands for one of the 3x3's.
    ///
    def wider(i: Int32): Int32 = match i {
        case 0 => 0
        case 1 => 1
        case _ => 3
    }

}
