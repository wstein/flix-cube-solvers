///
/// Turning a cube at sticker level, for any size.
///
/// The engines each carry a cubie model, because that is what a search needs.
/// This is the other thing: a plain permutation of stickers, defined once for
/// every size rather than once per puzzle. It is what checks a solution, and
/// what a 4x4 or 5x5 can be built on before it has an engine of its own.
///
/// Nothing here is tabulated. A sticker knows where it is in space and which
/// way it faces; a turn rotates both, and the sticker lands wherever that
/// puts it. One definition covers 2x2 through 5x5, and inner slices come out
/// of it for free.
///
mod CubeSolvers.Turning {

    use CubeSolvers.{Face, Kind, Move, MoveSequence}
    use CubeSolvers.Solver.{Facelets, Invalid}

    ///
    /// The cube after `m`, or why the move does not apply to it.
    ///
    pub def apply(cube: Facelets, m: Move): Result[Invalid, Facelets] =
        let kind = CubeSolvers.Solver.kindOf(cube);
        match CubeSolvers.validate(kind, m) {
            case Err(why) => Err(Invalid.Unsolvable(why))
            case Ok(_)    =>
                let turned = Vector.range(0, CubeSolvers.quarterTurns(CubeSolvers.turn(m)))
                    |> Vector.foldLeft(acc -> _ ->
                        quarter(kind, acc, CubeSolvers.face(m), CubeSolvers.depth(m)),
                        CubeSolvers.Solver.stickersOf(cube));
                Ok(Facelets.Checked(kind, turned))
        }

    ///
    /// The cube after the whole sequence, applied left to right.
    ///
    pub def replay(cube: Facelets, ms: MoveSequence): Result[Invalid, Facelets] =
        List.foldLeft((acc, m) -> Result.flatMap(c -> apply(c, m), acc), Ok(cube), ms)

    ///
    /// Whether every face shows one colour.
    ///
    pub def isSolved(cube: Facelets): Bool =
        let kind = CubeSolvers.Solver.kindOf(cube);
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let perFace = CubeSolvers.order(kind) * CubeSolvers.order(kind);
        List.range(0, 6) |> List.forAll(f ->
            let first = Vector.get(f * perFace, stickers);
            List.range(0, perFace) |> List.forAll(i -> Vector.get(f * perFace + i, stickers) == first))

    ///
    /// Where each sticker goes when `m` is made, as slot indices.
    ///
    /// The move as data. A coordinate over some of the stickers -- the
    /// centres of a 4x4, say -- needs to know how a turn moves those slots
    /// and nothing else, and re-deriving that from the geometry every time is
    /// what makes table building slow.
    ///
    pub def permutation(kind: Kind, m: Move): Result[Invalid, Vector[Int32]] =
        match CubeSolvers.validate(kind, m) {
            case Err(why) => Err(Invalid.Unsolvable(why))
            case Ok(_)    =>
                let n = CubeSolvers.order(kind);
                let once = quarterPermutation(kind, CubeSolvers.face(m), CubeSolvers.depth(m));
                Ok(Vector.range(0, CubeSolvers.quarterTurns(CubeSolvers.turn(m)))
                    |> Vector.foldLeft(acc -> _ -> Vector.map(i -> Vector.get(i, once), acc),
                        Vector.range(0, 6 * n * n)))
        }

    ///
    /// Where each sticker goes in one quarter turn of `depth` layers of
    /// `face`.
    ///
    def quarterPermutation(kind: Kind, face: Face, depth: Int32): Vector[Int32] =
        let n = CubeSolvers.order(kind);
        let m = n - 1;
        let axis = outward(face);
        let threshold = m - 2 * (depth - 1);
        Vector.range(0, 6 * n * n) |> Vector.map(i ->
            let (side, at) = place(n, i);
            let here = position(n, side, at);
            if (dot(here, axis) >= threshold)
                index(n, spinFace(face, side), spin(face, here))
            else
                i)

    ///
    /// One quarter turn of `depth` layers of `face`.
    ///
    def quarter(kind: Kind, stickers: Vector[Face], face: Face, depth: Int32): Vector[Face] =
        let n = CubeSolvers.order(kind);
        let m = n - 1;
        let axis = outward(face);
        let threshold = m - 2 * (depth - 1);
        region rc {
            let out = Array.repeat(rc, 6 * n * n, Face.U);
            foreach (i <- List.range(0, 6 * n * n)) {
                let (side, at) = place(n, i);
                let here = position(n, side, at);
                let target =
                    if (dot(here, axis) >= threshold)
                        index(n, spinFace(face, side), spin(face, here))
                    else
                        i;
                Array.put(Vector.get(i, stickers), target, out)
            };
            Array.toVector(out)
        }

    ///
    /// Which face a sticker belongs to, and where on it: row then column.
    ///
    def place(n: Int32, i: Int32): (Face, (Int32, Int32)) =
        let perFace = n * n;
        let f = i / perFace;
        let rest = i `Int32.modulo` perFace;
        (faceOfIndex(f), (rest / n, rest `Int32.modulo` n))

    ///
    /// Stickers run U, R, F, D, L, B.
    ///
    def faceOfIndex(f: Int32): Face = match f {
        case 0 => Face.U
        case 1 => Face.R
        case 2 => Face.F
        case 3 => Face.D
        case 4 => Face.L
        case _ => Face.B
    }

    ///
    /// The place of a face in that order.
    ///
    def indexOfFace(f: Face): Int32 = match f {
        case Face.U => 0
        case Face.R => 1
        case Face.F => 2
        case Face.D => 3
        case Face.L => 4
        case Face.B => 5
    }

    ///
    /// Where a sticker sits, in coordinates doubled so the centre is zero and
    /// every sticker has whole numbers.
    ///
    /// The layout is the one the sticker string uses: on U, row 0 touches B
    /// and column 0 touches L; on the side faces row 0 is the top, and column
    /// 0 touches whichever face precedes it going L, F, R, B.
    ///
    def position(n: Int32, f: Face, at: (Int32, Int32)): (Int32, Int32, Int32) =
        let m = n - 1;
        let (r, c) = at;
        match f {
            case Face.U => (2 * c - m, m, 2 * r - m)
            case Face.R => (m, m - 2 * r, m - 2 * c)
            case Face.F => (2 * c - m, m - 2 * r, m)
            case Face.D => (2 * c - m, -m, m - 2 * r)
            case Face.L => (-m, m - 2 * r, 2 * c - m)
            case Face.B => (m - 2 * c, m - 2 * r, -m)
        }

    ///
    /// The sticker at a place, given which way it faces.
    ///
    def index(n: Int32, f: Face, p: (Int32, Int32, Int32)): Int32 =
        let m = n - 1;
        let (x, y, z) = p;
        let (r, c) = match f {
            case Face.U => ((z + m) / 2, (x + m) / 2)
            case Face.R => ((m - y) / 2, (m - z) / 2)
            case Face.F => ((m - y) / 2, (x + m) / 2)
            case Face.D => ((m - z) / 2, (x + m) / 2)
            case Face.L => ((m - y) / 2, (z + m) / 2)
            case Face.B => ((m - y) / 2, (m - x) / 2)
        };
        indexOfFace(f) * n * n + r * n + c

    ///
    /// The direction a face looks.
    ///
    def outward(f: Face): (Int32, Int32, Int32) = match f {
        case Face.U => (0, 1, 0)
        case Face.R => (1, 0, 0)
        case Face.F => (0, 0, 1)
        case Face.D => (0, -1, 0)
        case Face.L => (-1, 0, 0)
        case Face.B => (0, 0, -1)
    }

    ///
    /// A quarter turn clockwise about a face, seen from outside it.
    ///
    def spin(about: Face, p: (Int32, Int32, Int32)): (Int32, Int32, Int32) =
        let (x, y, z) = p;
        match about {
            case Face.U => (-z, y, x)
            case Face.D => (z, y, -x)
            case Face.R => (x, z, -y)
            case Face.L => (x, -z, y)
            case Face.F => (y, -x, z)
            case Face.B => (-y, x, z)
        }

    ///
    /// A direction turned into the face that looks that way.
    ///
    def spinFace(about: Face, f: Face): Face = faceOfDirection(spin(about, outward(f)))

    ///
    /// The face looking in the given direction.
    ///
    def faceOfDirection(d: (Int32, Int32, Int32)): Face = match d {
        case (0, 1, 0)  => Face.U
        case (1, 0, 0)  => Face.R
        case (0, 0, 1)  => Face.F
        case (0, -1, 0) => Face.D
        case (-1, 0, 0) => Face.L
        case _          => Face.B
    }

    ///
    /// How far along a direction a point lies.
    ///
    def dot(p: (Int32, Int32, Int32), q: (Int32, Int32, Int32)): Int32 =
        let (a, b, c) = p;
        let (d, e, f) = q;
        a * d + b * e + c * f

}
