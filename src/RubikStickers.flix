///
/// The 3x3 sticker boundary: cubies out, cubies back in.
///
/// A 3x3 has fixed centres, so unlike the pocket cube there is no freedom to
/// spend: the cube arrives in the orientation its centres declare, and a state
/// whose centres are not the six faces in order is refused rather than
/// rotated.
///
mod Cube.Rubik.Stickers {

    use Cube.{Face, Kind}
    use Cube.Rubik.{State, corners, twists, edges, flips}
    use Cube.Solver.{Facelets, Invalid}

    ///
    /// The three sticker positions meeting at each corner cubicle, U or D
    /// first.
    ///
    /// Stickers run U, R, F, D, L, B, nine per face, row major.
    ///
    def cornerFacelet(): Vector[Vector[Int32]] = Vector#{
        Vector#{8, 9, 20},
        Vector#{6, 18, 38},
        Vector#{0, 36, 47},
        Vector#{2, 45, 11},
        Vector#{29, 26, 15},
        Vector#{27, 44, 24},
        Vector#{33, 53, 42},
        Vector#{35, 17, 51}
    }

    ///
    /// The colours each corner shows, in the same order as its facelets.
    ///
    def cornerColour(): Vector[Vector[Face]] = Vector#{
        Vector#{Face.U, Face.R, Face.F},
        Vector#{Face.U, Face.F, Face.L},
        Vector#{Face.U, Face.L, Face.B},
        Vector#{Face.U, Face.B, Face.R},
        Vector#{Face.D, Face.F, Face.R},
        Vector#{Face.D, Face.L, Face.F},
        Vector#{Face.D, Face.B, Face.L},
        Vector#{Face.D, Face.R, Face.B}
    }

    ///
    /// The two sticker positions meeting at each edge cubicle.
    ///
    def edgeFacelet(): Vector[Vector[Int32]] = Vector#{
        Vector#{5, 10},
        Vector#{7, 19},
        Vector#{3, 37},
        Vector#{1, 46},
        Vector#{32, 16},
        Vector#{28, 25},
        Vector#{30, 43},
        Vector#{34, 52},
        Vector#{23, 12},
        Vector#{21, 41},
        Vector#{50, 39},
        Vector#{48, 14}
    }

    ///
    /// The colours each edge shows, in the same order as its facelets.
    ///
    def edgeColour(): Vector[Vector[Face]] = Vector#{
        Vector#{Face.U, Face.R},
        Vector#{Face.U, Face.F},
        Vector#{Face.U, Face.L},
        Vector#{Face.U, Face.B},
        Vector#{Face.D, Face.R},
        Vector#{Face.D, Face.F},
        Vector#{Face.D, Face.L},
        Vector#{Face.D, Face.B},
        Vector#{Face.F, Face.R},
        Vector#{Face.F, Face.L},
        Vector#{Face.B, Face.L},
        Vector#{Face.B, Face.R}
    }

    ///
    /// Where each face's centre sticker sits.
    ///
    def centre(): Vector[Int32] = Vector#{4, 13, 22, 31, 40, 49}

    ///
    /// The six faces in the order stickers are given in.
    ///
    def faceOrder(): Vector[Face] =
        Vector#{Face.U, Face.R, Face.F, Face.D, Face.L, Face.B}

    ///
    /// The stickers a cubie state shows.
    ///
    pub def toFacelets(s: State): Facelets =
        region rc {
            let out = Array.repeat(rc, 54, Face.U);
            foreach (f <- List.range(0, 6))
                Array.put(Vector.get(f, faceOrder()), Vector.get(f, centre()), out);
            foreach (i <- List.range(0, 8)) {
                let corner = Vector.get(i, corners(s));
                let ori = Vector.get(i, twists(s));
                foreach (n <- List.range(0, 3)) {
                    let at = Vector.get((n + ori) `Int32.modulo` 3, Vector.get(i, cornerFacelet()));
                    Array.put(Vector.get(n, Vector.get(corner, cornerColour())), at, out)
                }
            };
            foreach (i <- List.range(0, 12)) {
                let edge = Vector.get(i, edges(s));
                let flip = Vector.get(i, flips(s));
                foreach (n <- List.range(0, 2)) {
                    let at = Vector.get((n + flip) `Int32.modulo` 2, Vector.get(i, edgeFacelet()));
                    Array.put(Vector.get(n, Vector.get(edge, edgeColour())), at, out)
                }
            };
            Facelets.Checked(Kind.Cube3x3, Array.toVector(out))
        }

    ///
    /// The cubie state a sticker vector describes, or why it describes none.
    ///
    pub def ofFacelets(f: Facelets): Result[Invalid, State] =
        if (Cube.Solver.kindOf(f) != Kind.Cube3x3)
            Err(Invalid.WrongKind(Cube.Solver.kindOf(f)))
        else
            let stickers = Cube.Solver.stickersOf(f);
            forM (_ <- centresAreInOrder(stickers);
                  cs <- Result.traverse(i -> corner(stickers, i), List.range(0, 8));
                  es <- Result.traverse(i -> edge(stickers, i), List.range(0, 12));
                  state <- assemble(cs, es))
                yield state

    ///
    /// A 3x3 whose centres are not the six faces in order is a rotated cube,
    /// not a different one -- but this model does not rotate, so it says so.
    ///
    def centresAreInOrder(stickers: Vector[Face]): Result[Invalid, Unit] =
        let wrong = List.range(0, 6) |> List.findLeft(f ->
            Vector.get(Vector.get(f, centre()), stickers) != Vector.get(f, faceOrder()));
        match wrong {
            case None    => Ok(())
            case Some(_) => Err(Invalid.Unsolvable("the centres are not U R F D L B; the cube is rotated"))
        }

    ///
    /// The corner at cubicle `i`, as which corner it is and how far twisted.
    ///
    def corner(stickers: Vector[Face], i: Int32): Result[Invalid, (Int32, Int32)] =
        let at = Vector.get(i, cornerFacelet());
        match upOrDown(stickers, at) {
            case None      => Err(Invalid.NotAPiece(i))
            case Some(ori) =>
                let sideA = Vector.get(Vector.get((ori + 1) `Int32.modulo` 3, at), stickers);
                let sideB = Vector.get(Vector.get((ori + 2) `Int32.modulo` 3, at), stickers);
                match Vector.findIndexOf(colours ->
                        Vector.get(1, colours) == sideA and Vector.get(2, colours) == sideB,
                        cornerColour()) {
                    case None    => Err(Invalid.NotAPiece(i))
                    case Some(c) => Ok((c, ori))
                }
        }

    ///
    /// The edge at cubicle `i`, as which edge it is and whether it is flipped.
    ///
    def edge(stickers: Vector[Face], i: Int32): Result[Invalid, (Int32, Int32)] =
        let at = Vector.get(i, edgeFacelet());
        let first = Vector.get(Vector.get(0, at), stickers);
        let second = Vector.get(Vector.get(1, at), stickers);
        match Vector.findIndexOf(colours ->
                Vector.get(0, colours) == first and Vector.get(1, colours) == second, edgeColour()) {
            case Some(e) => Ok((e, 0))
            case None    => match Vector.findIndexOf(colours ->
                    Vector.get(0, colours) == second and Vector.get(1, colours) == first, edgeColour()) {
                case Some(e) => Ok((e, 1))
                case None    => Err(Invalid.NotAPiece(8 + i))
            }
        }

    ///
    /// Whether a corner cubicle's U or D sticker is first, second or third.
    ///
    def upOrDown(stickers: Vector[Face], at: Vector[Int32]): Option[Int32] =
        Vector.findIndexOf(p -> match Vector.get(p, stickers) {
            case Face.U => true
            case Face.D => true
            case _      => false
        }, at)

    ///
    /// A state from eight corners and twelve edges, if they can be one.
    ///
    /// Three laws decide it, and every one of them describes a cube that is
    /// perfectly well coloured and still impossible: the twists must sum to a
    /// whole turn, the flips must pair up, and the two permutations must have
    /// the same parity, because every move is a four-cycle of corners and a
    /// four-cycle of edges at once.
    ///
    def assemble(cs: List[(Int32, Int32)], es: List[(Int32, Int32)]): Result[Invalid, State] =
        let cp = cs |> List.map(fst) |> List.toVector;
        let co = cs |> List.map(snd) |> List.toVector;
        let ep = es |> List.map(fst) |> List.toVector;
        let eo = es |> List.map(snd) |> List.toVector;
        if (not isPermutation(cp))
            Err(Invalid.Unsolvable("the same corner appears in two places"))
        else if (not isPermutation(ep))
            Err(Invalid.Unsolvable("the same edge appears in two places"))
        else if (Vector.sum(co) `Int32.modulo` 3 != 0)
            Err(Invalid.Unsolvable("the corner twists do not sum to a whole turn"))
        else if (Vector.sum(eo) `Int32.modulo` 2 != 0)
            Err(Invalid.Unsolvable("an odd number of edges is flipped"))
        else if (parity(cp) != parity(ep))
            Err(Invalid.Unsolvable("two pieces are swapped; corner and edge parity disagree"))
        else
            Ok(State.State(cp, co, ep, eo))

    ///
    /// Whether every index appears exactly once.
    ///
    def isPermutation(v: Vector[Int32]): Bool =
        (v |> Vector.toList |> List.distinct |> List.length) == Vector.length(v)

    ///
    /// The parity of a permutation: 0 for even, 1 for odd.
    ///
    /// Counted as inversions, which is the definition, not an optimisation.
    ///
    def parity(v: Vector[Int32]): Int32 =
        let n = Vector.length(v);
        let inversions = List.range(0, n) |> List.sumWith(i ->
            List.range(i + 1, n) |> List.count(j -> Vector.get(i, v) > Vector.get(j, v)));
        inversions `Int32.modulo` 2

}
