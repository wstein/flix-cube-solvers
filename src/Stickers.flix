///
/// The sticker boundary: cubies out, cubies back in.
///
/// Facelets are how a cube arrives from the outside world -- a camera, a
/// scanner, a text field -- and cubies are how it is solved. The two tables
/// below are the whole translation, and they are the standard ones: read
/// `cornerFacelet[i]` as the three sticker positions that meet at cubicle
/// `i`, U or D first, and `cornerColour[j]` as the colours corner `j` shows in
/// that same order when it is home.
///
mod Cube.Pocket.Stickers {

    use Cube.{Face, Kind}
    use Cube.Pocket.{State, permutation, twist}
    use Cube.Solver.{Facelets, Invalid}

    ///
    /// Where the three stickers of each cubicle sit in a 24-sticker vector.
    ///
    /// Stickers run U, R, F, D, L, B, four per face, row major -- so `U` is
    /// 0 to 3 and `B` is 20 to 23. The order within a cubicle matters: the
    /// U or D sticker is first, which is what makes twist readable.
    ///
    def cornerFacelet(): Vector[Vector[Int32]] = Vector#{
        Vector#{3, 4, 9},
        Vector#{2, 8, 17},
        Vector#{0, 16, 21},
        Vector#{1, 20, 5},
        Vector#{13, 11, 6},
        Vector#{12, 19, 10},
        Vector#{14, 23, 18},
        Vector#{15, 7, 22}
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
    /// The stickers a cubie state shows.
    ///
    /// Total, and checked by construction: a state that came from cubies has
    /// every colour exactly four times, so there is nothing here to reject.
    ///
    pub def toFacelets(s: State): Facelets =
        region rc {
            let out = Array.repeat(rc, 24, Face.U);
            foreach (i <- List.range(0, 8)) {
                let corner = Vector.get(i, permutation(s));
                let ori = Vector.get(i, twist(s));
                foreach (n <- List.range(0, 3)) {
                    let at = Vector.get((n + ori) `Int32.modulo` 3, Vector.get(i, cornerFacelet()));
                    Array.put(Vector.get(n, Vector.get(corner, cornerColour())), at, out)
                }
            };
            Facelets.Checked(Kind.Cube2x2, Array.toVector(out))
        }

    ///
    /// The cubie state a sticker vector describes, or why it describes none.
    ///
    /// Three things can go wrong that counting stickers cannot see: a corner
    /// whose colours belong to no cubie, the same corner appearing twice, and
    /// a total twist that no sequence of moves can produce. The last is the
    /// classic one -- a single corner rotated in place is a perfectly
    /// well-coloured cube that cannot exist.
    ///
    pub def ofFacelets(f: Facelets): Result[Invalid, State] =
        if (Cube.Solver.kindOf(f) != Kind.Cube2x2)
            Err(Invalid.WrongKind(Cube.Solver.kindOf(f)))
        else
            let stickers = Cube.Solver.stickersOf(f);
            forM (corners <- Result.traverse(i -> corner(stickers, i), List.range(0, 8));
                  state <- assemble(corners))
                yield state

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
                match matching(sideA, sideB) {
                    case None    => Err(Invalid.NotAPiece(i))
                    case Some(c) => Ok((c, ori))
                }
        }

    ///
    /// Which of a cubicle's three stickers carries the U or D colour.
    ///
    /// Every corner has exactly one, which is what makes twist a number
    /// rather than a convention.
    ///
    def upOrDown(stickers: Vector[Face], at: Vector[Int32]): Option[Int32] =
        Vector.findIndexOf(p -> match Vector.get(p, stickers) {
            case Face.U => true
            case Face.D => true
            case _      => false
        }, at)

    ///
    /// The corner showing these two side colours, in this order.
    ///
    def matching(sideA: Face, sideB: Face): Option[Int32] =
        Vector.findIndexOf(colours ->
            Vector.get(1, colours) == sideA and Vector.get(2, colours) == sideB,
            cornerColour())

    ///
    /// A state from eight decoded corners, if they can be one.
    ///
    def assemble(corners: List[(Int32, Int32)]): Result[Invalid, State] =
        let perm = corners |> List.map(fst) |> List.toVector;
        let ori = corners |> List.map(snd) |> List.toVector;
        if (Vector.length(Vector.toList(perm) |> List.distinct |> List.toVector) != 8)
            Err(Invalid.Unsolvable("the same corner appears in two places"))
        else if (Vector.sum(ori) `Int32.modulo` 3 != 0)
            Err(Invalid.Unsolvable("the corner twists do not sum to a whole turn"))
        else if (Vector.get(6, perm) != 6 or Vector.get(6, ori) != 0)
            Err(Invalid.Unsolvable("the DBL corner is not home; this model solves up to rotation and holds it still"))
        else
            Ok(State.State(perm, ori))

}
