///
/// The 3x3x3, as cubies rather than stickers.
///
/// Eight corners and twelve edges, each somewhere and each turned some way.
/// Unlike the pocket cube nothing is pinned: a 3x3 has fixed centres, so the
/// orientation of the whole cube is already decided and all six faces turn.
///
/// The tables below describe how a quarter turn permutes and twists those
/// pieces. They are the standard cubie model of the cube -- facts about the
/// puzzle, published in every treatment of it -- and they are checked here
/// against vectors produced by an independent implementation rather than
/// trusted.
///
mod Cube.Rubik {

    use Cube.{Face, Move, Turn}

    ///
    /// A cube state: corner positions and twists, then edge positions and
    /// flips.
    ///
    /// Corners are indexed URF, UFL, ULB, UBR, DFR, DLF, DBL, DRB and edges
    /// UR, UF, UL, UB, DR, DF, DL, DB, FR, FL, BL, BR. `corners[i]` is the
    /// corner now in cubicle `i`, twisted `twists[i]` thirds clockwise;
    /// `edges[i]` and `flips[i]` say the same for edges, where a flip is only
    /// ever 0 or 1.
    ///
    pub enum State with Eq, Order, ToString {
        case State(Vector[Int32], Vector[Int32], Vector[Int32], Vector[Int32])
    }

    ///
    /// The solved cube: every piece home, none turned.
    ///
    pub def solved(): State =
        State.State(Vector.range(0, 8), Vector.repeat(8, 0), Vector.range(0, 12), Vector.repeat(12, 0))

    ///
    /// Which corner sits in each cubicle.
    ///
    pub def corners(s: State): Vector[Int32] =
        let State.State(cp, _, _, _) = s;
        cp

    ///
    /// How far the corner in each cubicle is twisted.
    ///
    pub def twists(s: State): Vector[Int32] =
        let State.State(_, co, _, _) = s;
        co

    ///
    /// Which edge sits in each cubicle.
    ///
    pub def edges(s: State): Vector[Int32] =
        let State.State(_, _, ep, _) = s;
        ep

    ///
    /// Whether the edge in each cubicle is flipped.
    ///
    pub def flips(s: State): Vector[Int32] =
        let State.State(_, _, _, eo) = s;
        eo

    ///
    /// A 3x3 turns all six faces.
    ///
    pub def faces(): Vector[Face] =
        Vector#{Face.U, Face.R, Face.F, Face.D, Face.L, Face.B}

    ///
    /// Every move a 3x3 accepts: six faces, three turns.
    ///
    pub def moves(): Vector[Move] =
        faces() |> Vector.flatMap(f ->
            Vector#{Turn.Cw, Turn.Half, Turn.Ccw} |> Vector.map(t -> Cube.move(f, 1, t)))

    ///
    /// How a quarter turn of `f` moves the pieces.
    ///
    /// Four vectors: where each corner comes from, the twist it gains, where
    /// each edge comes from, and whether it flips. Read `corners[i]` as "the
    /// corner that ends up in cubicle `i` comes from cubicle `corners[i]`".
    ///
    /// Only `F` and `B` flip edges, which is the convention that makes edge
    /// orientation mean "solvable without F or B quarter turns".
    ///
    def quarterTurn(f: Face): (Vector[Int32], Vector[Int32], Vector[Int32], Vector[Int32]) = match f {
        case Face.U => (
            Vector#{3, 0, 1, 2, 4, 5, 6, 7},
            Vector#{0, 0, 0, 0, 0, 0, 0, 0},
            Vector#{3, 0, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11},
            Vector#{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0})
        case Face.R => (
            Vector#{4, 1, 2, 0, 7, 5, 6, 3},
            Vector#{2, 0, 0, 1, 1, 0, 0, 2},
            Vector#{8, 1, 2, 3, 11, 5, 6, 7, 4, 9, 10, 0},
            Vector#{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0})
        case Face.F => (
            Vector#{1, 5, 2, 3, 0, 4, 6, 7},
            Vector#{1, 2, 0, 0, 2, 1, 0, 0},
            Vector#{0, 9, 2, 3, 4, 8, 6, 7, 1, 5, 10, 11},
            Vector#{0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0})
        case Face.D => (
            Vector#{0, 1, 2, 3, 5, 6, 7, 4},
            Vector#{0, 0, 0, 0, 0, 0, 0, 0},
            Vector#{0, 1, 2, 3, 5, 6, 7, 4, 8, 9, 10, 11},
            Vector#{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0})
        case Face.L => (
            Vector#{0, 2, 6, 3, 4, 1, 5, 7},
            Vector#{0, 1, 2, 0, 0, 2, 1, 0},
            Vector#{0, 1, 10, 3, 4, 5, 9, 7, 8, 2, 6, 11},
            Vector#{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0})
        case Face.B => (
            Vector#{0, 1, 3, 7, 4, 5, 2, 6},
            Vector#{0, 0, 1, 2, 0, 0, 2, 1},
            Vector#{0, 1, 2, 11, 4, 5, 6, 10, 8, 9, 3, 7},
            Vector#{0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1})
    }

    ///
    /// The state after one quarter turn.
    ///
    def quarter(s: State, table: (Vector[Int32], Vector[Int32], Vector[Int32], Vector[Int32])): State =
        let State.State(cp, co, ep, eo) = s;
        let (cycle, added, eCycle, flipped) = table;
        let ncp = cycle |> Vector.map(i -> Vector.get(i, cp));
        let nco = cycle |> Vector.mapWithIndex(i -> origin ->
            (Vector.get(origin, co) + Vector.get(i, added)) `Int32.modulo` 3);
        let nep = eCycle |> Vector.map(i -> Vector.get(i, ep));
        let neo = eCycle |> Vector.mapWithIndex(i -> origin ->
            (Vector.get(origin, eo) + Vector.get(i, flipped)) `Int32.modulo` 2);
        State.State(ncp, nco, nep, neo)

    ///
    /// The state after `m`, or `None` if a 3x3 cannot make that move.
    ///
    /// Only depth is refusable here: every face turns, but a cube three layers
    /// thick has one inner slice, and turning it is a rotation of the other
    /// two rather than a move.
    ///
    pub def applyMove(s: State, m: Move): Option[State] =
        if (Cube.depth(m) != 1)
            None
        else
            let table = quarterTurn(Cube.face(m));
            Some(Vector.range(0, Cube.quarterTurns(Cube.turn(m)))
                |> Vector.foldLeft(acc -> _ -> quarter(acc, table), s))

    ///
    /// The state after the whole sequence, applied left to right.
    ///
    pub def replay(s: State, ms: List[Move]): Option[State] =
        List.foldLeft((acc, m) -> Option.flatMap(st -> applyMove(st, m), acc), Some(s), ms)

    ///
    /// Every state one move away, paired with the move that reaches it.
    ///
    pub def steps(s: State): Vector[(Move, State)] =
        moves() |> Vector.filterMap(m -> applyMove(s, m) |> Option.map(t -> (m, t)))

}
