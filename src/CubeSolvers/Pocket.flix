///
/// The 2x2x2, as cubies rather than stickers.
///
/// A pocket cube is eight corners and nothing else, so a state is a
/// permutation of those corners and a twist for each. The DBL corner is held
/// still -- solving is defined up to whole cube rotation, and pinning one
/// corner is how that freedom is spent. What is left is `U`, `R` and `F`, the
/// only three faces that move anything.
///
mod CubeSolvers.Pocket {

    use CubeSolvers.{Face, Move, Turn}

    ///
    /// A cube state: where each corner sits, and how far it is twisted.
    ///
    /// Both vectors are indexed by cubicle in the order URF, UFL, ULB, UBR,
    /// DFR, DLF, DBL, DRB. `perm[i]` is the corner now in cubicle `i`, and
    /// `twist[i]` is its rotation, 0, 1 or 2 clockwise.
    ///
    pub enum State with Eq, Order, ToString {
        case State(Vector[Int32], Vector[Int32])
    }

    ///
    /// The solved cube: every corner home, none twisted.
    ///
    pub def solved(): State = State.State(Vector.range(0, 8), Vector.repeat(8, 0))

    ///
    /// The corner sitting in each cubicle.
    ///
    pub def permutation(s: State): Vector[Int32] =
        let State.State(p, _) = s;
        p

    ///
    /// The twist of the corner in each cubicle.
    ///
    pub def twist(s: State): Vector[Int32] =
        let State.State(_, t) = s;
        t

    ///
    /// The three faces a pocket cube turns, given the DBL corner is fixed.
    ///
    /// `D`, `L` and `B` are missing on purpose: with a corner pinned they are
    /// the other three turns seen from behind, and offering both would double
    /// the branching factor of every search for nothing.
    ///
    pub def faces(): Vector[Face] = Vector#{Face.U, Face.R, Face.F}

    ///
    /// Every move a pocket cube accepts: three faces, three turns.
    ///
    pub def moves(): Vector[Move] =
        faces() |> Vector.flatMap(f ->
            Vector#{Turn.Cw, Turn.Half, Turn.Ccw} |> Vector.map(t -> CubeSolvers.move(f, 1, t)))

    ///
    /// How a single quarter turn of `f` moves and twists the corners, or
    /// `None` for the three faces a pocket cube does not turn.
    ///
    /// These are the standard cubie tables. Read `cycle[i]` as "the corner
    /// that ends up in cubicle `i` comes from cubicle `cycle[i]`", and
    /// `added[i]` as the clockwise twist it picks up on the way.
    ///
    def quarterTurn(f: Face): Option[(Vector[Int32], Vector[Int32])] = match f {
        case Face.U => Some((Vector#{3, 0, 1, 2, 4, 5, 6, 7}, Vector#{0, 0, 0, 0, 0, 0, 0, 0}))
        case Face.R => Some((Vector#{4, 1, 2, 0, 7, 5, 6, 3}, Vector#{2, 0, 0, 1, 1, 0, 0, 2}))
        case Face.F => Some((Vector#{1, 5, 2, 3, 0, 4, 6, 7}, Vector#{1, 2, 0, 0, 2, 1, 0, 0}))
        case _      => None
    }

    ///
    /// The state after one quarter turn described by `cycle` and `added`.
    ///
    def quarter(s: State, cycle: Vector[Int32], added: Vector[Int32]): State =
        let State.State(p, t) = s;
        let np = cycle |> Vector.map(i -> Vector.get(i, p));
        let nt = cycle |> Vector.mapWithIndex(i -> origin ->
            (Vector.get(origin, t) + Vector.get(i, added)) `Int32.modulo` 3);
        State.State(np, nt)

    ///
    /// The state after `m`, or `None` if a pocket cube cannot make that move.
    ///
    /// Depth is the reason a 2x2 move can be rejected as well as the face: a
    /// cube two layers deep has no inner slice, so anything past `depth = 1`
    /// is a rotation.
    ///
    pub def applyMove(s: State, m: Move): Option[State] =
        if (CubeSolvers.depth(m) != 1)
            None
        else
            quarterTurn(CubeSolvers.face(m)) |> Option.map(match (cycle, added) ->
                Vector.range(0, CubeSolvers.quarterTurns(CubeSolvers.turn(m))) |>
                    Vector.foldLeft(acc -> _ -> quarter(acc, cycle, added), s))

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

    ///
    /// Every state one move away.
    ///
    pub def neighbours(s: State): Vector[State] =
        steps(s) |> Vector.map(match (_, t) -> t)

}
