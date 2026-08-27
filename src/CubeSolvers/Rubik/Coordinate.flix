///
/// The 3x3 as coordinates, and how a move changes them.
///
/// A two-phase search never touches cubies: it works on a handful of small
/// integers, each describing one aspect of the cube, and it moves them through
/// tables computed once. This is that layer.
///
/// The three phase-one coordinates are corner twist, edge flip, and which four
/// positions the FR, FL, BL and BR edges occupy. Each is well defined on its
/// own because a move changes it without reference to the rest: the twist a
/// corner picks up depends on where it goes, not on which corner it is.
///
mod CubeSolvers.Rubik.Coordinate {

    use CubeSolvers.Move
    use CubeSolvers.Rubik.{State, applyMove, corners, twists, edges, flips, moves}

    ///
    /// How many corner orientations there are: three each for seven corners,
    /// with the eighth fixed by them.
    ///
    pub def twistCount(): Int32 = 2187

    ///
    /// How many edge orientations there are: two each for eleven edges, with
    /// the twelfth fixed by them.
    ///
    pub def flipCount(): Int32 = 2048

    ///
    /// How many ways four edges can occupy twelve positions, ignoring their
    /// order.
    ///
    pub def sliceCount(): Int32 = 495

    ///
    /// The corner twists of a state, as one number.
    ///
    pub def twist(s: State): Int32 =
        Vector.range(0, 7) |> Vector.foldLeft(acc -> i -> acc * 3 + Vector.get(i, twists(s)), 0)

    ///
    /// A state with the given corner twists and nothing else disturbed.
    ///
    pub def withTwist(n: Int32): State =
        let free = digits(n, 3, 7);
        let last = (3 - (Vector.sum(free) `Int32.modulo` 3)) `Int32.modulo` 3;
        State.State(Vector.range(0, 8), Vector.append(free, Vector#{last}), Vector.range(0, 12), Vector.repeat(12, 0))

    ///
    /// The edge flips of a state, as one number.
    ///
    pub def flip(s: State): Int32 =
        Vector.range(0, 11) |> Vector.foldLeft(acc -> i -> acc * 2 + Vector.get(i, flips(s)), 0)

    ///
    /// A state with the given edge flips and nothing else disturbed.
    ///
    pub def withFlip(n: Int32): State =
        let free = digits(n, 2, 11);
        let last = Vector.sum(free) `Int32.modulo` 2;
        State.State(Vector.range(0, 8), Vector.repeat(8, 0), Vector.range(0, 12), Vector.append(free, Vector#{last}))

    ///
    /// Which four positions hold the FR, FL, BL and BR edges, as one number.
    ///
    /// Ranked in the combinatorial number system: a four-element subset of
    /// twelve positions, numbered 0 to 494 by nothing more than which subset
    /// it is.
    ///
    /// Positions are counted from the far end, so that the solved cube -- its
    /// slice edges in the last four positions -- ranks 0. The first phase is
    /// then done when all three coordinates are zero, rather than when two of
    /// them are zero and the third is 494.
    ///
    pub def slice(s: State): Int32 =
        occupied(s) |> List.mapWithIndex(i -> p -> binomial(p, i + 1)) |> List.sum

    ///
    /// A state whose slice edges occupy the positions the number names.
    ///
    pub def withSlice(n: Int32): State =
        let positions = unrank(n) |> List.map(p -> 11 - p);
        let ep = region rc {
            let out = Array.repeat(rc, 12, 0);
            let others = List.range(0, 12) |> List.filter(p -> not List.memberOf(p, positions));
            foreach ((k, p) <- List.zip(List.range(0, 4), positions))
                Array.put(8 + k, p, out);
            foreach ((k, p) <- List.zip(List.range(0, 8), others))
                Array.put(k, p, out);
            Array.toVector(out)
        };
        State.State(Vector.range(0, 8), Vector.repeat(8, 0), ep, Vector.repeat(12, 0))

    ///
    /// The positions holding slice edges, in order.
    ///
    def occupied(s: State): List[Int32] =
        List.range(0, 12)
            |> List.filter(p -> Vector.get(p, edges(s)) >= 8)
            |> List.map(p -> 11 - p)
            |> List.reverse

    ///
    /// The four positions a slice number names.
    ///
    def unrank(n: Int32): List[Int32] =
        let (_, positions) = List.foldLeft(state -> k -> take(state, k), (n, Nil), List.reverse(List.range(1, 5)));
        positions

    ///
    /// Takes the largest position whose weight still fits, working down from
    /// the fourth slice edge to the first.
    ///
    def take(state: (Int32, List[Int32]), k: Int32): (Int32, List[Int32]) =
        let (left, found) = state;
        match List.maximum(List.filter(q -> binomial(q, k) <= left, List.range(0, 12))) {
            case Some(q) => (left - binomial(q, k), q :: found)
            case None    => (left, found)
        }

    ///
    /// `n` written in `base`, `width` digits, most significant first.
    ///
    def digits(n: Int32, base: Int32, width: Int32): Vector[Int32] =
        Vector.range(0, width) |> Vector.map(i ->
            (n / Int32.pow(base = base, width - 1 - i)) `Int32.modulo` base)

    ///
    /// How many ways to choose `k` of `n`.
    ///
    def binomial(n: Int32, k: Int32): Int32 =
        if (k < 0 or k > n)
            0
        else
            List.range(0, k) |> List.foldLeft(acc -> i -> acc * (n - i) / (i + 1), 1)

    ///
    /// Where each move takes each value of a coordinate.
    ///
    /// Built by decoding the coordinate into a cube, turning it, and reading
    /// the coordinate back -- which is slower than a closed form and is the
    /// reason the tables need no separate proof.
    ///
    pub def moveTable(count: Int32, decode: Int32 -> State, encode: State -> Int32): Vector[Vector[Int32]] =
        Vector.range(0, count) |> Vector.map(c ->
            let s = decode(c);
            moves() |> Vector.map(m -> match applyMove(s, m) {
                case Some(t) => encode(t)
                case None    => c
            }))

    ///
    /// The table for corner twist.
    ///
    pub def twistTable(): Vector[Vector[Int32]] = moveTable(twistCount(), withTwist, twist)

    ///
    /// The table for edge flip.
    ///
    pub def flipTable(): Vector[Vector[Int32]] = moveTable(flipCount(), withFlip, flip)

    ///
    /// The table for slice occupancy.
    ///
    pub def sliceTable(): Vector[Vector[Int32]] = moveTable(sliceCount(), withSlice, slice)

}
