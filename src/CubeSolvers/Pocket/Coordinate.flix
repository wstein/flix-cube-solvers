///
/// States as one integer, for tables too big to hold as cubies.
///
/// Seven corners move -- DBL is pinned -- so a state is seven positions and
/// seven twists. Three bits each and two bits each is 35 bits, which is an
/// Int64 and not an Int32. The packing is sparse: 3,674,160 states scattered
/// over 2^35. That costs nothing here, because what is stored is the states
/// that exist, not the space they are drawn from.
///
mod CubeSolvers.Pocket.Coordinate {

    use CubeSolvers.Pocket.State

    ///
    /// The cubicles that move.
    ///
    def mobile(): Vector[Int32] = Vector#{0, 1, 2, 3, 4, 5, 7}

    ///
    /// The state as one integer.
    ///
    pub def encode(s: State): Int64 =
        let perm = CubeSolvers.Pocket.permutation(s);
        let twist = CubeSolvers.Pocket.twist(s);
        mobile() |> Vector.foldLeft(acc -> i ->
            let p = Int32.toInt64(Vector.get(i, perm));
            let t = Int32.toInt64(Vector.get(i, twist));
            acc * 32i64 + p * 4i64 + t, 0i64)

    ///
    /// The state an integer stands for.
    ///
    pub def decode(code: Int64): State =
        region rc {
            let perm = Array.repeat(rc, 8, 6);
            let twist = Array.repeat(rc, 8, 0);
            foreach (k <- List.range(0, 7)) {
                let i = Vector.get(6 - k, mobile());
                let chunk = (code / Int32.toInt64(Int32.pow(base = 32, k))) `Int64.modulo` 32i64;
                Array.put(narrow(chunk / 4i64), i, perm);
                Array.put(narrow(chunk `Int64.modulo` 4i64), i, twist)
            };
            State.State(Array.toVector(perm), Array.toVector(twist))
        }

    ///
    /// A digit, as an Int32.
    ///
    def narrow(x: Int64): Int32 = Int64.tryToInt32(x) |> Option.getWithDefault(0)

    ///
    /// Every state one move away, as integers.
    ///
    pub def neighbours(code: Int64): Vector[Int64] =
        CubeSolvers.Pocket.neighbours(decode(code)) |> Vector.map(encode)

    ///
    /// Shortest distances from solved to every state within `depth` moves,
    /// over coordinates rather than cubies.
    ///
    /// Measured against `Search.table`, which does the same over cubies: at
    /// depth 6 this is about 1.4 times faster, and that is the whole of the
    /// win. Asked for the complete table -- all 3,674,160 states -- it
    /// finishes in 307 seconds and 3.1GB, so the packing does not change what
    /// this approach can reach. Use `Search.table`; this is here because the
    /// encoding is worth having and the measurement is worth being able to
    /// repeat.
    ///
    pub def table(depth: Int32): Map[Int64, Int32] =
        let facts = #{
            Dist(encode(CubeSolvers.Pocket.solved()); Down.Down(0)).
            Dist(next; deeper(d)) :- Dist(s; d), if (distance(d) < depth), let next = neighbours(s).
        };
        query facts select (s, d) from Dist(s; d)
            |> Vector.toList
            |> List.map(match (s, d) -> (s, distance(d)))
            |> List.toMap

    def deeper(d: Down[Int32]): Down[Int32] = Down.Down(distance(d) + 1)

    def distance(d: Down[Int32]): Int32 =
        let Down.Down(n) = d;
        n

}
