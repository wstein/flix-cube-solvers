///
/// Pruning tables: how far from done, at least.
///
/// A search that only knew "not yet" would explore the whole tree. These say
/// how many moves a cube still needs *at minimum*, by taking two coordinates
/// at a time and measuring the exact distance in that smaller world. The true
/// distance is never less, so the number is safe to prune with, and the search
/// that uses it still returns what it would have returned without it.
///
mod CubeSolvers.Rubik.Pruning {

    ///
    /// Distances from the solved pair to every pair of two coordinates.
    ///
    /// Indexed `a * bCount + b`. Breadth first from the origin over the move
    /// tables, marking as it goes -- no queue of states, only their indices,
    /// because a coordinate pair is already its own index.
    ///
    pub def pairTable(aMoves: Vector[Vector[Int32]], bMoves: Vector[Vector[Int32]]): Vector[Int32] =
        region rc {
            let bCount = Vector.length(bMoves);
            let size = Vector.length(aMoves) * bCount;
            let dist = Array.repeat(rc, size, -1);
            Array.put(0, 0, dist);
            sweep(dist, 0 :: Nil, aMoves, bMoves, bCount, 0);
            Array.toVector(dist)
        }

    ///
    /// Marks everything one move past `frontier`, then recurses on what was
    /// new.
    ///
    def sweep(
        dist: Array[Int32, r],
        frontier: List[Int32],
        aMoves: Vector[Vector[Int32]],
        bMoves: Vector[Vector[Int32]],
        bCount: Int32,
        d: Int32
    ): Unit \ r =
        if (List.isEmpty(frontier))
            ()
        else
            let next = List.foldLeft(acc -> index -> step(dist, acc, index, aMoves, bMoves, bCount, d), Nil, frontier);
            sweep(dist, next, aMoves, bMoves, bCount, d + 1)

    ///
    /// Every unmarked neighbour of one pair, marked and collected.
    ///
    def step(
        dist: Array[Int32, r],
        acc: List[Int32],
        index: Int32,
        aMoves: Vector[Vector[Int32]],
        bMoves: Vector[Vector[Int32]],
        bCount: Int32,
        d: Int32
    ): List[Int32] \ r =
        let a = Vector.get(index / bCount, aMoves);
        let b = Vector.get(index `Int32.modulo` bCount, bMoves);
        List.foldLeft(seen -> m -> {
            let to = Vector.get(m, a) * bCount + Vector.get(m, b);
            if (Array.get(to, dist) < 0) {
                Array.put(d + 1, to, dist);
                to :: seen
            } else
                seen
        }, acc, List.range(0, Vector.length(a)))

}
