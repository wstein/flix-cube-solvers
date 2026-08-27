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
        let bCount = Vector.length(bMoves);
        let size = Vector.length(aMoves) * bCount;
        CubeSolvers.Distances.of(size, 0, at ->
            let a = Vector.get(at / bCount, aMoves);
            let b = Vector.get(at `Int32.modulo` bCount, bMoves);
            Vector.range(0, Vector.length(a)) |> Vector.map(m ->
                Vector.get(m, a) * bCount + Vector.get(m, b)))

}
