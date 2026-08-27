///
/// Walking a phase downhill.
///
/// Every phase table here holds exact distances, so from a state `d` moves
/// from its goal there is always a move to one `d - 1` away. Solving a phase
/// is therefore not a search: look up, step, repeat.
///
mod CubeSolvers.Revenge.Walk {

    use CubeSolvers.{Move, MoveSequence}
    use CubeSolvers.Solver.{Facelets, Invalid}

    ///
    /// The moves that take `cube` to this phase's goal.
    ///
    pub def down(
        cube: Facelets,
        dist: Vector[Int32],
        at: Facelets -> Int32,
        allowed: Vector[Move]
    ): Result[Invalid, MoveSequence] =
        let here = Vector.get(at(cube), dist);
        if (here < 0)
            Err(Invalid.Unsolvable("this phase cannot reach its goal from here"))
        else if (here == 0)
            Ok(Nil)
        else
            match closer(cube, dist, at, allowed, here) {
                case None            => Err(Invalid.Unsolvable("no move brings this phase closer; the table and the cube disagree"))
                case Some((m, next)) => down(next, dist, at, allowed) |> Result.map(rest -> m :: rest)
            }

    ///
    /// A move that brings the phase one step nearer.
    ///
    def closer(
        cube: Facelets,
        dist: Vector[Int32],
        at: Facelets -> Int32,
        allowed: Vector[Move],
        here: Int32
    ): Option[(Move, Facelets)] =
        allowed |> Vector.findMap(m ->
            match CubeSolvers.Turning.apply(cube, m) {
                case Err(_)   => None
                case Ok(next) => if (Vector.get(at(next), dist) == here - 1) Some((m, next)) else None
            })

}
