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
    /// Every shortest answer this phase has, up to `cap`, as the cubes they
    /// leave behind.
    ///
    /// One answer is not enough. What the next phase needs of the wings is a
    /// property roughly one state in five hundred has, and every answer that
    /// shares a prefix drags the wings along the same way -- so a single
    /// answer, and even every shortest answer read in move order, gives far
    /// fewer independent tries than their number suggests.
    ///
    pub def manyWith(
        cube: Facelets,
        dist: Vector[Int32],
        at: Facelets -> Int32,
        allowed: Vector[Move],
        cap: Int32
    ): List[(MoveSequence, Facelets)] =
        spread(cube, dist, at, allowed, cap, 0)

    ///
    /// The same, turning the move order one place deeper at each level.
    ///
    /// Read in a fixed move order the first answers all begin the same way,
    /// and so leave the wings in much the same state. That matters because
    /// the next phase sifts these for a property about one state in five
    /// hundred has: eight thousand answers off the first ten of these
    /// yielded none at all, where a spread-out sixty yielded sixty-four.
    ///
    def spread(
        cube: Facelets,
        dist: Vector[Int32],
        at: Facelets -> Int32,
        allowed: Vector[Move],
        cap: Int32,
        spin: Int32
    ): List[(MoveSequence, Facelets)] =
        let here = Vector.get(at(cube), dist);
        let width = Vector.length(allowed);
        if (here < 0 or cap <= 0)
            Nil
        else if (here == 0)
            (Nil, cube) :: Nil
        else
            List.range(0, width)
                |> List.map(i -> Vector.get((i + spin) `Int32.modulo` width, allowed))
                |> List.filterMap(m -> match CubeSolvers.Turning.apply(cube, m) {
                    case Ok(next) =>
                        if (Vector.get(at(next), dist) == here - 1) Some((m, next)) else None
                    case Err(_) => None
                })
                |> List.foldLeft(acc -> match (m, next) ->
                    if (List.length(acc) >= cap)
                        acc
                    else
                        acc ::: (spread(next, dist, at, allowed, cap - List.length(acc), spin + 7)
                            |> List.map(match (rest, leaf) -> (m :: rest, leaf))), Nil)

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
