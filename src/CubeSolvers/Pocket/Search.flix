///
/// Solving a pocket cube, by meeting a table in the middle.
///
/// The table is a Datalog fixpoint: seed the solved state at distance zero,
/// let one rule walk the move graph, and read off the shortest distance to
/// every state within reach. Flix computes the least fixpoint; nothing here
/// writes a queue, a visited set or a loop.
///
/// The search then walks out from the scrambled state until it lands on a
/// state the table knows, and finishes by following the table downhill. God's
/// number for the pocket cube is 11 quarter- and half-turns, so a table `t`
/// moves deep leaves at most `11 - t` moves to find.
///
mod CubeSolvers.Pocket.Search {

    use CubeSolvers.Move
    use CubeSolvers.Pocket.{State, steps, solved}

    ///
    /// Shortest distances from solved to every state within `depth` moves.
    ///
    /// `Down[Int32]` and not `Int32`: in a lattice position Flix joins
    /// `Int32` upwards, so a plain count would converge on the *longest* walk
    /// found rather than the shortest. `Down` reverses the order, which is
    /// the difference between a distance table and nonsense.
    ///
    pub def table(depth: Int32): Map[State, Int32] =
        let facts = #{
            Dist(solved(); Down.Down(0)).
            Dist(next; deeper(d)) :-
                Dist(s; d),
                if (shallowerThan(d, depth)),
                let next = CubeSolvers.Pocket.neighbours(s).
        };
        query facts select (s, d) from Dist(s; d)
            |> Vector.toList
            |> List.map(match (s, d) -> (s, distance(d)))
            |> List.toMap

    ///
    /// The same table again, breadth-first over a mutable map in a region.
    ///
    /// This is what the table would look like written by hand, and it is here
    /// to be measured against `table`, not to be preferred to it.
    ///
    pub def tableMut(depth: Int32): Map[State, Int32] =
        region rc {
            let dist = MutMap.empty(rc);
            MutMap.put(solved(), 0, dist);
            ring(dist, solved() :: Nil, depth, 0);
            MutMap.toMap(dist)
        }

    ///
    /// Marks everything one move past `frontier` and recurses on what was new.
    ///
    def ring(dist: MutMap[State, Int32, r], frontier: List[State], depth: Int32, d: Int32): Unit \ r =
        if (d >= depth or List.isEmpty(frontier))
            ()
        else
            let candidates = List.flatMap(s -> Vector.toList(CubeSolvers.Pocket.neighbours(s)), frontier);
            let fresh = List.distinct(List.filter(t -> not MutMap.memberOf(t, dist), candidates));
            foreach (t <- fresh)
                MutMap.put(t, d + 1, dist);
            ring(dist, fresh, depth, d + 1)

    ///
    /// The distance one move further out.
    ///
    def deeper(d: Down[Int32]): Down[Int32] = Down.Down(distance(d) + 1)

    ///
    /// Whether a state at distance `d` may still be expanded.
    ///
    def shallowerThan(d: Down[Int32], depth: Int32): Bool = distance(d) < depth

    ///
    /// The distance a lattice value stands for.
    ///
    def distance(d: Down[Int32]): Int32 =
        let Down.Down(n) = d;
        n

    ///
    /// The moves that take `s` home, for a state the table already knows.
    ///
    /// Every step is downhill: from a state at distance `d` there is always a
    /// move to one at `d - 1`, because that is what the fixpoint computed.
    ///
    pub def descend(tbl: Map[State, Int32], s: State): Option[List[Move]] =
        match Map.get(s, tbl) {
            case None    => None
            case Some(0) => Some(Nil)
            case Some(d) =>
                steps(s) |> Vector.findMap(match (m, t) ->
                    if (Map.get(t, tbl) == Some(d - 1))
                        descend(tbl, t) |> Option.map(rest -> m :: rest)
                    else
                        None)
        }

    ///
    /// A shortest solution for `s`, or `None` if none is `maxLength` moves or
    /// shorter.
    ///
    /// Lengths are tried in order, so the first solution found is optimal.
    ///
    pub def solution(tbl: Map[State, Int32], depth: Int32, s: State, maxLength: Int32): Option[List[Move]] =
        List.range(0, maxLength + 1) |> List.findMap(total -> exactly(tbl, depth, s, total, None))

    ///
    /// A solution of exactly `remaining` moves, if one exists.
    ///
    /// The table is complete out to `depth`, so a state that close to home
    /// and not in it cannot be solved in `remaining` moves at all -- which is
    /// what stops the walk from growing past the half it is responsible for.
    ///
    /// Turning the same face twice in a row is never part of a shortest
    /// solution -- the pair is one move or none -- so `last` drops those
    /// branches and takes the walk from nine ways to six.
    ///
    def exactly(
        tbl: Map[State, Int32],
        depth: Int32,
        s: State,
        remaining: Int32,
        last: Option[CubeSolvers.Face]
    ): Option[List[Move]] =
        if (Map.get(s, tbl) == Some(remaining))
            descend(tbl, s)
        else if (remaining <= depth)
            None
        else
            steps(s)
                |> Vector.filter(match (m, _) -> Some(CubeSolvers.face(m)) != last)
                |> Vector.findMap(match (m, t) ->
                    exactly(tbl, depth, t, remaining - 1, Some(CubeSolvers.face(m)))
                        |> Option.map(rest -> m :: rest))

}
