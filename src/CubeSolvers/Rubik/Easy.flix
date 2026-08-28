///
/// Exact shallow distances for the 3x3 cube.
///
/// A complete optimal table for the cube is far beyond this package's scope,
/// but its first five half-turn-metric layers contain only 621,649 states.
/// That is enough to produce genuinely three-, four-, and five-move practice
/// scrambles. States are keyed by two compact coordinates rather than four
/// vectors, which keeps the retained table proportional to its information.
///
mod CubeSolvers.Rubik.Easy {

    use CubeSolvers.{Move, MoveSequence}
    use CubeSolvers.Rubik.{State, corners, twists, edges, flips, solved, steps}

    /// A complete corner and edge description, packed into two integers.
    pub enum Key with Eq, Order, ToString {
        case Key(Int64, Int64)
    }

    /// The distances and exact-distance layers through a configured depth.
    pub enum Table {
        case Table(Map[Key, Int32], Vector[Vector[Key]])
    }

    /// The largest supported table depth.
    pub def maximumDepth(): Int32 = 5

    /// The known number of states through the fifth layer, including solved.
    def capacity(): Int32 = 621649

    /// An exact distance table through `depth`, or a reason there is none.
    ///
    /// The frontier arrays are sized for the fifth layer exactly, so a deeper
    /// request would run off the end of them. Refusing it is the difference
    /// between an error and a corrupted table.
    pub def prepared(depth: Int32): Result[String, Table] =
        if (depth < 0 or depth > maximumDepth())
            Err("A table reaches 0 to ${maximumDepth()} moves, not ${depth}")
        else
            Ok(build(depth))

    /// The table, once the depth is known to be within reach.
    def build(depth: Int32): Table =
        region rc {
            let known = MutMap.empty(rc);
            let front = Array.repeat(rc, capacity(), solved());
            let back = Array.repeat(rc, capacity(), solved());
            MutMap.put(key(solved()), 0, known);
            Array.put(solved(), 0, front);
            walk(known, front, 1, back, depth, 0);
            let distances = MutMap.toMap(known);
            Table.Table(distances, layers(distances, depth))
        }

    /// The standard table, complete through five moves.
    pub def standard(): Table = build(maximumDepth())

    /// The compact key for a cubie state.
    pub def key(state: State): Key =
        let corner = Int32.toInt64(rank(corners(state))) * 2187i64 + Int32.toInt64(CubeSolvers.Rubik.Coordinate.twist(state));
        let edge = Int32.toInt64(rank(edges(state))) * 2048i64 + Int32.toInt64(CubeSolvers.Rubik.Coordinate.flip(state));
        Key.Key(corner, edge)

    /// The cubie state represented by a compact key.
    pub def stateOf(k: Key): State =
        let Key.Key(corner, edge) = k;
        let cp = unrank(8, narrow(corner / 2187i64));
        let ep = unrank(12, narrow(edge / 2048i64));
        let co = CubeSolvers.Rubik.Coordinate.withTwist(narrow(corner `Int64.modulo` 2187i64)) |> twists;
        let eo = CubeSolvers.Rubik.Coordinate.withFlip(narrow(edge `Int64.modulo` 2048i64)) |> flips;
        State.State(cp, co, ep, eo)

    /// The exact distance of a state, if the table reaches it.
    pub def distance(table: Table, state: State): Option[Int32] =
        let Table.Table(distances, _) = table;
        Map.get(key(state), distances)

    /// Every state at the requested exact distance, empty beyond the table.
    ///
    /// Total on purpose: a caller asking for a layer the table does not hold
    /// gets nothing rather than an index out of bounds.
    pub def layer(table: Table, distance: Int32): Vector[Key] =
        let Table.Table(_, layers) = table;
        if (distance < 0 or distance >= Vector.length(layers))
            Vector.empty()
        else
            Vector.get(distance, layers)

    /// A shortest route home for a state the table reaches.
    pub def descend(table: Table, state: State): Option[MoveSequence] =
        let Table.Table(distances, _) = table;
        home(distances, state)

    /// Expands one breadth-first ring at a time.
    def walk(
        known: MutMap[Key, Int32, r],
        front: Array[State, r],
        count: Int32,
        back: Array[State, r],
        depth: Int32,
        at: Int32
    ): Unit \ r =
        if (at >= depth or count == 0)
            ()
        else
            walk(known, back, scan(known, front, count, back, at + 1, 0, 0), front, depth, at + 1)

    /// Writes the fresh neighbours of one ring into the next frontier.
    def scan(
        known: MutMap[Key, Int32, r],
        front: Array[State, r],
        count: Int32,
        back: Array[State, r],
        nextDistance: Int32,
        index: Int32,
        written: Int32
    ): Int32 \ r =
        if (index >= count)
            written
        else
            scan(known, front, count, back, nextDistance, index + 1,
                mark(known, back, steps(Array.get(index, front)), nextDistance, 0, written))

    /// Records every previously unseen neighbour.
    def mark(
        known: MutMap[Key, Int32, r],
        back: Array[State, r],
        next: Vector[(Move, State)],
        distance: Int32,
        index: Int32,
        written: Int32
    ): Int32 \ r =
        if (index >= Vector.length(next))
            written
        else
            let (_, state) = Vector.get(index, next);
            let stateKey = key(state);
            let now =
                if (MutMap.memberOf(stateKey, known))
                    written
                else {
                    MutMap.put(stateKey, distance, known);
                    Array.put(state, written, back);
                    written + 1
                };
            mark(known, back, next, distance, index + 1, now)

    /// Builds the index of states at every exact distance.
    def layers(distances: Map[Key, Int32], depth: Int32): Vector[Vector[Key]] =
        List.range(0, depth + 1) |> List.map(d ->
            Map.toList(distances)
                |> List.filter(match (_, value) -> value == d)
                |> List.map(match (stateKey, _) -> stateKey)
                |> List.toVector)
            |> List.toVector

    /// Follows the distance gradient to solved.
    def home(distances: Map[Key, Int32], state: State): Option[MoveSequence] =
        match Map.get(key(state), distances) {
            case None    => None
            case Some(0) => Some(Nil)
            case Some(d) =>
                steps(state) |> Vector.findMap(match (move, next) ->
                    if (Map.get(key(next), distances) == Some(d - 1))
                        home(distances, next) |> Option.map(rest -> move :: rest)
                    else
                        None)
        }

    /// The lexicographic rank of a permutation.
    def rank(permutation: Vector[Int32]): Int32 =
        let size = Vector.length(permutation);
        List.range(0, size) |> List.foldLeft(total -> i ->
            let smaller = List.range(i + 1, size) |> List.count(j -> Vector.get(j, permutation) < Vector.get(i, permutation));
            total + smaller * factorial(size - 1 - i), 0)

    /// The permutation at a lexicographic rank.
    def unrank(size: Int32, value: Int32): Vector[Int32] =
        let (_, _, picked) = List.foldLeft(state -> i -> pick(state, size, i), (value, List.range(0, size), Nil), List.range(0, size));
        picked |> List.reverse |> List.toVector

    def pick(state: (Int32, List[Int32], List[Int32]), size: Int32, index: Int32): (Int32, List[Int32], List[Int32]) =
        let (left, available, picked) = state;
        let factor = factorial(size - 1 - index);
        let selected = left / factor;
        match List.nth(selected, available) {
            case Some(value) => (left `Int32.modulo` factor, List.filter(other -> other != value, available), value :: picked)
            case None        => (left, available, picked)
        }

    def factorial(n: Int32): Int32 =
        List.range(0, n) |> List.foldLeft(product -> i -> product * (i + 1), 1)

    def narrow(n: Int64): Int32 = Int64.tryToInt32(n) |> Option.getWithDefault(0)

}
