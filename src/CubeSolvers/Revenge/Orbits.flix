///
/// Which slots hold which half of the wings.
///
/// A 4x4's twenty-four wings fall into two orbits of twelve, and the third
/// phase cannot move a wing out of its orbit. So the second phase has to put
/// them back: every slot of the first orbit holding a wing of the first orbit,
/// or every one holding a wing of the second, which is the same split seen
/// from the other side.
///
/// Sifting for this after the fact does not work. The property picks twelve
/// slots out of twenty-four, so a state has it about once in `C(24, 12)`, and
/// six thousand second-phase answers produced none at all. It has to be a
/// bound the search obeys, which is what this table is for.
///
mod CubeSolvers.Revenge.Orbits {

    use CubeSolvers.Kind
    use CubeSolvers.Solver.Facelets
    use CubeSolvers.Revenge.Pairs

    ///
    /// The number of ways twelve of the twenty-four slots can hold the first
    /// orbit's wings.
    ///
    pub def count(): Int32 = 2704156

    ///
    /// How many slots there are, and how many wings each orbit has.
    ///
    pub def slots(): Int32 = 24

    /// The size of one orbit.
    pub def half(): Int32 = 12

    ///
    /// Binomials through `slots()`, so ranking never recomputes them.
    ///
    pub def binomials(): Vector[Vector[Int32]] =
        Vector.range(0, slots() + 1) |> Vector.map(n ->
            Vector.range(0, half() + 1) |> Vector.map(k -> binomial(n, k)))

    def binomial(n: Int32, k: Int32): Int32 =
        if (k < 0 or k > n)
            0
        else
            List.range(0, k) |> List.foldLeft(acc -> i -> acc * (n - i) / (i + 1), 1)

    ///
    /// The rank of a twelve-slot subset, in colexicographic order.
    ///
    /// `chosen` lists the slots holding a first-orbit wing, ascending.
    ///
    pub def rank(binoms: Vector[Vector[Int32]], chosen: Vector[Int32]): Int32 =
        chosen |> Vector.mapWithIndex(i -> slot ->
            Vector.get(i + 1, Vector.get(slot, binoms))) |> Vector.sum

    ///
    /// The subset at a rank, as a slot-indexed vector of zeroes and ones.
    ///
    pub def unrank(binoms: Vector[Vector[Int32]], at: Int32): Vector[Int32] =
        region rc {
            let out = Array.repeat(rc, slots(), 0);
            descend(binoms, out, at, half(), slots() - 1);
            Array.toVector(out)
        }

    ///
    /// Places the largest remaining slot, then the rest.
    ///
    def descend(
        binoms: Vector[Vector[Int32]],
        out: Array[Int32, r],
        left: Int32,
        k: Int32,
        slot: Int32
    ): Unit \ r =
        if (k == 0 or slot < 0)
            ()
        else
            let below = Vector.get(k, Vector.get(slot, binoms));
            if (below <= left) {
                Array.put(1, slot, out);
                descend(binoms, out, left - below, k - 1, slot - 1)
            } else
                descend(binoms, out, left, k, slot - 1)

    ///
    /// The slots of the first orbit, which is where its wings belong.
    ///
    pub def home(): Vector[Int32] =
        Pairs.firstOrbit() |> Set.toList |> List.sort |> List.toVector

    ///
    /// Whether each slot holds a first-orbit wing, given who sits where.
    ///
    pub def marked(who: Vector[Int32]): Vector[Int32] =
        let first = Pairs.firstOrbit();
        who |> Vector.map(w -> if (Set.memberOf(w, first)) 1 else 0)

    ///
    /// The slots holding a first-orbit wing, ascending.
    ///
    pub def chosen(bits: Vector[Int32]): Vector[Int32] =
        Vector.range(0, slots()) |> Vector.filter(i -> Vector.get(i, bits) == 1)

    ///
    /// A cube's coordinate.
    ///
    pub def coordinate(l: Pairs.Layout, binoms: Vector[Vector[Int32]], cube: Facelets): Int32 =
        rank(binoms, chosen(marked(Pairs.identityIn(l, cube))))

    ///
    /// Both split states: each orbit's wings at home, or the two exchanged.
    ///
    pub def goals(binoms: Vector[Vector[Int32]]): Vector[Int32] =
        let there = home();
        let away = Vector.range(0, slots()) |> Vector.filter(i -> not Vector.memberOf(i, there));
        Vector#{ rank(binoms, there), rank(binoms, away) }

    ///
    /// Who sits in each slot after each of the second phase's moves, applied
    /// to a solved cube. A move sends the wing at `who(i)` to slot `i`, so a
    /// state's bits move the same way.
    ///
    pub def steps(): Vector[Vector[Int32]] =
        let l = Pairs.layout();
        let solved = CubeSolvers.Solver.solved(Kind.Cube4x4);
        CubeSolvers.Revenge.Separate.moves() |> Vector.map(m ->
            match CubeSolvers.Turning.apply(solved, m) {
                case Ok(after) => Pairs.identityIn(l, after)
                case Err(_)    => Vector.range(0, slots())
            })

    ///
    /// Where move `m` takes state `at`.
    ///
    pub def after(binoms: Vector[Vector[Int32]], steps: Vector[Vector[Int32]], at: Int32, m: Int32): Int32 =
        let bits = unrank(binoms, at);
        let who = Vector.get(m, steps);
        rank(binoms, chosen(Vector.range(0, slots()) |> Vector.map(i ->
            Vector.get(Vector.get(i, who), bits))))

    ///
    /// The exact distance from every state to a split one.
    ///
    pub def table(): Vector[Int32] =
        let binoms = binomials();
        let moved = steps();
        CubeSolvers.Distances.ofMany(count(), goals(binoms), at ->
            Vector.range(0, Vector.length(moved)) |> Vector.map(m -> after(binoms, moved, at, m)))

}
