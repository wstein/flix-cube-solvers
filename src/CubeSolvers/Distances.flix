///
/// Breadth-first distances over a coordinate.
///
/// Every table in this package is the same shape: a space of numbered states,
/// a goal, and a way to step from one state to its neighbours. This walks it
/// outward from the goal and records how far each state is.
///
/// Written around flat arrays rather than lists, because the tables the 4x4
/// needs are measured in millions of states and a cons cell per state is not
/// a cost that scales.
///
mod CubeSolvers.Distances {

    ///
    /// How far each state is from `goal`, or `-1` where the goal cannot be
    /// reached.
    ///
    /// `next` gives the neighbours of a state. Moves are assumed reversible,
    /// so a state this cannot reach from the goal is a state that cannot
    /// reach the goal either -- which is what makes an unreachable entry
    /// meaningful rather than a bug.
    ///
    pub def of(size: Int32, goal: Int32, next: Int32 -> Vector[Int32] \ ef): Vector[Int32] \ ef =
        ofMany(size, Vector#{goal}, next)

    ///
    /// How far each state is from the nearest of several goals.
    ///
    /// A phase can have more than one finish. Tsai's second phase leaves the
    /// R and L centres in any of twelve arrangements the third can still
    /// repair, and a table measuring the distance to the closest of them is
    /// what lets the walk stop at whichever it reaches first.
    ///
    pub def ofMany(size: Int32, goals: Vector[Int32], next: Int32 -> Vector[Int32] \ ef): Vector[Int32] \ ef =
        region rc {
            let dist = Array.repeat(rc, size, -1);
            let front = Array.repeat(rc, size, 0);
            let back = Array.repeat(rc, size, 0);
            let count = goals |> Vector.foldLeft(seeded -> goal ->
                if (Array.get(goal, dist) < 0) {
                    Array.put(0, goal, dist);
                    Array.put(goal, seeded, front);
                    seeded + 1
                } else
                    seeded, 0);
            walk(dist, front, count, back, next, 0);
            Array.toVector(dist)
        }

    ///
    /// One ring at a time: everything unmarked next to the current frontier
    /// is marked and becomes the next frontier.
    ///
    def walk(
        dist: Array[Int32, r],
        front: Array[Int32, r],
        count: Int32,
        back: Array[Int32, r],
        next: Int32 -> Vector[Int32] \ ef,
        d: Int32
    ): Unit \ { ef, r } =
        if (count == 0)
            ()
        else
            // The frontiers swap: what this ring writes is what the next ring
            // reads, and the array just read becomes the scratch.
            walk(dist, back, scan(dist, front, count, back, next, d, 0, 0), front, next, d + 1)

    ///
    /// Expands the frontier one state at a time, returning how many new
    /// states were written behind it.
    ///
    def scan(
        dist: Array[Int32, r],
        front: Array[Int32, r],
        count: Int32,
        back: Array[Int32, r],
        next: Int32 -> Vector[Int32] \ ef,
        d: Int32,
        i: Int32,
        written: Int32
    ): Int32 \ { ef, r } =
        if (i >= count)
            written
        else
            scan(dist, front, count, back, next, d, i + 1,
                mark(dist, back, next(Array.get(i, front)), d, 0, written))

    ///
    /// Marks the unmarked among one state's neighbours.
    ///
    def mark(
        dist: Array[Int32, r],
        back: Array[Int32, r],
        tos: Vector[Int32],
        d: Int32,
        j: Int32,
        written: Int32
    ): Int32 \ r =
        if (j >= Vector.length(tos))
            written
        else
            let to = Vector.get(j, tos);
            let now =
                if (Array.get(to, dist) < 0) {
                    Array.put(d + 1, to, dist);
                    Array.put(to, written, back);
                    written + 1
                } else
                    written;
            mark(dist, back, tos, d, j + 1, now)

    ///
    /// The same walk, into a byte array the caller owns.
    ///
    /// The 4x4's edge table has thirty-one million states. As a `Vector` that
    /// is a couple of hundred megabytes; as bytes in a region it is a tenth of
    /// that, and it never has to be copied out. Depths beyond 127 do not
    /// occur, and would be a bug in the move set rather than a table this
    /// could not hold.
    ///
    pub def bytesInto(
        rc: Region[r],
        size: Int32,
        goals: Vector[Int32],
        next: Int32 -> Vector[Int32] \ ef
    ): Array[Int8, r] \ { ef, r } =
        let dist = Array.repeat(rc, size, -1i8);
        let front = Array.repeat(rc, size, 0);
        let back = Array.repeat(rc, size, 0);
        let count = goals |> Vector.foldLeft(seeded -> goal ->
            if (Array.get(goal, dist) < 0i8) {
                Array.put(0i8, goal, dist);
                Array.put(goal, seeded, front);
                seeded + 1
            } else
                seeded, 0);
        walkBytes(dist, front, count, back, next, 0i8);
        dist

    ///
    /// One ring at a time, over bytes.
    ///
    def walkBytes(
        dist: Array[Int8, r],
        front: Array[Int32, r],
        count: Int32,
        back: Array[Int32, r],
        next: Int32 -> Vector[Int32] \ ef,
        d: Int8
    ): Unit \ { ef, r } =
        if (count == 0)
            ()
        else
            walkBytes(dist, back, scanBytes(dist, front, count, back, next, d, 0, 0), front, next, d + 1i8)

    ///
    /// Expands the frontier, returning how many new states were written.
    ///
    def scanBytes(
        dist: Array[Int8, r],
        front: Array[Int32, r],
        count: Int32,
        back: Array[Int32, r],
        next: Int32 -> Vector[Int32] \ ef,
        d: Int8,
        i: Int32,
        written: Int32
    ): Int32 \ { ef, r } =
        if (i >= count)
            written
        else
            scanBytes(dist, front, count, back, next, d, i + 1,
                markBytes(dist, back, next(Array.get(i, front)), d, 0, written))

    ///
    /// Marks the unmarked among one state's neighbours.
    ///
    def markBytes(
        dist: Array[Int8, r],
        back: Array[Int32, r],
        tos: Vector[Int32],
        d: Int8,
        j: Int32,
        written: Int32
    ): Int32 \ r =
        if (j >= Vector.length(tos))
            written
        else
            let to = Vector.get(j, tos);
            let now =
                if (Array.get(to, dist) < 0i8) {
                    Array.put(d + 1i8, to, dist);
                    Array.put(to, written, back);
                    written + 1
                } else
                    written;
            markBytes(dist, back, tos, d, j + 1, now)

}
