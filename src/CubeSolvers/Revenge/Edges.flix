///
/// The 4x4's edge pairing, as the third phase searches it.
///
/// The state is the pairing permutation of twelve places, carried alongside a
/// second permutation that records which way round each place's two wings are
/// -- the moves need both, and the ranking needs them composed.
///
/// Ported from `Edge3` in TPR-4x4x4-Solver, whose coordinates and symmetry
/// reduction this follows: 12!/2 states folded to 1,538 symmetry classes
/// against 20,160, which is what makes the table fit at all.
///
mod CubeSolvers.Revenge.Edges {

    ///
    /// How many arrangements of the last eight places.
    ///
    pub def rawCount(): Int32 = 20160

    ///
    /// How many ways four places can be placed among twelve.
    ///
    pub def firstCount(): Int32 = 11880

    ///
    /// Half factorials, which is what an even permutation is indexed by.
    ///
    pub def halfFactorials(): Vector[Int32] =
        Vector#{1, 1, 1, 3, 12, 60, 360, 2520, 20160, 181440, 1814400, 19958400, 239500800}

    ///
    /// The pairing permutation, and the order its places are read in.
    ///
    pub type alias State = { edge = Vector[Int32], turned = Vector[Int32] }

    ///
    /// Every place paired and in order.
    ///
    pub def solved(): State = { edge = Vector.range(0, 12), turned = Vector.range(0, 12) }

    ///
    /// The state with the second permutation folded into the first.
    ///
    /// The moves shuffle both; the ranking only makes sense once they have
    /// been composed back together.
    ///
    pub def standard(s: State): State =
        region rc {
            let back = Array.repeat(rc, 12, 0);
            foreach (i <- List.range(0, 12))
                Array.put(i, Vector.get(i, s#turned), back);
            { edge = s#edge |> Vector.map(e -> Array.get(e, back)), turned = Vector.range(0, 12) }
        }

    ///
    /// The place of the first `end` entries among all their orderings.
    ///
    /// The digits are held in one Int64, four bits each, and removing one
    /// shifts the rest down -- which is how a permutation is ranked without
    /// searching for what is left.
    ///
    pub def rank(s: State, end: Int32): Int32 =
        let std = standard(s);
        climb(std#edge, end, 0, 0, 0xba9876543210i64)

    ///
    /// Adds one digit of the ranking.
    ///
    def climb(edge: Vector[Int32], end: Int32, i: Int32, idx: Int32, val: Int64): Int32 =
        if (i >= end)
            idx
        else
            let v = Int32.toInt64(Vector.get(i, edge) * 4);
            let digit = Int64.tryToInt32(Int64.bitwiseAnd(Int64.rightShift(val, Int64.tryToInt32(v) |> Option.getWithDefault(0)), 15i64))
                |> Option.getWithDefault(0);
            let shifted = Int64.leftShift(0x111111111110i64, Int64.tryToInt32(v) |> Option.getWithDefault(0));
            climb(edge, end, i + 1, idx * (12 - i) + digit, val - shifted)

    ///
    /// The state at a given place among the even permutations.
    ///
    pub def unrank(idx: Int32): State =
        region rc {
            let edge = Array.repeat(rc, 12, 0);
            let facts = halfFactorials();
            let (left, val, parity) = fill(edge, facts, 0, idx, 0xba9876543210i64, 0);
            if (parity `Int32.modulo` 2 == 0)
                Array.put(Int64.tryToInt32(val) |> Option.getWithDefault(0), 11, edge)
            else {
                Array.put(Array.get(10, edge), 11, edge);
                Array.put(Int64.tryToInt32(val) |> Option.getWithDefault(0), 10, edge)
            };
            let _ = left;
            { edge = Array.toVector(edge), turned = Vector.range(0, 12) }
        }

    ///
    /// Places the first eleven entries, digit by digit.
    ///
    def fill(
        edge: Array[Int32, r],
        facts: Vector[Int32],
        i: Int32,
        idx: Int32,
        val: Int64,
        parity: Int32
    ): (Int32, Int64, Int32) \ r =
        if (i >= 11)
            (idx, val, parity)
        else
            let p = Vector.get(11 - i, facts);
            let v = idx / p;
            let shift = v * 4;
            let digit = Int64.tryToInt32(Int64.bitwiseAnd(Int64.rightShift(val, shift), 15i64)) |> Option.getWithDefault(0);
            Array.put(digit, i, edge);
            let mask = Int64.leftShift(1i64, shift) - 1i64;
            let rest = Int64.bitwiseAnd(Int64.rightShift(val, 4), Int64.bitwiseNot(mask));
            fill(edge, facts, i + 1, idx `Int32.modulo` p, Int64.bitwiseAnd(val, mask) + rest, parity + v)

    ///
    /// The state after one of the third phase's twenty moves.
    ///
    /// The numbering is the reference's: U, U2, U', R2, F, F2, F', D, D2, D',
    /// L2, B, B2, B', then the six slice half turns u2, r2, f2, d2, l2, b2.
    ///
    pub def move(s: State, m: Int32): State =
        region rc {
            let e = toArray(rc, s#edge);
            let t = toArray(rc, s#turned);
            match m {
                case 0  => { circle(e, 0, 4, 1, 5); circle(t, 0, 4, 1, 5) }
                case 1  => { swap4(e, 0, 4, 1, 5); swap4(t, 0, 4, 1, 5) }
                case 2  => { circle(e, 0, 5, 1, 4); circle(t, 0, 5, 1, 4) }
                case 3  => { swap4(e, 5, 10, 6, 11); swap4(t, 5, 10, 6, 11) }
                case 4  => { circle(e, 0, 11, 3, 8); circle(t, 0, 11, 3, 8) }
                case 5  => { swap4(e, 0, 11, 3, 8); swap4(t, 0, 11, 3, 8) }
                case 6  => { circle(e, 0, 8, 3, 11); circle(t, 0, 8, 3, 11) }
                case 7  => { circle(e, 2, 7, 3, 6); circle(t, 2, 7, 3, 6) }
                case 8  => { swap4(e, 2, 7, 3, 6); swap4(t, 2, 7, 3, 6) }
                case 9  => { circle(e, 2, 6, 3, 7); circle(t, 2, 6, 3, 7) }
                case 10 => { swap4(e, 4, 8, 7, 9); swap4(t, 4, 8, 7, 9) }
                case 11 => { circle(e, 1, 9, 2, 10); circle(t, 1, 9, 2, 10) }
                case 12 => { swap4(e, 1, 9, 2, 10); swap4(t, 1, 9, 2, 10) }
                case 13 => { circle(e, 1, 10, 2, 9); circle(t, 1, 10, 2, 9) }
                case 14 => { swap4(e, 0, 4, 1, 5); swap4(t, 0, 4, 1, 5); swap2(e, 9, 11); swap2(t, 8, 10) }
                case 15 => { swap4(e, 5, 10, 6, 11); swap4(t, 5, 10, 6, 11); swap2(e, 1, 3); swap2(t, 0, 2) }
                case 16 => { swap4(e, 0, 11, 3, 8); swap4(t, 0, 11, 3, 8); swap2(e, 5, 7); swap2(t, 4, 6) }
                case 17 => { swap4(e, 2, 7, 3, 6); swap4(t, 2, 7, 3, 6); swap2(e, 8, 10); swap2(t, 9, 11) }
                case 18 => { swap4(e, 4, 8, 7, 9); swap4(t, 4, 8, 7, 9); swap2(e, 0, 2); swap2(t, 1, 3) }
                case _  => { swap4(e, 1, 9, 2, 10); swap4(t, 1, 9, 2, 10); swap2(e, 4, 6); swap2(t, 5, 7) }
            };
            { edge = Array.toVector(e), turned = Array.toVector(t) }
        }

    ///
    /// The state after one of the three rotations the symmetry uses.
    ///
    pub def rot(s: State, r: Int32): State =
        if (r == 0)
            move(move(s, 14), 17)
        else
            region rc {
                let e = toArray(rc, s#edge);
                let t = toArray(rc, s#turned);
                if (r == 1) {
                    circlex(e, t, 11, 5, 10, 6);
                    circlex(e, t, 5, 10, 6, 11);
                    circlex(e, t, 1, 2, 3, 0);
                    circlex(e, t, 4, 9, 7, 8);
                    circlex(e, t, 8, 4, 9, 7);
                    circlex(e, t, 0, 1, 2, 3)
                } else {
                    swapx(e, t, 4, 5); swapx(e, t, 5, 4);
                    swapx(e, t, 11, 8); swapx(e, t, 8, 11);
                    swapx(e, t, 7, 6); swapx(e, t, 6, 7);
                    swapx(e, t, 9, 10); swapx(e, t, 10, 9);
                    swapx(e, t, 1, 1); swapx(e, t, 0, 0);
                    swapx(e, t, 3, 3); swapx(e, t, 2, 2)
                };
                { edge = Array.toVector(e), turned = Array.toVector(t) }
            }

    ///
    /// The state after the `r`th of the eight symmetries.
    ///
    pub def rotate(s: State, r: Int32): State =
        if (r >= 2)
            rotate(rot(rot(s, 1), 2), r - 2)
        else if (r != 0)
            rot(s, 0)
        else
            s

    ///
    /// A working copy of a vector.
    ///
    def toArray(rc: Region[r], v: Vector[Int32]): Array[Int32, r] \ r =
        let out = Array.repeat(rc, Vector.length(v), 0);
        foreach (i <- List.range(0, Vector.length(v)))
            Array.put(Vector.get(i, v), i, out);
        out

    ///
    /// Four places moved round: d takes c, c takes b, b takes a, a takes d.
    ///
    def circle(a: Array[Int32, r], p: Int32, q: Int32, u: Int32, v: Int32): Unit \ r =
        let held = Array.get(v, a);
        Array.put(Array.get(u, a), v, a);
        Array.put(Array.get(q, a), u, a);
        Array.put(Array.get(p, a), q, a);
        Array.put(held, p, a)

    ///
    /// Two pairs exchanged.
    ///
    def swap4(a: Array[Int32, r], p: Int32, q: Int32, u: Int32, v: Int32): Unit \ r =
        swap2(a, p, u);
        swap2(a, q, v)

    ///
    /// Two places exchanged.
    ///
    def swap2(a: Array[Int32, r], x: Int32, y: Int32): Unit \ r =
        let held = Array.get(x, a);
        Array.put(Array.get(y, a), x, a);
        Array.put(held, y, a)

    ///
    /// One place of each array exchanged, which is what a rotation does.
    ///
    def swapx(e: Array[Int32, r], t: Array[Int32, r], x: Int32, y: Int32): Unit \ r =
        let held = Array.get(x, e);
        Array.put(Array.get(y, t), x, e);
        Array.put(held, y, t)

    ///
    /// Four places moved round, alternating between the two arrays.
    ///
    def circlex(e: Array[Int32, r], t: Array[Int32, r], p: Int32, q: Int32, u: Int32, v: Int32): Unit \ r =
        let held = Array.get(v, t);
        Array.put(Array.get(u, e), v, t);
        Array.put(Array.get(q, t), u, e);
        Array.put(Array.get(p, e), q, t);
        Array.put(held, p, e)

    ///
    /// How many symmetry classes the first coordinate has.
    ///
    pub def classCount(): Int32 = 1538

    ///
    /// Which symmetry undoes which.
    ///
    def symmetryInverse(): Vector[Int32] = Vector#{0, 1, 6, 3, 4, 5, 2, 7}

    ///
    /// The first coordinate's 11,880 arrangements, folded by the eight
    /// symmetries.
    ///
    /// Two arrangements that a rotation carries into one another are the same
    /// problem, so only one of each family is stored. That is what turns
    /// 239.5 million states into 31 million, and it is the only reason the
    /// table fits.
    ///
    pub def classify(): { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 } =
        region rc {
            let seen = Array.repeat(rc, firstCount(), false);
            let raw2sym = Array.repeat(rc, firstCount(), 0);
            let sym2raw = Array.repeat(rc, firstCount(), 0);
            let selfSymmetry = Array.repeat(rc, firstCount(), 0);
            let found = fold(rc, seen, raw2sym, sym2raw, selfSymmetry, 0, 0);
            { sym2raw = Array.toVector(sym2raw),
              raw2sym = Array.toVector(raw2sym),
              selfSymmetry = Array.toVector(selfSymmetry),
              classes = found }
        }

    ///
    /// Walks the arrangements, opening a class at each one not yet claimed.
    ///
    def fold(
        rc: Region[r],
        seen: Array[Bool, r],
        raw2sym: Array[Int32, r],
        sym2raw: Array[Int32, r],
        selfSymmetry: Array[Int32, r],
        i: Int32,
        count: Int32
    ): Int32 \ r =
        if (i >= firstCount())
            count
        else if (Array.get(i, seen))
            fold(rc, seen, raw2sym, sym2raw, selfSymmetry, i + 1, count)
        else {
            Array.put(i, count, sym2raw);
            claim(seen, raw2sym, selfSymmetry, unrank(i * rawCount()), 0, count, i);
            fold(rc, seen, raw2sym, sym2raw, selfSymmetry, i + 1, count + 1)
        }

    ///
    /// Marks the eight arrangements a rotation carries this one into.
    ///
    def claim(
        seen: Array[Bool, r],
        raw2sym: Array[Int32, r],
        selfSymmetry: Array[Int32, r],
        s: State,
        j: Int32,
        count: Int32,
        representative: Int32
    ): Unit \ r =
        if (j >= 8)
            ()
        else
            let idx = rank(s, 4);
            // A rotation that sends the class back to itself: the table build
            // uses these to fill several entries from one expansion.
            if (idx == representative)
                Array.put(Int32.bitwiseOr(Array.get(count, selfSymmetry), Int32.leftShift(1, j)), count, selfSymmetry)
            else
                ();
            Array.put(true, idx, seen);
            Array.put(count * 8 + Vector.get(j, symmetryInverse()), idx, raw2sym);
            let turnedOnce = rot(s, 0);
            let next = if (j `Int32.modulo` 2 == 1) rot(rot(turnedOnce, 1), 2) else turnedOnce;
            claim(seen, raw2sym, selfSymmetry, next, j + 1, count, representative)

    ///
    /// How many of the phase's moves the edge coordinate needs.
    ///
    /// Seventeen of the twenty: the coordinate cannot tell the last three
    /// apart from moves it already has.
    ///
    pub def edgeMoves(): Int32 = 17

    ///
    /// The state's second permutation, inverted -- what `standard` composes
    /// with.
    ///
    pub def backward(s: State): Vector[Int32] =
        region rc {
            let back = Array.repeat(rc, 12, 0);
            foreach (i <- List.range(0, 12))
                Array.put(i, Vector.get(i, s#turned), back);
            Array.toVector(back)
        }

    ///
    /// Each move followed by each rotation, as a pair of permutations.
    ///
    /// A hundred and sixty of each, indexed `move * 8 + rotation`. Having
    /// them lets a state be moved, rotated and ranked in one pass, which is
    /// what the table build does thirty-one million times over.
    ///
    pub def moveRotations(): { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] } =
        let pairs = List.range(0, 20) |> List.flatMap(m ->
            List.range(0, 8) |> List.map(r ->
                let turned = rotate(move(solved(), m), r);
                (turned#edge, backward(turned))));
        { forward = pairs |> List.map(fst) |> List.toVector,
          back = pairs |> List.map(snd) |> List.toVector }

    ///
    /// The rank of the first `end` places after a move and rotation, without
    /// building the state it would give.
    ///
    pub def rankThrough(
        tables: { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        which: Int32,
        edge: Vector[Int32],
        end: Int32
    ): Int32 =
        let forward = Vector.get(which, tables#forward);
        let back = Vector.get(which, tables#back);
        through(forward, back, edge, end, 0, 0, 0xba9876543210i64)

    ///
    /// Adds one digit, reading the place the move and rotation send it to.
    ///
    def through(
        forward: Vector[Int32],
        back: Vector[Int32],
        edge: Vector[Int32],
        end: Int32,
        i: Int32,
        idx: Int32,
        val: Int64
    ): Int32 =
        if (i >= end)
            idx
        else
            let place = Vector.get(Vector.get(Vector.get(i, forward), edge), back);
            let v = place * 4;
            let digit = Int64.tryToInt32(Int64.bitwiseAnd(Int64.rightShift(val, v), 15i64)) |> Option.getWithDefault(0);
            through(forward, back, edge, end, i + 1, idx * (12 - i) + digit, val - Int64.leftShift(0x111111111110i64, v))

    ///
    /// How many states the pruning table holds.
    ///
    pub def pruningSize(): Int32 = classCount() * rawCount()

    ///
    /// The state at a place, written into an array the caller owns.
    ///
    /// The table build unranks thirty-one million states. Allocating one
    /// array per state is what makes that take hours rather than minutes.
    ///
    pub def unrankInto(out: Array[Int32, r], idx: Int32): Unit \ r =
        let facts = halfFactorials();
        let (_, val, parity) = fillInto(out, facts, 0, idx, 0xba9876543210i64, 0);
        let last = Int64.tryToInt32(val) |> Option.getWithDefault(0);
        if (parity `Int32.modulo` 2 == 0)
            Array.put(last, 11, out)
        else {
            Array.put(Array.get(10, out), 11, out);
            Array.put(last, 10, out)
        }

    ///
    /// Places the first eleven entries into the caller's array.
    ///
    def fillInto(
        out: Array[Int32, r],
        facts: Vector[Int32],
        i: Int32,
        idx: Int32,
        val: Int64,
        parity: Int32
    ): (Int32, Int64, Int32) \ r =
        if (i >= 11)
            (idx, val, parity)
        else
            let p = Vector.get(11 - i, facts);
            let v = idx / p;
            let shift = v * 4;
            let digit = Int64.tryToInt32(Int64.bitwiseAnd(Int64.rightShift(val, shift), 15i64)) |> Option.getWithDefault(0);
            Array.put(digit, i, out);
            let mask = Int64.leftShift(1i64, shift) - 1i64;
            let rest = Int64.bitwiseAnd(Int64.rightShift(val, 4), Int64.bitwiseNot(mask));
            fillInto(out, facts, i + 1, idx `Int32.modulo` p, Int64.bitwiseAnd(val, mask) + rest, parity + v)

    ///
    /// The rank of the first `end` places after a move and rotation, reading
    /// the state from an array.
    ///
    pub def rankThroughArray(
        forward: Vector[Int32],
        back: Vector[Int32],
        edge: Array[Int32, r],
        end: Int32
    ): Int32 \ r =
        throughArray(forward, back, edge, end, 0, 0, 0xba9876543210i64)

    ///
    /// Adds one digit, reading the place the move and rotation send it to.
    ///
    def throughArray(
        forward: Vector[Int32],
        back: Vector[Int32],
        edge: Array[Int32, r],
        end: Int32,
        i: Int32,
        idx: Int32,
        val: Int64
    ): Int32 \ r =
        if (i >= end)
            idx
        else
            let place = Vector.get(Array.get(Vector.get(i, forward), edge), back);
            let v = place * 4;
            let digit = Int64.tryToInt32(Int64.bitwiseAnd(Int64.rightShift(val, v), 15i64)) |> Option.getWithDefault(0);
            throughArray(forward, back, edge, end, i + 1, idx * (12 - i) + digit, val - Int64.leftShift(0x111111111110i64, v))

    ///
    /// The exact distance to a paired cube, for every state, two bits each.
    ///
    /// Thirty-one million states in under eight megabytes. Two bits hold the
    /// depth modulo three, which is enough: a neighbour is nearer, level with
    /// or further from the goal, and only one of those is one less than the
    /// present depth. The true distance is recovered by walking down.
    ///
    /// Built by sweeping rather than by queue: at each depth the whole table
    /// is scanned for entries at that depth and their neighbours are filled
    /// in. A frontier of thirty-one million indices would cost more than the
    /// table.
    ///
    pub def pruning(rc: Region[r], limit: Int32): { table = Array[Int32, r], filled = Int32, depth = Int32 } \ r =
        let classes = classify();
        let tables = moveRotations();
        let words = pruningSize() / 16;
        let table = Array.repeat(rc, words, -1);
        let scratch = Array.repeat(rc, 12, 0);
        put(table, 0, 0);
        let (filled, depth) = sweep(table, scratch, classes, tables, 1, 0, limit);
        { table = table, filled = filled, depth = depth }

    ///
    /// One depth at a time, until the table is full or the limit is reached.
    ///
    def sweep(
        table: Array[Int32, r],
        scratch: Array[Int32, r],
        classes: { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 },
        tables: { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        filled: Int32,
        depth: Int32,
        limit: Int32
    ): (Int32, Int32) \ r =
        if (filled >= pruningSize() or depth >= limit)
            (filled, depth)
        else
            let found = scanWords(table, scratch, classes, tables, depth `Int32.modulo` 3,
                (depth + 1) `Int32.modulo` 3, 0, filled);
            if (found == filled)
                (filled, depth)
            else
                sweep(table, scratch, classes, tables, found, depth + 1, limit)

    ///
    /// Scans the packed words for entries at the current depth.
    ///
    def scanWords(
        table: Array[Int32, r],
        scratch: Array[Int32, r],
        classes: { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 },
        tables: { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        want: Int32,
        next: Int32,
        word: Int32,
        filled: Int32
    ): Int32 \ r =
        if (word * 16 >= pruningSize())
            filled
        else
            let packed = Array.get(word, table);
            let after =
                if (packed == -1)
                    filled
                else
                    scanEntries(table, scratch, classes, tables, want, next, word, 0, packed, filled);
            scanWords(table, scratch, classes, tables, want, next, word + 1, after)

    ///
    /// Scans the sixteen entries of one word.
    ///
    def scanEntries(
        table: Array[Int32, r],
        scratch: Array[Int32, r],
        classes: { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 },
        tables: { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        want: Int32,
        next: Int32,
        word: Int32,
        j: Int32,
        packed: Int32,
        filled: Int32
    ): Int32 \ r =
        if (j >= 16)
            filled
        else
            let after =
                if (Int32.bitwiseAnd(Int32.rightShift(packed, j * 2), 3) == want)
                    expand(table, scratch, classes, tables, word * 16 + j, next, filled)
                else
                    filled;
            scanEntries(table, scratch, classes, tables, want, next, word, j + 1, packed, after)

    ///
    /// Fills in every neighbour of one state that has not been reached yet.
    ///
    def expand(
        table: Array[Int32, r],
        scratch: Array[Int32, r],
        classes: { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 },
        tables: { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        at: Int32,
        next: Int32,
        filled: Int32
    ): Int32 \ r =
        let first = Vector.get(at / rawCount(), classes#sym2raw);
        unrankInto(scratch, first * rawCount() + (at `Int32.modulo` rawCount()));
        step(table, scratch, classes, tables, 0, next, filled)

    ///
    /// Where move `m` sends the state at `index`.
    ///
    /// The whole transition: unrank the state, rank it again through the
    /// move, canonicalise the first coordinate by symmetry, and carry the
    /// second through the same symmetry. `scratch` is the caller's, so this
    /// allocates nothing.
    ///
    pub def after(
        classes: { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 },
        tables: { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        scratch: Array[Int32, r],
        index: Int32,
        m: Int32
    ): Int32 \ r =
        let first = Vector.get(index / rawCount(), classes#sym2raw);
        unrankInto(scratch, first * rawCount() + (index `Int32.modulo` rawCount()));
        moved(classes, tables, scratch, m)

    ///
    /// The index after a move, for a state already unranked into `scratch`.
    ///
    def moved(
        classes: { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 },
        tables: { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        scratch: Array[Int32, r],
        m: Int32
    ): Int32 \ r =
        let plain = m * 8;
        let movedFirst = rankThroughArray(
            Vector.get(plain, tables#forward), Vector.get(plain, tables#back), scratch, 4);
        let packed = Vector.get(movedFirst, classes#raw2sym);
        let symmetry = packed `Int32.modulo` 8;
        let withSymmetry = plain + symmetry;
        let movedRest = rankThroughArray(
            Vector.get(withSymmetry, tables#forward),
            Vector.get(withSymmetry, tables#back), scratch, 10) `Int32.modulo` rawCount();
        (packed / 8) * rawCount() + movedRest

    ///
    /// Where a pairing sits in the table.
    ///
    /// The first coordinate is canonicalised by symmetry, and the same
    /// symmetry is then applied to the second -- that coupling is the whole
    /// point of the fold, and getting it wrong would index a table of the
    /// right size with the wrong number.
    ///
    /// Used once per solve, so it may allocate; the transitions inside the
    /// search use `after`, which does not.
    ///
    pub def indexOf(
        classes: { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 },
        state: State
    ): Int32 =
        let packed = Vector.get(rank(state, 4), classes#raw2sym);
        let symmetry = packed `Int32.modulo` 8;
        let rest = rank(rotate(state, symmetry), 10) `Int32.modulo` rawCount();
        (packed / 8) * rawCount() + rest

    ///
    /// Tries each of the phase's moves from the state in `scratch`.
    ///
    def step(
        table: Array[Int32, r],
        scratch: Array[Int32, r],
        classes: { sym2raw = Vector[Int32], raw2sym = Vector[Int32], selfSymmetry = Vector[Int32], classes = Int32 },
        tables: { forward = Vector[Vector[Int32]], back = Vector[Vector[Int32]] },
        m: Int32,
        next: Int32,
        filled: Int32
    ): Int32 \ r =
        if (m >= edgeMoves())
            filled
        else
            let idx = moved(classes, tables, scratch, m);
            let after =
                if (get(table, idx) == 3) {
                    put(table, idx, next);
                    filled + 1
                } else
                    filled;
            step(table, scratch, classes, tables, m + 1, next, after)

    ///
    /// The two bits at `index`.
    ///
    pub def get(table: Array[Int32, r], index: Int32): Int32 \ r =
        Int32.bitwiseAnd(Int32.rightShift(Array.get(index / 16, table), (index `Int32.modulo` 16) * 2), 3)

    ///
    /// Writes the two bits at `index`, which must be unwritten.
    ///
    pub def put(table: Array[Int32, r], index: Int32, value: Int32): Unit \ r =
        let word = index / 16;
        let shift = (index `Int32.modulo` 16) * 2;
        Array.put(Int32.bitwiseXor(Array.get(word, table), Int32.leftShift(Int32.bitwiseXor(3, value), shift)), word, table)

}
