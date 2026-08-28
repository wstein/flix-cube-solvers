///
/// The 4x4 edges, which are not edges but pairs of wings.
///
/// A 3x3 has twelve edge pieces. A 4x4 has twenty-four wings, two to each of
/// those twelve places, and reduction means getting the right two together in
/// each place -- after which the cube can be turned like a 3x3 and solved
/// like one.
///
/// Which stickers belong to which wing is not tabulated here. Two stickers
/// are the same piece when they sit on the same cubie, and the geometry
/// already knows where every sticker's cubie is.
///
mod CubeSolvers.Revenge.Wings {

    use CubeSolvers.{Face, Kind, Move}
    use CubeSolvers.Solver.Facelets

    ///
    /// The sticker slots that belong to wings, in face order.
    ///
    /// On a 4x4 those are the edge stickers of each face that are not
    /// corners: the middle two of each border row and column.
    ///
    pub def slots(): Vector[Int32] =
        Vector.range(0, 6) |> Vector.flatMap(f ->
            Vector#{1, 2, 4, 7, 8, 11, 13, 14} |> Vector.map(o -> f * 16 + o))

    ///
    /// The two sticker slots of each wing: twenty-four pairs.
    ///
    /// Grouped by where the cubie sits, so no adjacency table is needed --
    /// two stickers on one cubie have one position and two directions.
    ///
    pub def pieces(): Vector[(Int32, Int32)] =
        slots()
            |> Vector.toList
            |> List.map(s -> (cubie(s), s))
            |> List.sortBy(fst)
            |> twoByTwo
            |> List.toVector

    ///
    /// Where a sticker's cubie sits, in the doubled coordinates the geometry
    /// uses.
    ///
    pub def cubie(slot: Int32): (Int32, Int32, Int32) =
        let n = 4;
        let m = n - 1;
        let f = slot / 16;
        let rest = slot `Int32.modulo` 16;
        let r = rest / n;
        let c = rest `Int32.modulo` n;
        match f {
            case 0 => (2 * c - m, m, 2 * r - m)
            case 1 => (m, m - 2 * r, m - 2 * c)
            case 2 => (2 * c - m, m - 2 * r, m)
            case 3 => (2 * c - m, -m, m - 2 * r)
            case 4 => (-m, m - 2 * r, 2 * c - m)
            case _ => (m - 2 * c, m - 2 * r, -m)
        }

    ///
    /// The twelve places a 3x3 would call an edge, each holding two wings.
    ///
    /// Two wings share a place when their cubies lie along the same edge of
    /// the cube: the same two faces, differing only in how far along they
    /// sit.
    ///
    pub def places(): Vector[(Int32, Int32)] =
        pieces()
            |> Vector.toList
            |> List.map(piece -> (along(piece), fst(piece)))
            |> List.sortBy(fst)
            |> twoByTwo
            |> List.toVector

    ///
    /// Consecutive entries taken in twos, keeping only what they carry.
    ///
    /// Sound because each cubie carries exactly two wing stickers and each
    /// edge of the cube carries exactly two wings: sort by the thing they
    /// share and the partners end up side by side.
    ///
    def twoByTwo(xs: List[(k, Int32)]): List[(Int32, Int32)] with Order[k] = match xs {
        case (_, a) :: (_, b) :: rest => (a, b) :: twoByTwo(rest)
        case _                        => Nil
    }

    ///
    /// Which edge of the cube a wing lies along: the two extreme coordinates,
    /// with the third dropped.
    ///
    def along(piece: (Int32, Int32)): (Int32, Int32, Int32) =
        let (a, _) = piece;
        let (x, y, z) = cubie(a);
        let m = 3;
        if (Int32.abs(x) != m)
            (0, y, z)
        else if (Int32.abs(y) != m)
            (x, 0, z)
        else
            (x, y, 0)

    ///
    /// How many of the twelve places hold two wings that match.
    ///
    /// A place is done when both its wings show the same two colours the same
    /// way round -- which is what lets the pair be turned as one edge.
    ///
    pub def paired(cube: Facelets): Int32 =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        places() |> Vector.count(match (one, two) ->
            let (a, b) = wingOf(one);
            let (c, d) = wingOf(two);
            // The two wings' stickers are not listed in the same order, so
            // line them up by which face each sticker is on before comparing.
            let (e, f) = if (sameFace(a, c)) (c, d) else (d, c);
            Vector.get(a, stickers) == Vector.get(e, stickers)
                and Vector.get(b, stickers) == Vector.get(f, stickers))

    ///
    /// The two stickers of the wing whose first sticker is `slot`.
    ///
    def wingOf(slot: Int32): (Int32, Int32) =
        match Vector.find(match (a, b) -> a == slot or b == slot, pieces()) {
            case Some(p) => p
            case None    => (slot, slot)
        }

    ///
    /// Whether two sticker slots are on the same face.
    ///
    def sameFace(a: Int32, b: Int32): Bool = a / 16 == b / 16

    ///
    /// Whether every place holds a matching pair.
    ///
    pub def isReduced(cube: Facelets): Bool = paired(cube) == 12

    ///
    /// Each wing's two stickers, in an order that tells it from its mirror.
    ///
    /// Two wings of one edge show the same two colours, so a colour pair
    /// cannot say which is which. An *ordered* pair can: the two sit at
    /// opposite ends of the edge, and taking the faces in the order the
    /// edge's own direction points distinguishes them. Turning the cube turns
    /// that direction with it, so the rule needs no table and survives every
    /// move.
    ///
    pub def ordered(): Vector[(Int32, Int32)] =
        pieces() |> Vector.map(match (a, b) ->
            let (first, second) = if (faceOf(a) < faceOf(b)) (a, b) else (b, a);
            let along = cross(outward(faceOf(first)), outward(faceOf(second)));
            if (dot(cubie(first), along) > 0) (first, second) else (second, first))

    ///
    /// Which wing sits at each place: the home wing's number, per position.
    ///
    /// A wing showing colours `x` then `y` in its own order is the wing whose
    /// home shows faces `x` then `y` in theirs.
    ///
    pub def identity(cube: Facelets): Vector[Int32] =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let homes = ordered() |> Vector.map(match (a, b) -> (faceOf(a), faceOf(b)));
        ordered() |> Vector.map(match (a, b) ->
            let shown = (faceIndexOf(Vector.get(a, stickers)), faceIndexOf(Vector.get(b, stickers)));
            match Vector.indexOf(shown, homes) {
                case Some(i) => i
                case None    => -1
            })

    ///
    /// Whether the wings are an odd number of swaps from home.
    ///
    /// The 4x4 needs this as a coordinate of its own: the second phase has to
    /// leave the edge permutation's parity matching the corners', and a
    /// single number is all that takes.
    ///
    pub def parity(cube: Facelets): Int32 = permutationParity(identity(cube))

    ///
    /// The parity of a permutation, counted as inversions.
    ///
    pub def permutationParity(perm: Vector[Int32]): Int32 =
        let n = Vector.length(perm);
        let inversions = List.range(0, n) |> List.sumWith(i ->
            List.range(i + 1, n) |> List.count(j -> Vector.get(i, perm) > Vector.get(j, perm)));
        inversions `Int32.modulo` 2

    ///
    /// Which face a sticker slot is on, as a number.
    ///
    def faceOf(slot: Int32): Int32 = slot / 16

    ///
    /// Which face a colour is, as a number.
    ///
    def faceIndexOf(f: Face): Int32 = match f {
        case Face.U => 0
        case Face.R => 1
        case Face.F => 2
        case Face.D => 3
        case Face.L => 4
        case Face.B => 5
    }

    ///
    /// The direction a face looks.
    ///
    def outward(f: Int32): (Int32, Int32, Int32) = match f {
        case 0 => (0, 1, 0)
        case 1 => (1, 0, 0)
        case 2 => (0, 0, 1)
        case 3 => (0, -1, 0)
        case 4 => (-1, 0, 0)
        case _ => (0, 0, -1)
    }

    ///
    /// The direction along the edge two faces meet at.
    ///
    def cross(p: (Int32, Int32, Int32), q: (Int32, Int32, Int32)): (Int32, Int32, Int32) =
        let (a, b, c) = p;
        let (d, e, f) = q;
        (b * f - c * e, c * d - a * f, a * e - b * d)

    ///
    /// How far along a direction a point lies.
    ///
    def dot(p: (Int32, Int32, Int32), q: (Int32, Int32, Int32)): Int32 =
        let (a, b, c) = p;
        let (d, e, f) = q;
        a * d + b * e + c * f

}
