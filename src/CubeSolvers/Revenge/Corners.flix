///
/// The 4x4's corners, which behave exactly as a 3x3's.
///
/// Only one thing is wanted from them here: the parity of their permutation.
/// A reduced 4x4 can present a position no 3x3 ever does -- two edges
/// swapped, everything else solved -- and what rules it out is the edge
/// permutation's parity agreeing with the corners'. The second phase has to
/// leave them agreeing, so it has to be able to see them.
///
mod CubeSolvers.Revenge.Corners {

    use CubeSolvers.Face
    use CubeSolvers.Solver.Facelets

    ///
    /// The sticker slots that belong to corners, in face order.
    ///
    /// The four corners of each face: rows and columns zero and three.
    ///
    pub def slots(): Vector[Int32] =
        Vector.range(0, 6) |> Vector.flatMap(f ->
            Vector#{0, 3, 12, 15} |> Vector.map(o -> f * 16 + o))

    ///
    /// The three sticker slots of each corner: eight triples.
    ///
    /// Grouped by cubie, the same way wings are: three stickers on one cubie
    /// are one piece.
    ///
    pub def pieces(): Vector[(Int32, Int32, Int32)] =
        slots()
            |> Vector.toList
            |> List.map(s -> (cubie(s), s))
            |> List.sortBy(fst)
            |> threeByThree
            |> List.toVector

    ///
    /// Consecutive entries taken in threes.
    ///
    def threeByThree(xs: List[((Int32, Int32, Int32), Int32)]): List[(Int32, Int32, Int32)] = match xs {
        case (_, a) :: (_, b) :: (_, c) :: rest => (a, b, c) :: threeByThree(rest)
        case _                                  => Nil
    }

    ///
    /// Where a sticker's cubie sits.
    ///
    def cubie(slot: Int32): (Int32, Int32, Int32) = CubeSolvers.Revenge.Wings.cubie(slot)

    ///
    /// Which corner sits at each place.
    ///
    /// A corner shows three colours and no two corners show the same three,
    /// so the set of colours names it -- no order and no mirror to worry
    /// about, unlike the wings.
    ///
    pub def identity(cube: Facelets): Vector[Int32] =
        let stickers = CubeSolvers.Solver.stickersOf(cube);
        let homes = pieces() |> Vector.map(match (a, b, c) ->
            List.sort(faceOf(a) :: faceOf(b) :: faceOf(c) :: Nil));
        pieces() |> Vector.map(match (a, b, c) ->
            let shown = List.sort(
                colourOf(Vector.get(a, stickers))
                    :: colourOf(Vector.get(b, stickers))
                    :: colourOf(Vector.get(c, stickers)) :: Nil);
            match Vector.indexOf(shown, homes) {
                case Some(i) => i
                case None    => -1
            })

    ///
    /// Whether the corners are an odd number of swaps from home.
    ///
    pub def parity(cube: Facelets): Int32 =
        CubeSolvers.Revenge.Wings.permutationParity(identity(cube))

    ///
    /// Which face a sticker slot is on, as a number.
    ///
    def faceOf(slot: Int32): Int32 = slot / 16

    ///
    /// Which face a colour is, as a number.
    ///
    def colourOf(f: Face): Int32 = match f {
        case Face.U => 0
        case Face.R => 1
        case Face.F => 2
        case Face.D => 3
        case Face.L => 4
        case Face.B => 5
    }

}
