///
/// The 4x4 solver: three phases of reduction, then the 3x3 engine.
///
/// Tsai's method by way of TPR-4x4x4-Solver. The first phase separates the R
/// and L centres, the second settles the rest and leaves the edge and corner
/// parities agreeing, the third finishes the centres and pairs the edges --
/// and what is left turns like a 3x3, so a 3x3 solves it.
///
/// The second phase is the awkward one. Its answers must leave the wings
/// split between the two orbits the third phase moves them in, and it does
/// not promise that: roughly one answer in seven hundred qualifies. So its
/// answers are enumerated and sifted rather than taken.
///
mod CubeSolvers.Revenge.Engine {

    use CubeSolvers.{Move, MoveSequence}
    use CubeSolvers.Solver.{Facelets, Invalid}
    use CubeSolvers.Revenge.{Centers, Separate, Pairing, Pairs, Reduced, Walk}

    ///
    /// Everything the solver needs, built once.
    ///
    pub type alias Tables[r: Eff] = {
        separating = Vector[Int32],
        settling = Vector[Int32],
        steps = Separate.Steps,
        pairing = Pairing.Tables[r],
        rubik = CubeSolvers.Rubik.Engine.Engine
    }

    ///
    /// How many second-phase answers to sift before giving up on a length.
    ///
    /// About one in seven hundred separates the wings, so a few thousand is
    /// the right order. The reference tries five hundred per first-phase
    /// answer and ten thousand first-phase answers.
    ///
    pub def sifted(): Int32 = 3000

    ///
    /// How long a third phase may run to.
    ///
    pub def deepestPairing(): Int32 = 15

    ///
    /// Builds every table. Minutes, once: the edge table alone is 31 million
    /// states.
    ///
    pub def prepared(rc: Region[r]): Tables[r] \ r =
        { separating = Centers.table(),
          settling = Separate.table(),
          steps = Separate.steps(),
          pairing = Pairing.prepared(rc),
          rubik = CubeSolvers.Rubik.Engine.prepared() }

    ///
    /// The moves that solve a 4x4, or why they were not found.
    ///
    pub def solution(t: Tables[r], cube: Facelets): Result[Invalid, MoveSequence] \ r =
        forM (first <- Walk.down(cube, t#separating, Centers.separation, Centers.moves());
              afterFirst <- CubeSolvers.Turning.replay(cube, first);
              rest <- fromSettled(t, afterFirst))
            yield CubeSolvers.simplify(first ::: rest)

    ///
    /// The rest of the solve, from a cube whose R and L centres are separated.
    ///
    def fromSettled(t: Tables[r], cube: Facelets): Result[Invalid, MoveSequence] \ r =
        let at = Separate.coordinate(cube);
        let shortest = Vector.get(at, t#settling);
        if (shortest < 0)
            Err(Invalid.Unsolvable("the second phase cannot settle this cube"))
        else
            match trySettling(t, cube, at, shortest, shortest + 4) {
                case Some(moves) => Ok(moves)
                case None        => Err(Invalid.Unsolvable("no second phase answer left the wings separated and finishable"))
            }

    ///
    /// Tries each second-phase length in turn, sifting its answers.
    ///
    def trySettling(
        t: Tables[r],
        cube: Facelets,
        at: Int32,
        depth: Int32,
        limit: Int32
    ): Option[MoveSequence] \ r =
        if (depth > limit)
            None
        else
            let answers = Separate.manyExactly(t#steps, t#settling, at, depth, sifted());
            match List.findMap(answer -> finishFrom(t, cube, answer), answers) {
                case Some(moves) => Some(moves)
                case None        => trySettling(t, cube, at, depth + 1, limit)
            }

    ///
    /// One second-phase answer, taken through to the end if it separates the
    /// wings and the third phase can finish it.
    ///
    def finishFrom(t: Tables[r], cube: Facelets, answer: List[Int32]): Option[MoveSequence] \ r =
        let second = Separate.sequence(answer);
        match CubeSolvers.Turning.replay(cube, second) {
            case Err(_)       => None
            case Ok(settled)  =>
                if (not Pairs.isSeparatedIn(t#pairing#layout, settled))
                    None
                else
                    match Pairing.solution(t#pairing, settled, deepestPairing()) {
                        case None       => None
                        case Some(third) => match CubeSolvers.Turning.replay(settled, third) {
                            case Err(_)      => None
                            case Ok(reduced) => finishAsRubik(t, reduced)
                                |> Option.map(last -> second ::: third ::: last)
                        }
                    }
        }

    ///
    /// The last stage: a reduced cube handed to the 3x3 engine.
    ///
    def finishAsRubik(t: Tables[r], cube: Facelets): Option[MoveSequence] =
        match Reduced.asRubik(cube) {
            case Err(_)  => None
            case Ok(as3) => match CubeSolvers.Solver.search(t#rubik, as3, CubeSolvers.Solver.defaultOptions()) {
                case CubeSolvers.Solver.Outcome.Solved(moves) => Some(moves)
                case _                                        => None
            }
        }

}
