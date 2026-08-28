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
    /// Measured, one in 459 of them separates the wings -- close to the
    /// `2^12 / C(24, 12)` the geometry predicts. What matters more than the
    /// count is that the answers be spread out: read in one fixed move order
    /// they share prefixes, and share what happens to the wings along them.
    /// The reference tries five hundred per first-phase answer.
    ///
    pub def sifted(): Int32 = 200

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
    /// Reduction is gathered first and searched second. Every first-phase
    /// answer is taken through the second phase, and every second-phase answer
    /// that leaves the wings separated is kept along with what the third phase
    /// must at least cost from there. Only then is anything searched, cheapest
    /// first -- because a third-phase search whose budget is too small still
    /// walks the whole tree before saying so, and there is no point paying
    /// that for a candidate a lookup has already ruled out.
    ///
    pub def solution(t: Tables[r], cube: Facelets): Result[Invalid, MoveSequence] \ r =
        match Walk.down(cube, t#separating, Centers.separation, Centers.moves()) {
            case Err(e) => Err(e)
            case Ok(_)  =>
                let ready = gather(t, cube);
                if (List.isEmpty(ready))
                    Err(Invalid.Unsolvable("no reduction left the wings separated"))
                else
                    match tryEach(t, List.sortBy(match (v, _, _) -> v, ready), budget(ready)) {
                        case Some(answer) => Ok(CubeSolvers.simplify(answer))
                        case None         => Err(Invalid.Unsolvable(
                            "the third phase could not finish any reduction"))
                    }
        }

    ///
    /// How many first-phase answers to try.
    ///
    /// Each one gives the wings a different history, which is what the sift
    /// needs: answers sharing a prefix drag the wings along the same way, so
    /// a single first-phase answer yields far fewer independent tries than
    /// its second-phase answers suggest. The reference keeps five hundred.
    ///
    pub def firstAnswers(): Int32 = 60

    ///
    /// Every reduction worth searching: what it cost, where it leaves the
    /// cube, and the least the third phase can add.
    ///
    def gather(t: Tables[r], cube: Facelets): List[(Int32, MoveSequence, Facelets)] \ r =
        Walk.manyWith(cube, t#separating, Centers.separation, Centers.moves(), firstAnswers())
            |> List.mapWithIndex(k -> pair -> (k, pair))
            |> List.flatMap(match (k, (first, afterFirst)) -> {
                let at = Separate.coordinate(afterFirst);
                let shortest = Vector.get(at, t#settling);
                if (shortest < 0)
                    Nil
                else
                    List.range(0, slack() + 1) |> List.flatMap(extra ->
                        Separate.manyExactlyFrom(t#steps, t#settling, at,
                                shortest + extra, sifted(), k * 5 + extra)
                            |> List.filterMap(answer -> {
                                let second = Separate.sequence(answer);
                                match CubeSolvers.Turning.replay(afterFirst, second) {
                                    case Err(_)      => None
                                    case Ok(settled) =>
                                        if (not Pairs.isSeparatedIn(t#pairing#layout, settled))
                                            None
                                        else
                                            let used = List.length(first) + List.length(second);
                                            let least = Pairing.bound(t#pairing, settled);
                                            if (least < 0)
                                                None
                                            else
                                                Some((used + least, first ::: second, settled))
                                }
                            }))
            })

    ///
    /// How many moves past the shortest the second phase may spend.
    ///
    def slack(): Int32 = 2

    ///
    /// The budget to start at: what the most promising reduction promises.
    ///
    def budget(ready: List[(Int32, MoveSequence, Facelets)]): Int32 =
        ready |> List.map(match (v, _, _) -> v) |> List.minimum |> Option.getWithDefault(0)

    ///
    /// Searches the reductions within the budget, then widens it.
    ///
    def tryEach(
        t: Tables[r],
        ready: List[(Int32, MoveSequence, Facelets)],
        allowed: Int32
    ): Option[MoveSequence] \ r =
        if (allowed > longest())
            None
        else
            let within = ready |> List.filter(match (v, _, _) -> v <= allowed);
            match List.findMap(match (_, done, settled) ->
                    finishFrom(t, done, settled, allowed - List.length(done)), within) {
                case Some(answer) => Some(answer)
                case None         => tryEach(t, ready, allowed + 1)
            }

    ///
    /// The longest reduction this engine will look for.
    ///
    def longest(): Int32 = 32

    ///
    /// One second-phase answer, taken through to the end if it separates the
    /// wings and the third phase can finish it.
    ///
    def finishFrom(
        t: Tables[r],
        done: MoveSequence,
        settled: Facelets,
        allowed: Int32
    ): Option[MoveSequence] \ r =
        match Pairing.solution(t#pairing, settled, Int32.min(allowed, deepestPairing())) {
            case None        => None
            case Some(third) => match CubeSolvers.Turning.replay(settled, third) {
                case Err(_)      => None
                case Ok(reduced) => finishAsRubik(t, reduced)
                    |> Option.map(last -> done ::: third ::: last)
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
