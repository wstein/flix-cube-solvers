///
/// A prepared 3x3 solver, in two phases.
///
/// The engine is its tables, and building them is the expensive part -- a few
/// seconds, once. Solving afterwards is a search over small integers.
///
/// Solutions are short but not shortest. A two-phase search finds the best
/// answer of the form "reduce, then finish", and the shortest solution to a
/// cube need not have that shape: this is the trade that makes solving a 3x3
/// quick, and it is why `Cube.Pocket` promises optimality and this does not.
///
mod Cube.Rubik.Engine {

    use Cube.{Kind, Move}
    use Cube.Rubik.{State, replay}
    use Cube.Rubik.Phase1
    use Cube.Rubik.Phase2
    use Cube.Rubik.Stickers.ofFacelets
    use Cube.Solver.{Facelets, Options, Outcome}

    ///
    /// A solver holding the tables of both phases.
    ///
    pub enum Engine {
        case Engine(Phase1.Tables, Phase2.Tables)
    }

    ///
    /// Builds both phases' tables. Seconds, once.
    ///
    pub def prepared(): Engine = Engine.Engine(Phase1.tables(), Phase2.tables())

    ///
    /// The longest a two-phase solution is allowed to get by default.
    ///
    /// The first phase never needs more than twelve moves and the second never
    /// more than eighteen, so nothing is lost by refusing to look past thirty.
    ///
    pub def defaultBound(): Int32 = 30

    ///
    /// The longest first phase worth trying.
    ///
    def deepestFirstPhase(): Int32 = 12

    ///
    /// A solution of at most `maxMoves` moves, or `None`.
    ///
    /// Every first-phase length from the shortest possible upwards is tried,
    /// and each is finished as far as the best answer so far allows. The
    /// result is simplified, because the two halves are found separately and
    /// meet without knowing they turned the same face last and first.
    ///
    /// Named `solution` because `solve` is the Datalog keyword.
    ///
    pub def solution(e: Engine, s: State, maxMoves: Int32): Option[List[Move]] =
        let Engine.Engine(t1, t2) = e;
        attempt(t1, t2, s, Phase1.estimate(t1, s), maxMoves, None)

    ///
    /// Tries a first phase of exactly `d1` moves, then the next length.
    ///
    def attempt(
        t1: Phase1.Tables,
        t2: Phase2.Tables,
        s: State,
        d1: Int32,
        maxMoves: Int32,
        best: Option[List[Move]]
    ): Option[List[Move]] =
        let bestLength = best |> Option.map(List.length) |> Option.getWithDefault(maxMoves + 1);
        if (d1 > deepestFirstPhase() or d1 >= bestLength)
            best
        else
            attempt(t1, t2, s, d1 + 1, maxMoves, improve(t1, t2, s, d1, bestLength, best))

    ///
    /// The better of `best` and whatever a first phase of `d1` moves leads to.
    ///
    def improve(
        t1: Phase1.Tables,
        t2: Phase2.Tables,
        s: State,
        d1: Int32,
        bestLength: Int32,
        best: Option[List[Move]]
    ): Option[List[Move]] =
        match Phase1.exactly(t1, s, d1) {
            case None      => best
            case Some(one) => match replay(s, one) {
                case None     => best
                case Some(g1) => match Phase2.finish(t2, g1, bestLength - d1 - 1) {
                    case None      => best
                    case Some(two) =>
                        let whole = Cube.simplify(one ::: two);
                        if (List.length(whole) < bestLength) Some(whole) else best
                }
            }
        }

    ///
    /// The 3x3 engine reads no clock and touches nothing that outlives the
    /// call, so it declares no effect and may be used from a pure function.
    ///
    instance Cube.Solver.Solver[Engine] {

        type Aef = {}

        pub def kind(_solver: Engine): Kind = Kind.Cube3x3

        pub def search(solver: Engine, cube: Facelets, options: Options): Outcome =
            let bound = Int32.min(defaultBound(), Option.getWithDefault(defaultBound(), options#maxMoves));
            match ofFacelets(cube) {
                case Err(why)  => Outcome.Rejected(why)
                case Ok(state) => match solution(solver, state, bound) {
                    case Some(ms) => Outcome.Solved(ms)
                    case None     =>
                        if (bound < defaultBound())
                            Outcome.BudgetSpent
                        else
                            Outcome.Exhausted
                }
            }

    }

}
