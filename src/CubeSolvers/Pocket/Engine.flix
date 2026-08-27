///
/// A prepared pocket cube solver.
///
/// The engine *is* its table, so preparing one is the expensive part and
/// searching with it is not. That is why the table is a value passed around
/// rather than a cache hidden behind the call: a caller who wants to pay the
/// cost once, or to pay it at startup, or never, can see exactly where the
/// cost is.
///
mod CubeSolvers.Pocket.Engine {

    use CubeSolvers.Kind
    use CubeSolvers.Pocket.Search.{table, solution}
    use CubeSolvers.Pocket.Stickers.ofFacelets
    use CubeSolvers.Solver.{Facelets, Options, Outcome}

    ///
    /// A solver holding a distance table and the depth it was built to.
    ///
    pub enum Engine {
        case Engine(Map[CubeSolvers.Pocket.State, Int32], Int32)
    }

    ///
    /// An engine whose table reaches `depth` moves from solved.
    ///
    /// Deeper is faster to search and slower to build. The search stays
    /// complete at any depth -- it walks further out to compensate -- so this
    /// trades preparation against solving and nothing else.
    ///
    pub def prepared(depth: Int32): Engine = Engine.Engine(table(depth), depth)

    ///
    /// An engine with a table six moves deep.
    ///
    /// God's number for the pocket cube is 11, so six leaves at most five
    /// moves to walk, which is the point where building and searching cost
    /// about the same.
    ///
    pub def standard(): Engine = prepared(6)

    ///
    /// The longest a shortest solution can be, for any pocket cube.
    ///
    pub def godsNumber(): Int32 = 11

    ///
    /// The pocket engine solves without any effect at all: no clock, no file, no
    /// mutable state outliving the call. `Aef = {}` says so, and a caller in a
    /// pure function may use it.
    ///
    instance CubeSolvers.Solver.Solver[Engine] {

        type Aef = {}

        pub def kind(_solver: Engine): Kind = Kind.Cube2x2

        pub def search(
            solver: Engine,
            cube: Facelets,
            options: Options
        ): Outcome =
            let Engine.Engine(tbl, depth) = solver;
            let bound = Int32.min(
                godsNumber(),
                Option.getWithDefault(godsNumber(), options#maxMoves));
            match ofFacelets(cube) {
                case Err(why) => Outcome.Rejected(why)
                case Ok(state) => match solution(tbl, depth, state, bound) {
                    case Some(moves) => Outcome.Solved(moves)
                    case None        =>
                        if (bound < godsNumber())
                            Outcome.BudgetSpent
                        else
                            Outcome.Exhausted
                }
            }

    }

}
