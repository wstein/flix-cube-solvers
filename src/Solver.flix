///
/// The solver contract: the input a solver accepts, the answer it gives back,
/// and the knobs a caller may turn.
///
/// A solver is anything that can turn a legal `Facelets` into moves. What
/// engine sits behind it -- a table in Flix, a Java library, a search written
/// next year -- is not this module's business, which is why the contract is a
/// trait rather than a function.
///
mod Cube.Solver {

    use Cube.{Kind, Face, MoveSequence}

    ///
    /// Anything that can solve a cube.
    ///
    /// `solve` is effectful because every real engine is: it reads a clock to
    /// honour the budget, and the table-driven ones touch memory that outlives
    /// the call. A pure solver may still implement it -- `IO` is a ceiling,
    /// not an obligation.
    ///
    pub trait Solver[t] {

        ///
        /// The cube kind this solver handles. Handing it any other kind is a
        /// `Rejected`, never an exception.
        ///
        pub def kind(solver: t): Kind

        ///
        /// Solves the given state, or explains why it did not.
        ///
        /// Named `search` because both `solve` and `run` are Flix keywords --
        /// the Datalog fixpoint and the effect handler respectively -- and
        /// neither name can be taken back.
        ///
        pub def search(solver: t, cube: Facelets, options: Options): Outcome \ IO

    }

    ///
    /// A cube state, one sticker per position, in URFDLB face order and
    /// row-major within each face.
    ///
    /// Opaque on purpose: every value of this type has been through
    /// `facelets`, so a solver never has to re-check what it was handed.
    ///
    pub enum Facelets with Eq {
        case Checked(Kind, Vector[Face])
    }

    ///
    /// A state prints as its kind and its sticker string, which is what a bug
    /// report needs and what `facelets` accepts back.
    ///
    instance ToString[Facelets] {
        pub def toString(f: Facelets): String =
            let Facelets.Checked(k, stickers) = f;
            let s = stickers |> Vector.map(Cube.faceToken) |> Vector.toList |> List.map(Char.toString) |> List.join("");
            "${k}: ${s}"
    }

    ///
    /// Why a cube state was rejected.
    ///
    /// `WrongLength` and `StickerCounts` are cheap and local. `Unsolvable`
    /// covers the states that are well-formed but unreachable -- a single
    /// flipped edge, a lone twisted corner -- which only an engine that
    /// understands the piece structure can detect, and which no solver may
    /// report as "engine unavailable".
    ///
    pub enum Invalid with Eq {
        /// The state has the wrong number of stickers: expected, then given.
        case WrongLength(Int32, Int32)
        /// A colour appears the wrong number of times: face, expected, given.
        case StickerCounts(Face, Int32, Int32)
        /// Well-formed, but no sequence of moves reaches it from solved.
        case Unsolvable(String)
    }

    ///
    /// Renders the reason as a sentence a caller can show a user unchanged.
    ///
    instance ToString[Invalid] {
        pub def toString(e: Invalid): String = match e {
            case Invalid.WrongLength(expected, actual) =>
                "Expected ${expected} stickers but got ${actual}"
            case Invalid.StickerCounts(face, expected, actual) =>
                "Face ${face} appears ${actual} times, expected ${expected}"
            case Invalid.Unsolvable(why) =>
                "State is unreachable: ${why}"
        }
    }

    ///
    /// Returns a checked cube state, or the first reason it is not one.
    ///
    /// Checks what is checkable without knowing the piece structure: the
    /// sticker count, and that each colour appears exactly as often as it
    /// must. Reachability is left to the engine.
    ///
    pub def facelets(k: Kind, stickers: Vector[Face]): Result[Invalid, Facelets] =
        let expected = Cube.faceletCount(k);
        let actual = Vector.length(stickers);
        if (actual != expected)
            Err(Invalid.WrongLength(expected, actual))
        else
            let n = Cube.order(k);
            let perFace = n * n;
            let wrong = List.findMap(f ->
                let seen = Vector.count(s -> s == f, stickers);
                if (seen != perFace)
                    Some(Invalid.StickerCounts(f, perFace, seen))
                else
                    None,
                faces());
            match wrong {
                case Some(e) => Err(e)
                case None    => Ok(Facelets.Checked(k, stickers))
            }

    ///
    /// The six faces in URFDLB order, which is the order stickers are given in.
    ///
    pub def faces(): List[Face] =
        Face.U :: Face.R :: Face.F :: Face.D :: Face.L :: Face.B :: Nil

    ///
    /// The solved state of the given kind.
    ///
    pub def solved(k: Kind): Facelets =
        let perFace = Cube.order(k) * Cube.order(k);
        let stickers = faces() |> List.flatMap(f -> List.repeat(perFace, f)) |> List.toVector;
        Facelets.Checked(k, stickers)

    ///
    /// The kind of cube the given state describes.
    ///
    pub def kindOf(f: Facelets): Kind =
        let Facelets.Checked(k, _) = f;
        k

    ///
    /// The stickers of the given state, in the order they were given.
    ///
    pub def stickersOf(f: Facelets): Vector[Face] =
        let Facelets.Checked(_, stickers) = f;
        stickers

    ///
    /// What a caller is willing to spend.
    ///
    /// Both are wishes, not guarantees: an engine that cannot honour a bound
    /// says so through `Outcome`, and never by returning a longer solution
    /// than asked for.
    ///
    pub type alias Options = { timeBudgetMillis = Option[Int64], maxMoves = Option[Int32] }

    ///
    /// No bounds: solve until solved.
    ///
    pub def defaultOptions(): Options = { timeBudgetMillis = None, maxMoves = None }

    ///
    /// What came back.
    ///
    /// A solution exists exactly in `Solved`, so a caller cannot reach for one
    /// that was never found. `Exhausted` means the engine searched everything
    /// it can search and there is nothing there; `BudgetSpent` means it ran
    /// out of the caller's time or moves and makes no claim either way.
    ///
    pub enum Outcome with Eq, ToString {
        case Solved(MoveSequence)
        case Rejected(Invalid)
        case BudgetSpent
        case Exhausted
    }


}
