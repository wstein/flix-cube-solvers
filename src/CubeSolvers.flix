///
/// The cube model: what a puzzle is, what a move is, and how moves are
/// written down.
///
/// Nothing here knows how to solve anything. It is the vocabulary every
/// solver in this package speaks, and it is deliberately pure -- parsing and
/// rendering a move sequence must not need a compiler, a table or a clock.
///
mod CubeSolvers {

    ///
    /// The cube sizes this package models.
    ///
    /// Size is intrinsic to the case, not a payload, so an illegal size
    /// cannot be constructed.
    ///
    pub enum Kind with Eq, Order, ToString {
        case Cube2x2
        case Cube3x3
        case Cube4x4
        case Cube5x5
    }

    ///
    /// The order of a cube of the given kind: 2 through 5.
    ///
    pub def order(k: Kind): Int32 = match k {
        case Kind.Cube2x2 => 2
        case Kind.Cube3x3 => 3
        case Kind.Cube4x4 => 4
        case Kind.Cube5x5 => 5
    }

    ///
    /// The number of stickers on a cube of the given kind.
    ///
    pub def faceletCount(k: Kind): Int32 =
        let n = order(k);
        6 * n * n

    ///
    /// The kind of the given order, or `None` when no such cube is modelled.
    ///
    pub def kindOfOrder(n: Int32): Option[Kind] = match n {
        case 2 => Some(Kind.Cube2x2)
        case 3 => Some(Kind.Cube3x3)
        case 4 => Some(Kind.Cube4x4)
        case 5 => Some(Kind.Cube5x5)
        case _ => None
    }

    ///
    /// The six faces, in the conventional URFDLB order.
    ///
    pub enum Face with Eq, Order, ToString {
        case U
        case R
        case F
        case D
        case L
        case B
    }

    ///
    /// The letter that stands for the given face.
    ///
    pub def faceToken(f: Face): Char = match f {
        case Face.U => 'U'
        case Face.R => 'R'
        case Face.F => 'F'
        case Face.D => 'D'
        case Face.L => 'L'
        case Face.B => 'B'
    }

    ///
    /// The face the given letter stands for, or `None`.
    ///
    pub def faceOfToken(c: Char): Option[Face] = match c {
        case 'U' => Some(Face.U)
        case 'R' => Some(Face.R)
        case 'F' => Some(Face.F)
        case 'D' => Some(Face.D)
        case 'L' => Some(Face.L)
        case 'B' => Some(Face.B)
        case _   => None
    }

    ///
    /// How far a layer is turned.
    ///
    /// Only the three quarter-turn multiples that leave the cube in a legal
    /// state exist; there is no `case Identity`, because a move that changes
    /// nothing is not a move.
    ///
    pub enum Turn with Eq, Order, ToString {
        case Cw
        case Half
        case Ccw
    }

    ///
    /// The quarter turns the given turn makes, clockwise.
    ///
    pub def quarterTurns(t: Turn): Int32 = match t {
        case Turn.Cw   => 1
        case Turn.Half => 2
        case Turn.Ccw  => 3
    }

    ///
    /// The turn that makes the given number of quarter turns.
    ///
    /// Counts are taken modulo four, so `4` is not a turn and yields `None`
    /// rather than an identity case.
    ///
    pub def turnOfQuarterTurns(q: Int32): Option[Turn] =
        match Int32.modulo(q, 4) {
            case 1 => Some(Turn.Cw)
            case 2 => Some(Turn.Half)
            case 3 => Some(Turn.Ccw)
            case _ => None
        }

    ///
    /// The turn that undoes the given one.
    ///
    pub def inverseTurn(t: Turn): Turn = match t {
        case Turn.Cw   => Turn.Ccw
        case Turn.Half => Turn.Half
        case Turn.Ccw  => Turn.Cw
    }

    ///
    /// A single layer turn: a face, how many layers deep from it are taken,
    /// and how far they turn.
    ///
    /// `depth` counts layers, so `depth = 1` is the outer layer alone and
    /// `depth = 2` is the outer pair -- the `Uw` of standard notation. Whole
    /// cube rotations are not moves and cannot be written here.
    ///
    /// An enum rather than a record, because Flix derives `Eq` and `Order`
    /// for enums and not for records, and a move that cannot be compared is
    /// no use to a test or a solver.
    ///
    pub enum Move with Eq, Order {
        case Move(Face, Int32, Turn)
    }

    ///
    /// The move that turns `depth` layers of `f` by `t`.
    ///
    pub def move(f: Face, depth: Int32, t: Turn): Move = Move.Move(f, depth, t)

    ///
    /// The face the given move turns against.
    ///
    pub def face(m: Move): Face =
        let Move.Move(f, _, _) = m;
        f

    ///
    /// How many layers deep the given move reaches.
    ///
    pub def depth(m: Move): Int32 =
        let Move.Move(_, d, _) = m;
        d

    ///
    /// How far the given move turns.
    ///
    pub def turn(m: Move): Turn =
        let Move.Move(_, _, t) = m;
        t

    ///
    /// A move prints as itself in standard notation, not as its constructor.
    ///
    instance ToString[Move] {
        pub def toString(m: Move): String = renderMove(m)
    }

    ///
    /// A solution, or any run of moves, applied left to right.
    ///
    pub type alias MoveSequence = List[Move]

    ///
    /// Returns the given move if it is legal on the given kind, or an error
    /// naming what is wrong with it.
    ///
    /// A move must leave something to turn against: `depth` reaching the far
    /// face turns the whole cube, which is a rotation, not a move.
    ///
    pub def validate(k: Kind, m: Move): Result[String, Move] =
        if (depth(m) < 1)
            Err("Depth must be at least 1 but was ${depth(m)}")
        else if (depth(m) >= order(k))
            Err("Depth ${depth(m)} turns the whole ${k}; that is a rotation, not a move")
        else
            Ok(m)

    ///
    /// The move that undoes the given one.
    ///
    pub def inverseMove(m: Move): Move = move(face(m), depth(m), inverseTurn(turn(m)))

    ///
    /// The sequence that undoes the given one.
    ///
    pub def inverse(ms: MoveSequence): MoveSequence =
        ms |> List.map(inverseMove) |> List.reverse

    ///
    /// The given move in standard notation.
    ///
    pub def renderMove(m: Move): String =
        let prefix = if (depth(m) > 2) "${depth(m)}" else "";
        let wide = if (depth(m) > 1) "w" else "";
        let suffix = match turn(m) {
            case Turn.Cw   => ""
            case Turn.Half => "2"
            case Turn.Ccw  => "'"
        };
        "${prefix}${faceToken(face(m))}${wide}${suffix}"

    ///
    /// The move a token stands for, or what is wrong with the token.
    ///
    /// Reads what `renderMove` writes: an optional depth, a face, `w` for a
    /// wide turn, and a suffix. `R` is the outer layer, `Rw` the outer pair,
    /// `3Rw` three deep. A depth without a `w` is refused rather than guessed
    /// at, because `3R` means the third slice alone in some notations and
    /// three layers in others.
    ///
    pub def parseMove(token: String): Result[String, Move] =
        let (digits, rest) = List.span(Char.isDigit, String.toList(token));
        match rest {
            case letter :: tail => forM (
                    f <- faceOfToken(letter) |> Option.toOk("Not a face: ${letter}");
                    d <- parseDepth(digits, tail);
                    t <- parseTurn(dropWide(tail)))
                yield move(f, d, t)
            case Nil => Err("Not a move: ${token}")
        }

    ///
    /// The rest of a token after its `w`, if it had one.
    ///
    def dropWide(tail: List[Char]): List[Char] = match tail {
        case 'w' :: rest => rest
        case _           => tail
    }

    ///
    /// How deep a token turns, from its digits and whether it is wide.
    ///
    def parseDepth(digits: List[Char], tail: List[Char]): Result[String, Int32] =
        let wide = List.head(tail) == Some('w');
        match (digits, wide) {
            case (Nil, false) => Ok(1)
            case (Nil, true)  => Ok(2)
            case (ds, true)   =>
                forM (n <- Int32.fromString(digitsToString(ds)) |> Option.toOk("Not a depth: ${digitsToString(ds)}");
                      ok <- if (n > 1) Ok(n) else Err("A wide turn is at least two layers deep, not ${n}"))
                    yield ok
            case (ds, false)  => Err("A depth needs a 'w': ${digitsToString(ds)}")
        }

    ///
    /// Digits as the number they spell.
    ///
    /// `List.toString` would render them as a list -- `3 :: Nil` -- which
    /// parses as nothing at all.
    ///
    def digitsToString(ds: List[Char]): String = ds |> List.map(Char.toString) |> List.join("")

    ///
    /// How far a token turns, from what follows its face.
    ///
    def parseTurn(suffix: List[Char]): Result[String, Turn] = match suffix {
        case Nil       => Ok(Turn.Cw)
        case '2' :: Nil => Ok(Turn.Half)
        case '\'' :: Nil => Ok(Turn.Ccw)
        case other     => Err("Not a turn: ${digitsToString(other)}")
    }

    ///
    /// The sequence a line of notation stands for.
    ///
    /// Whitespace separates moves and nothing else does, so an empty string
    /// is an empty sequence rather than an error.
    ///
    pub def parse(text: String): Result[String, MoveSequence] =
        text
            |> String.splitOn(substr = " ")
            |> List.map(String.trim)
            |> List.filter(t -> not String.isEmpty(t))
            |> Result.traverse(parseMove)

    ///
    /// The sequence with consecutive turns of one layer combined.
    ///
    /// `F F2` is `F'`, `U U'` is nothing, and removing a pair can bring two
    /// more together -- `U R R' U` is `U2` -- so this works to a fixed point
    /// rather than in one pass. A two-phase search needs it: the two halves
    /// are found separately and meet without knowing they turned the same
    /// face last and first.
    ///
    pub def simplify(ms: MoveSequence): MoveSequence =
        ms |> List.foldLeft(combine, Nil) |> List.reverse

    ///
    /// Adds a move to a reversed sequence, merging it into the last one if
    /// they turn the same layer.
    ///
    def combine(done: MoveSequence, m: Move): MoveSequence = match done {
        case previous :: rest if face(previous) == face(m) and depth(previous) == depth(m) =>
            match turnOfQuarterTurns(quarterTurns(turn(previous)) + quarterTurns(turn(m))) {
                case None    => rest
                case Some(t) => move(face(m), depth(m), t) :: rest
            }
        case _ => m :: done
    }

    ///
    /// The given sequence in standard notation, one space between moves.
    ///
    pub def render(ms: MoveSequence): String =
        ms |> List.map(renderMove) |> List.join(" ")

}
