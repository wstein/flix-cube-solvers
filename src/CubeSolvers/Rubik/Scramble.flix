///
/// Cryptographically-random 3x3 random-state scrambles.
///
/// The sampled state is uniform over legal fixed-centre 3x3 positions, except
/// for the solved state and its eighteen one-move neighbours. That is the WCA
/// random-state condition for 3x3. The supplied two-phase engine then writes
/// a sequence to reach it; that sequence is deliberately not claimed shortest.
///
mod CubeSolvers.Rubik.Scramble {

    import java.security.SecureRandom

    use CubeSolvers.MoveSequence
    use CubeSolvers.Rubik.State
    use CubeSolvers.Rubik.Engine.{Engine, defaultBound, solution}

    ///
    /// A cryptographically-random legal 3x3 state requiring at least two
    /// moves to solve.
    ///
    pub def secureState(): State \ IO =
        let random = new SecureRandom();
        sample(random)

    ///
    /// A cryptographically-random random-state scramble, suitable in shape
    /// for 3x3 competition practice. It is not an official WCA program:
    /// official competitions must use the current TNoodle.
    ///
    pub def secure(engine: Engine): MoveSequence \ IO =
        let state = secureState();
        match solution(engine, state, defaultBound()) {
            case Some(home) => CubeSolvers.inverse(home)
            case None       => Nil
        }

    def sample(random: SecureRandom): State \ IO =
        let corners = shuffle(random, List.range(0, 8), Nil);
        let edges = shuffle(random, List.range(0, 12), Nil);
        let legalEdges = if (even(corners) == even(edges)) edges else swapFirst(edges);
        let twists = digits(random, 7, 3, Nil, 0);
        let flips = digits(random, 11, 2, Nil, 0);
        let state = State.State(corners |> List.toVector, twists |> List.toVector,
                                legalEdges |> List.toVector, flips |> List.toVector);
        if (tooEasy(state)) sample(random) else state

    /// A uniform permutation, created by Fisher-Yates selection.
    def shuffle(random: SecureRandom, remaining: List[Int32], done: List[Int32]): List[Int32] \ IO =
        match remaining {
            case Nil => List.reverse(done)
            case _   =>
                let (picked, rest) = removeAt(remaining, random.nextInt(List.length(remaining)));
                shuffle(random, rest, picked :: done)
        }

    /// Independent orientations, with the final orientation fixed by its law.
    def digits(random: SecureRandom, remaining: Int32, base: Int32, done: List[Int32], sum: Int32): List[Int32] \ IO =
        if (remaining == 0)
            List.reverse(done) ::: Int32.modulo(base - Int32.modulo(sum, base), base) :: Nil
        else
            let digit = random.nextInt(base);
            digits(random, remaining - 1, base, digit :: done, sum + digit)

    /// The parity of a permutation, calculated from its inversions.
    def even(xs: List[Int32]): Bool =
        Int32.modulo(inversions(xs), 2) == 0

    def inversions(xs: List[Int32]): Int32 = match xs {
        case x :: rest => List.count(y -> x > y, rest) + inversions(rest)
        case Nil       => 0
    }

    def swapFirst(xs: List[Int32]): List[Int32] = match xs {
        case a :: b :: rest => b :: a :: rest
        case _              => xs
    }

    def removeAt(xs: List[Int32], at: Int32): (Int32, List[Int32]) = match xs {
        case x :: rest =>
            if (at == 0)
                (x, rest)
            else
                let (picked, others) = removeAt(rest, at - 1);
                (picked, x :: others)
        case Nil => (0, Nil)
    }

    /// Exactly the states the WCA's general minimum excludes for 3x3.
    def tooEasy(state: State): Bool =
        state == CubeSolvers.Rubik.solved() or
        (CubeSolvers.Rubik.steps(CubeSolvers.Rubik.solved()) |> Vector.exists(s -> snd(s) == state))

}
