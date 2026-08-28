///
/// Random-state scrambles for the pocket cube.
///
/// The state representation fixes the DBL corner, so its 3,674,160 states
/// are precisely the 2x2 positions up to whole-cube rotation. `sample` draws
/// each of those states with equal probability when its seed stream is
/// uniform. `wca` rejects the states fewer than four moves from solved, as
/// the WCA's 2x2 specification requires, then asks the exact solver for a
/// shortest route and reverses it into a scramble.
///
/// This is compatible with the *state* requirement, but is not an official
/// WCA scramble program: official competitions must use the current TNoodle.
///
mod CubeSolvers.Pocket.Scramble {

    import java.security.SecureRandom

    use CubeSolvers.MoveSequence
    use CubeSolvers.Pocket.State
    use CubeSolvers.Pocket.Engine.Engine
    use CubeSolvers.Solver.{Outcome, defaultOptions, search}

    ///
    /// A uniformly selected fixed-corner state, followed by the next seed.
    ///
    /// The seed is advanced by the Park-Miller generator. It makes this API
    /// reproducible for tests and callers that supply their own entropy; it
    /// deliberately does not claim that a caller's seed is unpredictable.
    ///
    pub def sample(seed: Int64): (State, Int64) =
        let (permutation, afterPermutation) = shuffled(seed, mobileCorners(), Nil);
        let (twists, afterTwists) = randomTwists(afterPermutation, 6, Nil, 0);
        (State.State(insertPinned(permutation, 6) |> List.toVector,
                     insertPinned(twists, 0) |> List.toVector), afterTwists)

    ///
    /// A random-state scramble requiring at least four moves to solve,
    /// followed by the seed for the next draw.
    ///
    /// `engine` must be an exact pocket engine. `Engine.standard()` is the
    /// intended value: it proves the route shortest while retaining only its
    /// six-move table between calls.
    ///
    pub def wca(engine: Engine, seed: Int64): (MoveSequence, Int64) =
        let (state, next) = sample(seed);
        match search(engine, CubeSolvers.Pocket.Stickers.toFacelets(state), defaultOptions()) {
            case Outcome.Solved(home) if List.length(home) >= 4 => (CubeSolvers.inverse(home), next)
            case Outcome.Solved(_) => wca(engine, next)
            case _ => wca(engine, next)
        }

    ///
    /// A scramble whose resulting position is exactly `distance` moves from
    /// solved, followed by the next seed.
    ///
    /// This is for controlled practice: the number is a proven minimum, not
    /// merely the number of moves printed. Values from 0 through 11 are
    /// meaningful for the pocket cube; callers should use that range.
    ///
    pub def atDistance(seed: Int64, distance: Int32): (MoveSequence, Int64) =
        let table = CubeSolvers.Pocket.Search.table(distance);
        let states = statesAt(table, distance);
        let (at, next) = draw(seed, List.length(states));
        let state = stateAt(states, at);
        match CubeSolvers.Pocket.Search.descend(table, state) {
            case Some(home) => (CubeSolvers.inverse(home), next)
            case None       => (Nil, next)
        }

    ///
    /// An easy practice scramble, uniformly choosing one certified distance
    /// from 3, 4, and 5 before sampling a state at that distance.
    ///
    /// Unlike `wca`, this deliberately is not a competition distribution.
    ///
    pub def easy(seed: Int64): (MoveSequence, Int64) =
        let (offset, next) = draw(seed, 3);
        atDistance(next, offset + 3)

    ///
    /// A cryptographically-random easy practice scramble.
    ///
    /// Unlike the seeded methods, this draws every choice directly from
    /// `SecureRandom`; it does not reduce strong entropy to a small seed.
    ///
    pub def secureEasy(): MoveSequence \ IO =
        let random = new SecureRandom();
        secureAtDistance(random, random.nextInt(3) + 3)

    ///
    /// A cryptographically-random 2x2 random-state scramble meeting the WCA
    /// minimum of four moves.
    ///
    /// The output still is not an official competition scramble: the WCA
    /// requires its current TNoodle program for that purpose.
    ///
    pub def secureWca(engine: Engine): MoveSequence \ IO =
        let random = new SecureRandom();
        secureAtLeastFour(engine, random)

    /// The non-zero state of the Park-Miller generator after `seed`.
    def next(seed: Int64): Int64 =
        let modulus = 2147483647i64;
        let normalized = Int64.modulo(Int64.abs(seed), modulus - 1i64) + 1i64;
        Int64.modulo(normalized * 48271i64, modulus)

    /// A uniform integer in [0, bound), with rejection avoiding modulo bias.
    def draw(seed: Int64, bound: Int32): (Int32, Int64) =
        let after = next(seed);
        let size = 2147483646i64;
        let width = Int32.toInt64(bound);
        let limit = size - Int64.modulo(size, width);
        let candidate = after - 1i64;
        if (candidate < limit)
            (Int64.tryToInt32(Int64.modulo(candidate, width)) |> Option.getWithDefault(0), after)
        else
            draw(after, bound)

    /// The seven corners that move, which is every corner but the pinned one.
    ///
    /// DBL sits at index six and is held still, so it is not shuffled -- it is
    /// put back afterwards by `insertPinned`. Shuffling `0` through `6` would
    /// deal that corner twice and corner seven never, and the result is not a
    /// cube at all: it fails to decode, and a sampler that retries until it
    /// decodes never returns.
    def mobileCorners(): List[Int32] = List.range(0, 6) ::: 7 :: Nil

    /// A Fisher-Yates shuffle, expressed by repeatedly selecting one item.
    def shuffled(seed: Int64, remaining: List[Int32], done: List[Int32]): (List[Int32], Int64) =
        match remaining {
            case Nil => (List.reverse(done), seed)
            case _   =>
                let (at, after) = draw(seed, List.length(remaining));
                let (chosen, rest) = removeAt(remaining, at);
                shuffled(after, rest, chosen :: done)
        }

    /// Six independent twists, with the seventh determined by the twist law.
    def randomTwists(seed: Int64, remaining: Int32, done: List[Int32], sum: Int32): (List[Int32], Int64) =
        if (remaining == 0)
            ((List.reverse(done) ::: Int32.modulo(3 - Int32.modulo(sum, 3), 3) :: Nil), seed)
        else
            let (twist, after) = draw(seed, 3);
            randomTwists(after, remaining - 1, twist :: done, sum + twist)

    /// The selected item and every other item, preserving their order.
    def removeAt(xs: List[Int32], at: Int32): (Int32, List[Int32]) = match xs {
        case x :: rest =>
            if (at == 0)
                (x, rest)
            else
                let (chosen, others) = removeAt(rest, at - 1);
                (chosen, x :: others)
        case Nil => (0, Nil)
    }

    /// Every state at one exact table distance.
    def statesAt(table: Map[State, Int32], distance: Int32): List[State] =
        Map.toList(table)
            |> List.filter(match (_, d) -> d == distance)
            |> List.map(match (state, _) -> state)

    /// The state at a valid list index.
    def stateAt(states: List[State], at: Int32): State = match states {
        case state :: rest => if (at == 0) state else stateAt(rest, at - 1)
        case Nil => CubeSolvers.Pocket.solved()
    }

    /// Inserts the fixed DBL corner after the first six mobile cubicles.
    def insertPinned(xs: List[Int32], pinned: Int32): List[Int32] =
        insertAfter(xs, pinned, 6)

    def insertAfter(xs: List[Int32], value: Int32, count: Int32): List[Int32] =
        if (count == 0)
            value :: xs
        else match xs {
            case x :: rest => x :: insertAfter(rest, value, count - 1)
            case Nil => value :: Nil
        }

    /// A uniformly selected state using Java's cryptographically strong RNG.
    def secureSample(random: SecureRandom): State \ IO =
        let permutation = secureShuffle(random, List.range(0, 7), Nil);
        let twists = secureTwists(random, 6, Nil, 0);
        State.State(insertPinned(permutation, 6) |> List.toVector,
                    insertPinned(twists, 0) |> List.toVector)

    def secureShuffle(random: SecureRandom, remaining: List[Int32], done: List[Int32]): List[Int32] \ IO =
        match remaining {
            case Nil => List.reverse(done)
            case _   =>
                let (chosen, rest) = removeAt(remaining, random.nextInt(List.length(remaining)));
                secureShuffle(random, rest, chosen :: done)
        }

    def secureTwists(random: SecureRandom, remaining: Int32, done: List[Int32], sum: Int32): List[Int32] \ IO =
        if (remaining == 0)
            List.reverse(done) ::: Int32.modulo(3 - Int32.modulo(sum, 3), 3) :: Nil
        else
            let twist = random.nextInt(3);
            secureTwists(random, remaining - 1, twist :: done, sum + twist)

    def secureAtDistance(random: SecureRandom, distance: Int32): MoveSequence \ IO =
        let table = CubeSolvers.Pocket.Search.table(distance);
        let states = statesAt(table, distance);
        let state = stateAt(states, random.nextInt(List.length(states)));
        match CubeSolvers.Pocket.Search.descend(table, state) {
            case Some(home) => CubeSolvers.inverse(home)
            case None       => Nil
        }

    def secureAtLeastFour(engine: Engine, random: SecureRandom): MoveSequence \ IO =
        let state = secureSample(random);
        match search(engine, CubeSolvers.Pocket.Stickers.toFacelets(state), defaultOptions()) {
            case Outcome.Solved(home) if List.length(home) >= 4 => CubeSolvers.inverse(home)
            case _ => secureAtLeastFour(engine, random)
        }

}
