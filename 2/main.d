import std.stdio;
import std.math: abs;
import std.algorithm: canFind;

struct Pair(T) 
{ 
    T fst; 
    T snd; 
}

alias Pair!(int[]) Result;
alias Pair!(int)   Config;

pure Result balanceScale(const Config initial, const int[] weights)
{
    const ulong N = weights.length;

    /* Absolute value of the difference between the two intial pan weights. */
    const int d = abs(initial.fst - initial.snd);

    pure Result result(int[] a, int[] b) {
        return initial.fst > initial.snd ? Result(a, b) : Result(b, a);
    }

    /* Special case: Is the scale already balanced? */
    if (0 == d) {
        return Result([], []);
    }

    /* Let a and b denote the inital weights placed in the left and right pan.
     * Then consider the one weight case first: For all weights w, 
     * 
     * - If |a - b| = w then put w in the min(a, b) pan. 
     */
    foreach (i; 0..N)
    {
        const int w = weights[i];

        if (d == w) {
            return result([], [w]);
        }
    }

    /* Try two weights: Again, let a and b be the initial weights. Now take all 
     * pairs (w, v) of weights from the input array.
     * 
     * - If |a - b| = |w - v|, then put w and v in separate pans.
     * - If |a - b| = w + v, put both w and v in the same pan.
     */
    foreach (i; 0..N)
    {
        const int w = weights[i];

        foreach (j; (i + 1)..N)
        {
            const int v = weights[j];

            if (d == abs(w - v)) {
                return result([w], [v]);
            } else if (d == w + v) {
                return result([], [w, v]);
            }
        }
    }

    /* Cannot balance the scale. */
    return Result([-1], [-1]);
}

/* Run with e.g., rdmd -unittest main.d */
unittest
{
    struct Test
    {
        const int[]    weights;
        const Config   config;
        const Result[] results;
    }

    foreach (Test test; [
        Test([1, 2, 6, 7], Config(5, 9), [Result([6], [2])]),
        Test([1, 2, 6, 7], Config(9, 5), [Result([2], [6])]),
        Test([1, 2, 4, 7], Config(5, 9), [Result([4], [])]),
        Test([1, 2, 4, 7], Config(9, 5), [Result([], [4])]),
        Test([7, 4, 2, 1], Config(9, 5), [Result([], [4])]),
        Test([4, 7, 1, 2], Config(9, 5), [Result([], [4])]),
        Test([1, 2, 4, 9], Config(11, 5), [Result([], [4, 2]), Result([], [2, 4])]),
        Test([1, 2, 4, 9], Config(3, 3), [Result([], [])]),
        Test([4, 6, 7, 8], Config(3, 8), [Result([-1], [-1])])
    ]) {
        auto actualResult = balanceScale(test.config, test.weights);
        writeln(test);
        assert(test.results.canFind(actualResult));
    }
    
    writeln("All tests okay!");
}

void main()
{
}
