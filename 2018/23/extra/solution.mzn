int: N;
array[1..N, 1..4] of int: BOTS;

array[1..3] of var -15 * 10^8..15 * 10^8: bot;

solve maximize
    count(i in 1..N) (
        sum(j in 1..3) (
            abs(bot[j] - BOTS[i, j])
        ) <= BOTS[i, 4]
    );

output [show(sum(i in 1..3) (abs(bot[i])))]
