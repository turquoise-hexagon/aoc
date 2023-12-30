int: N;
array [1..N, 1..5] of int: INGREDIENTS;

var int: score;
array [1..N] of var 0..100: recipe;

constraint sum(recipe) == 100;

constraint
    score =
        product(i in 1..4) (
            max(
                sum(j in 1..N) (
                    recipe[j] * INGREDIENTS[j, i]
                ),
                0
            )
        );

solve maximize score;

output [show(score)]
