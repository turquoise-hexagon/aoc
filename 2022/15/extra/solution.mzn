int: N;
array [1..N, 1..4] of int: INPUT;

function var bool: is_in_range(var int: x, var int: y, var int: i) = (
    (abs(          x - INPUT[i, 1]) + abs(          y - INPUT[i, 2])) <=
    (abs(INPUT[i, 1] - INPUT[i, 3]) + abs(INPUT[i, 2] - INPUT[i, 4]))
);

var int: x;
var int: y;

constraint x >= 0 /\ x <= 4000000;
constraint y >= 0 /\ y <= 4000000;

constraint
    forall(i in 1..N) (
        not(is_in_range(x, y, i))
    );

output [show(x * 4000000 + y)]
