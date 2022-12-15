int: length;
array [1..length, 1..4] of int: input;

function var bool: is_in_range(var int: x, var int: y, var int: i) = (
    (abs(          x - input[i, 1]) + abs(          y - input[i, 2])) <=
    (abs(input[i, 1] - input[i, 3]) + abs(input[i, 2] - input[i, 4]))
);

var int: x;
var int: y;

constraint x >= 0 /\ x <= 4000000;
constraint y >= 0 /\ y <= 4000000;

constraint forall(i in 1..length) (
    not(is_in_range(x, y, i))
);

output [show(x * 4000000 + y)]
