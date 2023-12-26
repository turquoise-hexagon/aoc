int: N;

array [1..N, 1..3] of int: COORDS;
array [1..N, 1..3] of int: VELOCITIES;

array [1..N] of var int: times;
array [1..3] of var int: coords;
array [1..3] of var -300..300: velocities;

constraint
    forall(i in 1..N) (
        times[i] >= 0
    );

constraint
    forall(i in 1..N, j in 1..3) (
        COORDS[i, j] + VELOCITIES[i, j] * times[i] == coords[j] + velocities[j] * times[i]
    );

output [show(sum(coords))]
