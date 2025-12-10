/* data */
int: NB_PBs;
int: NB_Js;
int: NB_Bs;

array [1..NB_PBs * NB_Js * NB_Bs] of int: _Bs;
array [1..NB_PBs * NB_Js] of int: _Js;

/* internalize data */
array [1..NB_PBs, 1..NB_Bs, 1..NB_Js] of int: Bs =
    array3d(1..NB_PBs, 1..NB_Bs, 1..NB_Js, _Bs);

array [1..NB_PBs, 1..NB_Js] of int: Js =
    array2d(1..NB_PBs, 1..NB_Js, _Js);

/* problem */
var int: result;

array [1..NB_PBs, 1..NB_Bs] of var int: presses;

constraint
    forall(i in presses) (
        i >= 0
    );

constraint
    forall(i in 1..NB_PBs) (
        forall(j in 1..NB_Js) (
            Js[i, j] ==
                sum(k in 1..NB_Bs) (
                    Bs[i, k, j] * presses[i, k]
                )
            )
        );

constraint
    result = sum(presses);

solve minimize result;

output[show(result)]
