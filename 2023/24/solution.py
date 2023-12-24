#!/usr/bin/env python3

import re
import sys
import z3

s = z3.Solver()

x, y, z, dx, dy, dz = map(z3.Real, ['x', 'y', 'z', 'dx', 'dy', 'dz'])

for i, l in enumerate(sys.stdin.readlines()):
    a, b, c, da, db, dc = map(int, re.findall('-?\d+', l))

    t = z3.Real(f't{i}')

    s.add(t >= 0)
    s.add(a + da * t == x + dx * t)
    s.add(b + db * t == y + dy * t)
    s.add(c + dc * t == z + dz * t)

assert str(s.check() == 'sat')

print(s.model().eval(x + y + z))
