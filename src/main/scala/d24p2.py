import z3

x = z3.BitVec('x', 64)
y = z3.BitVec('y', 64)
z = z3.BitVec('z', 64)
dx = z3.BitVec('dx', 64)
dy = z3.BitVec('dy', 64)
dz = z3.BitVec('dz', 64)

t0 = z3.BitVec('t0', 64)
t1 = z3.BitVec('t1', 64)
t2 = z3.BitVec('t2', 64)
t3 = z3.BitVec('t3', 64)
t4 = z3.BitVec('t4', 64)

s = z3.Solver()
s.add(t0 >= 0),
s.add(t1 >= 0),
s.add(t2 >= 0),
s.add(t3 >= 0),
s.add(t4 >= 0),
s.add(227 * t0 + 144788461200241 == x + dx * t0)
s.add(158 * t0 + 195443318499267 == y + dy * t0)
s.add(5 * t0 + 285412990927879 == z + dz * t0)
s.add(37 * t1 + 266680201159206 == x + dx * t1)
s.add(-56 * t1 + 319693757705834 == y + dy * t1)
s.add(138 * t1 + 207679493757440 == z + dz * t1)
s.add(-88 * t2 + 343135145904814 == x + dx * t2)
s.add(41 * t2 + 302103279002870 == y + dy * t2)
s.add(9 * t2 + 240702357103107 == z + dz * t2)
s.add(-22 * t3 + 344900100024424 == x + dx * t3)
s.add(-140 * t3 + 366032694378845 == y + dy * t3)
s.add(7 * t3 + 216398516914389 == z + dz * t3)
s.add(-17 * t4 + 333882464390486 == x + dx * t4)
s.add(-40 * t4 + 338355498622498 == y + dy * t4)
s.add(-75 * t4 + 248150007635020 == z + dz * t4)

assert s.check() == z3.sat

m = s.model()

x, y, z = m.eval(x), m.eval(y), m.eval(z)

x, y, z = x.as_long(), y.as_long(), z.as_long()

print(x, y, z)

ans = x + y + z

print(ans)
