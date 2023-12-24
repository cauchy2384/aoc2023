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
s.add(-2 * t0 + 19 == x + dx * t0)
s.add(1 * t0 + 13 == y + dy * t0)
s.add(-2 * t0 + 30 == z + dz * t0)
s.add(-1 * t1 + 18 == x + dx * t1)
s.add(-1 * t1 + 19 == y + dy * t1)
s.add(-2 * t1 + 22 == z + dz * t1)
s.add(-2 * t2 + 20 == x + dx * t2)
s.add(-2 * t2 + 25 == y + dy * t2)
s.add(-4 * t2 + 34 == z + dz * t2)
s.add(-1 * t3 + 12 == x + dx * t3)
s.add(-2 * t3 + 31 == y + dy * t3)
s.add(-1 * t3 + 28 == z + dz * t3)
s.add(1 * t4 + 20 == x + dx * t4)
s.add(-5 * t4 + 19 == y + dy * t4)
s.add(-3 * t4 + 15 == z + dz * t4)

assert s.check() == z3.sat

m = s.model()

x, y, z = m.eval(x), m.eval(y), m.eval(z)

x, y, z = x.as_long(), y.as_long(), z.as_long()

print(x, y, z)

ans = x + y + z

print(ans)
