package aoc2023.d24

import java.io.FileWriter
import java.io.File

def solution(filename: String, limitMin: Float, limitMax: Float): Int =
    val hailstones = parseFile(filename)
        .toList

    hailstones
        .combinations(2)
        .map(pair => {
            val (a, b) = (pair(0), pair(1))
            val (x, y) = a.intersectsWithoutTime(b)
            val (ta, tb) = (a.timePart1(x, y), b.timePart1(x, y))
            (x, y, ta, tb)
        })
        .filter((x, y, ta, tb) => ta >= 0 &&  tb >= 0)
        .map((x, y, ta, tb) => (x, y))
        .map(pair => {
            val (x, y) = pair
            if x >= limitMin && x <= limitMax && y >= limitMin && y <= limitMax then
                1
            else
                0
        })
        .sum

def solution2(filename: String, output: String): Int =
    val hailstones = parseFile(filename)
        // .drop(3)
        .take(5)
        .toList

    val fileWriter = new FileWriter(new File(output))
    fileWriter.write(
        List(
            "import z3\n\n",
            "x = z3.BitVec('x', 64)\n",
            "y = z3.BitVec('y', 64)\n",
            "z = z3.BitVec('z', 64)\n",
            "dx = z3.BitVec('dx', 64)\n",
            "dy = z3.BitVec('dy', 64)\n",
            "dz = z3.BitVec('dz', 64)\n\n",
        )
        .mkString("")
    )
    
    hailstones
        .map(h => s"t${h.id} = z3.BitVec('t${h.id}', 64)")
        .foreach(s => fileWriter.write(s + "\n"))

    fileWriter.write("\ns = z3.Solver()\n")

    hailstones
        .map(h => s"s.add(t${h.id} >= 0),")
        .foreach(s => fileWriter.write(s + "\n"))

    val s = hailstones
        .map(h =>
            List(
                s"s.add(${h.v.x.toLong} * t${h.id} + ${h.c0.x.toLong} == x + dx * t${h.id})",
                s"s.add(${h.v.y.toLong} * t${h.id} + ${h.c0.y.toLong} == y + dy * t${h.id})",
                s"s.add(${h.v.z.toLong} * t${h.id} + ${h.c0.z.toLong} == z + dz * t${h.id})",
            )
        )
        .flatten
        .foreach(s => fileWriter.write(s + "\n"))
   
    fileWriter.write("\nassert s.check() == z3.sat\n")

    fileWriter.write("\nm = s.model()\n")
    fileWriter.write("\nx, y, z = m.eval(x), m.eval(y), m.eval(z)\n")
    fileWriter.write("\nx, y, z = x.as_long(), y.as_long(), z.as_long()\n")

    fileWriter.write("\nprint(x, y, z)\n")

    fileWriter.write("\nans = x + y + z\n")
    fileWriter.write("\nprint(ans)\n") 

    
    fileWriter.close()

    0

class Coordinate(val x: String, val y: String, val z: String) {
    override def toString(): String = 
        s"($x, $y, $z)"
}

class Velocity(val x: String, val y: String, val z: String) {
    override def toString(): String = 
        s"($x, $y, $z)"
}

class Hailstone(val id: Int, val c0: Coordinate, val v: Velocity) {
    val name = (id + 'a'.toInt).toChar.toString

    val k = v.y.toFloat / v.x.toFloat
    val b = c0.y.toFloat - k * c0.x.toFloat

    def intersectsWithoutTime(that: Hailstone): (Float, Float) = 
        val x = (that.b - this.b) / (this.k - that.k)
        val y = this.k * x + this.b
        (x, y)

    def timePart1(c: (Float, Float)): Float =
        val t = (c._1.toFloat - c0.x.toFloat) / v.x.toFloat
        t

    def formulaPart1(): String = 
        s"y(x) = $k * x + $b"

    override def toString(): String = 
        s"$name: $c0, $v"

    def intersects(that: Hailstone): (Float, Float, Float, Boolean) =
        val (a, b) = (this, that)
        val tx = (b.c0.x.toFloat - a.c0.x.toFloat) / (a.v.x.toFloat - b.v.x.toFloat)
        val ty = (b.c0.y.toFloat - a.c0.y.toFloat) / (a.v.y.toFloat - b.v.y.toFloat)
        val tz = (b.c0.z.toFloat - a.c0.z.toFloat) / (a.v.z.toFloat - b.v.z.toFloat)
        (tx, ty, tz, tx >= 0 && tx == ty && ty == tz)
}

def parseFile(filename: String): Iterator[Hailstone] =
    io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .map(zline =>
            val (line, idx) = zline
            val s"$left @ $right" = line: @unchecked
            val s"$x,$y,$z" = left: @unchecked
            val s"$dx,$dy,$dz" = right: @unchecked
            val c = Coordinate(x.trim(), y.trim(), z.trim())
            val v = Velocity(dx.trim(), dy.trim(), dz.trim())
            Hailstone(idx, c, v)
        )