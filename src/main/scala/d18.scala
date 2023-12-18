package aoc2023.d18

def solution(filename: String, format: Int): Long =
    var coords = if format == 1 then parseLines(filename) else parseLines2(filename)

    val start = coords.head
    val (p, _) = coords.foldLeft((0L, start))((acc, c) => 
        val (ans, prev) = acc
        val l = math.abs(c.x - prev.x) + math.abs(c.y - prev.y)
        (ans + l, c)
    )

    coords = coords
    val n = coords.size
        
    val (xl, yl) = coords
        .toArray
        .foldLeft((List[Long](), List[Long]()))((acc, c) => 
            val (xs, ys) = acc
            (xs :+ c.x, ys :+ c.y)
        )
    
    val (xx, yy) = (xl.toArray, yl.toArray)
    
    var ans = xx(n - 1) * yy(0) - xx(0) * yy(n - 1)
    (0 to n - 2)
        .foreach(i => ans = ans + xx(i) * yy(i + 1))
    (0 to n - 2)
        .foreach(i => ans = ans - xx(i + 1) * yy(i))
    ans = math.abs(ans) / 2

    ans = ans + p/2 + 1

    ans


def parseLines(filename: String): List[Coordinate] =
    val start = Coordinate(0, 0)
    val (coords, _) = io.Source.fromFile(filename)
    .getLines()
    .map(line => {
        val s"${dir} ${steps} ${etc}" = line: @unchecked
        (Directions.withName(dir), steps.toInt)
    })
    .foldLeft(List[Coordinate](start), start)((acc, x) => 
        val (coords, c) = acc
        val (dir, steps) = x
        val newC = dir match {
            case Directions.U => Coordinate(c.x, c.y - steps)
            case Directions.D => Coordinate(c.x, c.y + steps)
            case Directions.L => Coordinate(c.x - steps, c.y)
            case Directions.R => Coordinate(c.x + steps, c.y)
        }
        (coords :+ newC, newC)
    )

    coords

def parseLines2(filename: String): List[Coordinate] =
    val start = Coordinate(0, 0)
    val (coords, _) = io.Source.fromFile(filename)
    .getLines()
    .map(x => {
        val s"(#${v})" = x.split(" ").last: @unchecked
        v
    })
    .map(x => 
        val (steps, d) = (x.take(5), x.drop(5))
        val stepsLong = java.lang.Long.parseLong(steps, 16)
        val dir = d.toInt match {
            case 0 => Directions.R
            case 1 => Directions.D
            case 2 => Directions.L
            case 3 => Directions.U
        }
        (dir, stepsLong)
    )
    .foldLeft(List[Coordinate](start), start)((acc, x) => 
        val (coords, c) = acc
        val (dir, steps) = x
        val newC = dir match {
            case Directions.U => Coordinate(c.x, c.y - steps)
            case Directions.D => Coordinate(c.x, c.y + steps)
            case Directions.L => Coordinate(c.x - steps, c.y)
            case Directions.R => Coordinate(c.x + steps, c.y)
        }
        (coords :+ newC, newC)
    )

    coords

object Directions extends Enumeration {
  type Direction = Value

  val U, D, L, R = Value
}

class Coordinate(_x: Long, _y: Long) {
    var x = _x
    var y = _y

    override def toString(): String = {
        s"($x,$y)"
    }

    override def equals(x: Any): Boolean = {
        x match {
            case that: Coordinate => that.x == this.x && that.y == this.y
            case _ => false
        }
    }

    override def hashCode(): Int = {
        s"$x,$y".hashCode()
    }
}