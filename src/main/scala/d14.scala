package aoc2023.d14

def solution(filename: String): Long =
    var mx = io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .foldLeft(Matrix())((mat, yline) => {
            var (line, y) = yline
            line.zipWithIndex.foreach((c, x) => {
                mat.add(Coordinate(x, y), c)
            })
            mat
        })

    mx.tilt(Directions.N)
    mx.load()

def solution2(filename: String, maxIts: Int): Long =
    var mx = io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .foldLeft(Matrix())((mat, yline) => {
            var (line, y) = yline
            line.zipWithIndex.foreach((c, x) => {
                mat.add(Coordinate(x, y), c)
            })
            mat
        })

    val cycles = cycle(mx, Map[String, Int](), 0, maxIts)
    val (delta, its) = cycles
    val remaining = (1000000000 - its) % delta
    (1 to remaining)
        .foreach(_ => mx.cycle())

    mx.load()

def cycle(mx: Matrix, cache: Map[String, Int], its: Int, maxIts: Int): (Int, Int) = {
    if its == maxIts then
        (0, 0)
    else
        val h = mx.hash()
        if cache.contains(h) then
            val delta = its -  cache(h)
            (delta, cache(h))
        else
            val newCache = cache + (mx.hash() -> its)
            mx.cycle()
            cycle(mx, newCache, its + 1, maxIts)
}

def hasLoop(m: Map[Long, List[Int]], w: Long): Option[Int] = {
    if m.contains(w) then
        val l = m(w)
        if l.length < 3 then
            None
        else if l(0) - l(1) == l(1) - l(2) then
            Some(l(0) - l(1))
        else
            None
    else
        None
}

object Directions extends Enumeration {
  type Direction = Value

  val N, E, S, W = Value
}

class Matrix {
    var mx = Map[Coordinate, Char]()

    lazy val xmax: Int = mx.keys.map(_.x).max
    lazy val ymax: Int = mx.keys.map(_.y).max

    def add(c: Coordinate, v: Char) = mx = mx + (c -> v)

    def cycle() = {
            ("NWSE")
                .map(c => Directions.withName(c.toString))
                .foreach(tilt(_))
    }

    def tilt(d: Directions.Value) = {
        val ys = d match {
            case Directions.N => (0 to ymax)
            case Directions.S => (0 to ymax).reverse
            case Directions.E => (0 to xmax).reverse
            case Directions.W => (0 to xmax)
        }
        val xs = d match {
            case Directions.N => (0 to xmax)
            case Directions.S => (0 to xmax)
            case Directions.E => (0 to ymax)
            case Directions.W => (0 to ymax)
        }
        
        ys.foreach(y => {
            xs.foreach(x => {
                d match
                    case Directions.N => roll(Coordinate(x, y), d, 0)
                    case Directions.S => roll(Coordinate(x, y), d, 0)
                    case Directions.E => roll(Coordinate(y, x), d, 0)
                    case Directions.W => roll(Coordinate(y, x), d, 0)
            })
        })
    }

    def roll(c: Coordinate, d: Directions.Value, rolls: Int): Int = {
        val x = c.x
        val y = c.y
        if mx.getOrElse(c, '?') == 'O' then
            val c2 = d match {
                case Directions.N => Coordinate(x, y - 1)
                case Directions.S => Coordinate(x, y + 1)
                case Directions.E => Coordinate(x + 1, y)
                case Directions.W => Coordinate(x - 1, y)
            }
            if (isFree(c2)) then
                mx = mx + (c2 -> mx(c))
                mx = mx + (c -> '.')
                roll(c2, d, rolls + 1)
            else
                rolls
        else
            rolls
    }

    def isFree(c: Coordinate): Boolean = {
        mx.getOrElse(c, '?') == '.'
    }

    def load(): Long = {
        (0 to ymax)
            .foldLeft(0L)((acc, y) => {
                (0 to xmax)
                    .foldLeft(acc)((acc, x) => {
                        acc + rockLoad(Coordinate(x, y))
                    })
            })
    }

    def rockLoad(c: Coordinate): Long = {
        if mx.getOrElse(c, '?') == 'O' then
            val rows: Long = ymax - c.y + 1
            rows
        else
            0L 
    }

    override def toString(): String = 
        (0 to ymax)
            .map(y => (0 to xmax)
                .map(x => mx(Coordinate(x, y)))
                .mkString("")
            )
            .mkString("\n")

    def hash(): String = {
        toString()
    }
}

class Coordinate(_x: Int, _y: Int) {
    var x = _x
    var y = _y

    override def toString(): String = {
        s"($x,$y)"
    }

    override def equals(x: Any): Boolean = {
        x match {
            case c: Coordinate => c.x == this.x && c.y == this.y
            case _ => false
        }
    }

    override def hashCode(): Int = {
        x.hashCode() + y.hashCode()
    }
}