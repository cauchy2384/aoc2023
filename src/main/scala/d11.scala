package aoc2023.d11

def solution(filename: String, weight: Long): Long =
    val mx = Matrix(weight)
    
    io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .foreach((line, y) => {
            line
                .zipWithIndex
                .foreach((c, x) => {
                    mx.set(Coordinate(x, y), c)
                })
        })

    mx.analyze()
    
    val galaxies = mx.galaxies()

    galaxies
        .map(galaxy => {
            galaxies
                .filter(_ != galaxy)
                .map(g => mx.distance(galaxy, g))
        })
        .flatten
        .foldLeft(0L)(_ + _)
        ./(2)


class Matrix(_w: Long) {
    var mx = Map[Coordinate, Char]()
    var xs = List[Long]()
    var ys = List[Long]()
    val weight = _w

    def get(c: Coordinate): Char = mx(c)

    def set(c: Coordinate, v: Char): Unit =  mx = mx + (c -> v)

    def xmax() = mx.keys.map(_.x).max

    def ymax() = mx.keys.map(_.y).max

    def analyze() = {
        _analyze(true)
        _analyze(false)
    }

    def _analyze(horizontal: Boolean) = {
        val x = xmax()
        val y = ymax()

        (0L to y).foreach(y => {
            val spaces = (0L to x)
                .foldLeft(0)((acc, x) => {
                    val c = if horizontal then get(Coordinate(x, y)) else get(Coordinate(y, x))
                    if c == '.' then
                        acc + 1 
                    else 
                        acc
                    })
            val shouldExpand = spaces == x + 1
            if shouldExpand then
                if !horizontal then
                    xs = xs :+ y
                else
                    ys = ys :+ y
        })
    }

    def galaxies(): List[Coordinate] = {
        (0L to ymax()).flatMap(y => {
            (0L to xmax()).map(x => {
                Coordinate(x, y)
            })
        }).filter(c => get(c) == '#').toList
    }

    def distance(c1: Coordinate, c2: Coordinate): Long = {
        val d = Math.abs(c1.x - c2.x) + Math.abs(c1.y - c2.y)
        val xmin = Math.min(c1.x, c2.x)
        val xmax = Math.max(c1.x, c2.x)
        val ymin = Math.min(c1.y, c2.y)
        val ymax = Math.max(c1.y, c2.y)
        val spaces = xs.filter(x => x > xmin && x < xmax).size + ys.filter(y => y > ymin && y < ymax).size

        d + spaces * (weight - 1)
    }

    override def toString(): String = {
        val x = xmax()
        val y = ymax()

        (0L to y).map(y => {
            (0L to x).map(x => {
                mx(Coordinate(x, y))
            }).mkString("")
        }).mkString("\n")
    }
}

class Coordinate(_x: Long, _y: Long) {
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