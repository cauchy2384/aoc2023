package aoc2023.d21

import scala.annotation.tailrec
import scala.collection.mutable.Queue
import java.util.ArrayList

def solution(filename: String, steps: Long): Long =
    val mx = parseFile(filename)

    @tailrec
    def bfs(q: Queue[Coordinate], it: Long, maxSteps: Long): Queue[Coordinate] =
        if it == maxSteps then
            q
        else
            var q2 = Queue[Coordinate]()
            var s = Set[Coordinate]()
            q
                .foreach(c => {
                    Directions.values
                        .foreach(dir => {
                            val nc = c.move(dir)
                            if !s.contains(nc) then
                                s = s + nc
                                if !mx.isOutOfBounds(nc) && !mx.isStone(nc) then
                                    q2.enqueue(nc)
                        })
                })
            bfs(q2, it + 1, maxSteps)

    val q = bfs(Queue(mx.start), 0, steps)

    q.toSet.size

def solution2(filename: String, steps: Long): Long =
    val mx = parseFile(filename)

    @tailrec
    def bfs(q: Queue[Coordinate], it: Long, maxSteps: Long): Long =
        if it == maxSteps then
            q.size
        else
            val next = q.map(c => 
                    Directions.values.toList
                        .map(dir => c.move(dir))
                        .filter(c => !mx.isStone(c))
            )
            .flatten
            .toSet

            val q2 = Queue[Coordinate]()
            q2.enqueueAll(next)

            bfs(q2, it + 1, maxSteps)

    val sz = mx.xmax + 1

    val xs = (0 to 2)
        .map(i => sz/2 + i * sz)
        .map(_.toLong)
        .toArray
    
    val ys = xs
        .map(i => bfs(Queue(mx.start), 0, i))
        .map(_.toLong)
        .toArray

    lagrange(steps, xs, ys)

// Lagrange's Interpolation formula for ax^2 + bx + c
def lagrange(x: Long, xs: Array[Long],  ys: Array[Long]): Long =
    val (x0, x1, x2) = (xs(0), xs(1), xs(2))
    val (y0, y1, y2) = (ys(0), ys(1), ys(2))

    val a = (x - x1) * (x - x2) / ((x0 - x1) * (x0 - x2))
    val b = (x - x0) * (x - x2) / ((x1 - x0) * (x1 - x2))
    val c = (x - x0) * (x - x1) / ((x2 - x0) * (x2 - x1))

    a * y0 + b * y1 + c * y2

class Matrix {
    var mx = Map[Coordinate, Char]()
    var start = Coordinate(0, 0)

    lazy val xmax: Int = mx.keys.map(_.x).max

    lazy val ymax: Int = mx.keys.map(_.y).max

    def add(c: Coordinate, v: Char) = mx = mx + (c -> v)

    def at(c: Coordinate): Char = mx(c)

    def atDefault(c: Coordinate): Char = 
        if mx.contains(c) then
            mx(c)
        else
            at(normalize(c))

    def setStart(c: Coordinate) = start = c

    def isOutOfBounds(c: Coordinate): Boolean = 
        c.x < 0 || c.y < 0 || c.x > xmax || c.y > ymax
    
    @tailrec
    final def normalize(c: Coordinate): Coordinate =
        if c.x < 0 then
            normalize(Coordinate(c.x + xmax + 1, c.y))
        else if c.y < 0 then
            normalize(Coordinate(c.x, c.y + ymax + 1))
        else if c.x > xmax then
            normalize(Coordinate(c.x - xmax - 1, c.y))
        else if c.y > ymax then
            normalize(Coordinate(c.x, c.y - ymax - 1))
        else
            c

    def isStone(c: Coordinate): Boolean = 
        val c2 = normalize(c)
        at(c2) == '#'

    def deepCopy(): Matrix =
        val mx = Matrix()
        mx.start = this.start
        this.mx.foreach(x => {
            val (c, v) = x
            mx.add(c, v)
        })
        mx

    override def toString(): String = 
        (0 to ymax)
            .map(y => (0 to xmax)
                .map(x => mx(Coordinate(x, y)))
                .mkString("")
            )
            .mkString("\n")
}

def parseFile(filename: String): Matrix =
    val mx = Matrix()
    io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .foreach(zline => {
            val (line, y) = zline
            line.zipWithIndex
                .foreach(zchar => {
                    val (c, x) = zchar
                    c match {
                        case 'S' => 
                            mx.add(Coordinate(x, y), c)
                            mx.setStart(Coordinate(x, y))
                        case _ =>
                             mx.add(Coordinate(x, y), c)
                    }
                })
            })
    mx

object Directions extends Enumeration {
  type Direction = Value

  val U, D, L, R = Value
}

class Coordinate(var x: Int, var y: Int) {
    def move(dir: Directions.Value): Coordinate = {
        dir match {
            case Directions.U => Coordinate(x, y - 1)
            case Directions.D => Coordinate(x, y + 1)
            case Directions.L => Coordinate(x - 1, y)
            case Directions.R => Coordinate(x + 1, y)
        }
    }

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