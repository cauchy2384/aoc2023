package aoc2023.d16

import scala.collection.mutable.Queue
import scala.collection.mutable.Set

def solution(filename: String): Int =
    var mx = Matrix()
    io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .foreach(zline => {
            val (line, y) = zline
            line.zipWithIndex
                .foreach(zchar => {
                    val (c, x) = zchar
                    mx.add(Coordinate(x, y), c)
                })
            })

    mx.enlight(Coordinate(0, 0), Directions.R)


def solution2(filename: String): Int =
    var mx = Matrix()
    io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .foreach(zline => {
            val (line, y) = zline
            line.zipWithIndex
                .foreach(zchar => {
                    val (c, x) = zchar
                    mx.add(Coordinate(x, y), c)
                })
            })

    val a = (0 to mx.xmax)
        .map(x => 
            List(
                mx.enlight(Coordinate(x, 0), Directions.D),
                mx.enlight(Coordinate(x, mx.ymax), Directions.U),
            )
        )
        .flatten
        .max

    val b = (0 to mx.ymax)
        .map(y => 
            List(
                mx.enlight(Coordinate(0, y), Directions.R),
                mx.enlight(Coordinate(mx.xmax, y), Directions.L),
            )
        )
        .flatten
        .max

    a.max(b)

class Matrix {
    var mx = Map[Coordinate, Char]()
    var visited = Set[(Coordinate, Directions.Value)]()

    lazy val xmax: Int = mx.keys.map(_.x).max

    lazy val ymax: Int = mx.keys.map(_.y).max

    def add(c: Coordinate, v: Char) = mx = mx + (c -> v)

    def isVisited(c: Coordinate, d: Directions.Value): Boolean = visited.contains((c, d))

    def visit(c: Coordinate, d: Directions.Value) = visited.add((c, d))

    def reset() = visited = Set[(Coordinate, Directions.Value)]()

    def enlight(c: Coordinate, d: Directions.Value): Int = 
        reset()
        light(c, d)
        visitedTotal()

    def light(c: Coordinate, d: Directions.Value): Boolean =
        if isOutOfBounds(c) || isVisited(c, d) then
            false
        else
            visit(c, d)
            val v = mx(c)
            v match {
                case '.' =>
                    d match
                        case Directions.U => light(Coordinate(c.x, c.y - 1), d)
                        case Directions.D => light(Coordinate(c.x, c.y + 1), d)
                        case Directions.L => light(Coordinate(c.x - 1, c.y), d)
                        case Directions.R => light(Coordinate(c.x + 1, c.y), d)
                case '\\' =>
                    d match {
                        case Directions.U => light(Coordinate(c.x - 1, c.y), Directions.L)
                        case Directions.D => light(Coordinate(c.x + 1, c.y), Directions.R)
                        case Directions.L => light(Coordinate(c.x, c.y - 1), Directions.U)
                        case Directions.R => light(Coordinate(c.x, c.y + 1), Directions.D)
                    }
                case '/' =>
                    d match {
                        case Directions.U => light(Coordinate(c.x + 1, c.y), Directions.R)
                        case Directions.D => light(Coordinate(c.x - 1, c.y), Directions.L)
                        case Directions.L => light(Coordinate(c.x, c.y + 1), Directions.D)
                        case Directions.R => light(Coordinate(c.x, c.y - 1), Directions.U)
                    }
                case '|' => 
                    d match {
                        case Directions.U => light(Coordinate(c.x, c.y - 1), d)
                        case Directions.D => light(Coordinate(c.x, c.y + 1), d)
                        case _ => 
                            light(Coordinate(c.x, c.y - 1), Directions.U)
                            light(Coordinate(c.x, c.y + 1), Directions.D)
                    }
                case '-' =>
                    d match {
                        case Directions.L => light(Coordinate(c.x - 1, c.y), d)
                        case Directions.R => light(Coordinate(c.x + 1, c.y), d)
                        case _ => 
                            light(Coordinate(c.x - 1, c.y), Directions.L)
                            light(Coordinate(c.x + 1, c.y), Directions.R)
                    }
            }

    def isOutOfBounds(c: Coordinate): Boolean = 
        c.x < 0 || c.y < 0 || c.x > xmax || c.y > ymax

    def visitedString(): String = 
        (0 to ymax)
            .map(y => (0 to xmax)
                .map(x => {
                    val c = Coordinate(x, y)
                    List((c, Directions.U), (c, Directions.D), (c, Directions.L), (c, Directions.R))
                        .map(cd => visited.contains(cd))
                        .filter(_ == true)
                        .length match {
                            case 0 => "."
                            case _ => "#"
                        }
                })
                .mkString("")
            )
            .mkString("\n")

    def visitedTotal(): Int =
        visited
            .map(_._1)
            .toSet
            .size

    override def toString(): String = 
        (0 to ymax)
            .map(y => (0 to xmax)
                .map(x => mx(Coordinate(x, y)))
                .mkString("")
            )
            .mkString("\n")
}

object Directions extends Enumeration {
  type Direction = Value

  val U, D, L, R = Value
}

class Coordinate(_x: Int, _y: Int) {
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