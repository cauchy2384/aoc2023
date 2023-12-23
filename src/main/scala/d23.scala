package aoc2023.d23

import scala.annotation.tailrec
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue

def solution(filename: String): Long =
    val mx = parseFile(filename)

    val start = mx.start()
    val end = mx.end()

    var ans = 0L
    var path = Map[Coordinate, Long]()
    var bests = Map[Coordinate, Long]()

    @tailrec
    def dfs(stack: Stack[(Coordinate, Long, Map[Coordinate, Long])]): Long =
        if stack.isEmpty then
            0L
        else
            val (c, steps, visited) = stack.pop()

            if mx.isOutOfBounds(c) then
                dfs(stack)
            else if mx.at(c) == '#' then
                dfs(stack)
            else if visited.contains(c) then
                dfs(stack)
            else if c == end then
                if steps > ans then
                    ans = steps
                    path = visited
                dfs(stack)
            else
                val ok = 
                    bests.get(c) match
                        case Some(v) => v < steps
                        case None => true
                if !ok then
                    dfs(stack)
                else
                    bests = bests + (c -> steps)
                    val steps2 = steps + 1
                    val visited2 = visited + (c -> steps)
                    val v = mx.at(c)
                    v match
                        case '.' => 
                            stack.pushAll(
                                Directions
                                    .values
                                    .toList
                                    .map(dir => (c.move(dir), steps2, visited2))
                            )
                        case '>' =>
                            stack.push((c.move(Directions.R), steps2, visited2)) 
                        case '<' =>
                            stack.push((c.move(Directions.L), steps2, visited2))
                        case '^' =>
                            stack.push((c.move(Directions.U), steps2, visited2))
                        case 'v' =>
                            stack.push((c.move(Directions.D), steps2, visited2))
                        case _ => throw new Exception(s"unknown char: $v")
                    dfs(stack)

    dfs(Stack((start, 0L, Map[Coordinate, Long]())))

    ans

def solution2(filename: String): Long =
    val mx = parseFile(filename)
    mx.fixSlopes()

    val start = mx.start()
    val end = mx.end()
    val graph = mx.graph()

    var g2 = Map[Coordinate, List[(Coordinate, Long)]]()

    def path(c: Coordinate, steps: Long, visited: Set[Coordinate]): (Long, Coordinate) = 
        val edges = graph(c)
            .filter(e => !visited.contains(e))
        if edges.length == 1 then
            path(edges.head, steps + 1, visited + c)
        else
            (steps, c)

    graph.foreach(ve =>
        val (v, edges) = ve
        if v == start || edges.length > 1 then
            edges.foreach(e => 
                val (steps, dest) = path(e, 1L, Set(v))
                g2 = g2 + (v -> ((dest, steps) :: g2.getOrElse(v, List())))
            )
    )

    var ans = 0L

    def dfs(c: Coordinate, steps: Long, visited: Set[Coordinate]): (Long, Boolean) =
        if c == end then
            ans = ans.max(steps)
            (steps, true)
        else
            val edges = g2(c)
                .filter(e => !visited.contains(e._1))
            if edges.isEmpty then
                (steps, false)
            else
                val ans = edges
                    .map(e => dfs(e._1, steps + e._2, visited + c))
                    .filter(_._2)
                if ans.isEmpty then
                    (steps, false)
                else
                    ans.maxBy(_._1)

    dfs(start, 0L, Set[Coordinate]())
    
    ans

class Matrix {
    var mx = Map[Coordinate, Char]()

    lazy val xmax: Int = mx.keys.map(_.x).max

    lazy val ymax: Int = mx.keys.map(_.y).max

    def add(c: Coordinate, v: Char) = mx = mx + (c -> v)

    def at(c: Coordinate): Char = mx(c)

    def start(): Coordinate = 
        mx.filter(_._1.y == 0)
            .filter(_._2 == '.')
            .keys
            .head

    def end(): Coordinate =
        mx.filter(_._1.y == ymax)
            .filter(_._2 == '.')
            .keys
            .head

    def isOutOfBounds(c: Coordinate): Boolean = 
        c.x < 0 || c.y < 0 || c.x > xmax || c.y > ymax

    def fixSlopes() = 
        val slopes = mx
            .filter(_._2 != '.')
            .filter(_._2 != '#')
            .foreach(slope => {
                add(slope._1, '.')
            })

    def graph(): Map[Coordinate, List[Coordinate]] =
        mx
            .filter(_._2 == '.')
            .map(c => {
                val (coord, _) = c
                val adj = Directions
                    .values
                    .toList
                    .map(dir => coord.move(dir))
                    .filter(c => mx.contains(c) && mx(c) == '.')
                (coord, adj)
            })

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
                    mx.add(Coordinate(x, y), c)
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