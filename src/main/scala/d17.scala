package aoc2023.d17

import scala.annotation.tailrec
import scala.math.Ordering
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Map
import scala.collection.mutable.Set

def solution(filename: String, minSteps: Int, maxSteps: Int): Int =
    val mx = parseFile(filename)

    mx.add(Coordinate(0, 0), 0)

    val start = Coordinate(0, 0)
    val end = Coordinate(mx.xmax, mx.ymax)

    var dist = Map[Key, Int]()
    var visited = Set[Key]()

    @tailrec 
    def dijkstra(q: PriorityQueue[State]): Int =
        if q.isEmpty then
            0
        else
            val state = q.dequeue()
            val key = Key(state.pos, state.dir, state.stepsInLine)

            val best = if dist.contains(key) then 
                dist(key) 
            else if key.pos == start then
                0
            else 
                Int.MaxValue
            
            if visited.contains(key) then
                dijkstra(q)
            else if state.heat > best then
                dijkstra(q)
            else
                visited.add(key)
                dist.addOne(key, state.heat)
                
                q.addAll(nextStates(state, mx, minSteps, maxSteps))
                dijkstra(q)

    var q = PriorityQueue[State](
        State(Coordinate(0, 0), 0, Directions.R, 0, Coordinate(-1, -1)),
        State(Coordinate(0, 0), 0, Directions.D, 0, Coordinate(-1, -1)),
    )(Ordering.by((_: State).heat).reverse)

    dijkstra(q)

    val ans = dist.filter(z => {
            val (c, d) = z
            c.pos == end && c.stepsInLine >= minSteps
        })
        .map(z => {
            val (c, d) = z
            d
        })
        .min

    ans

def nextStates(state: State, mx: Matrix, minSteps: Int, maxSteps: Int): List[State] =
    val nextDir = state.dir match {
        case Directions.U => List(Directions.L, Directions.U, Directions.R)
        case Directions.D => List(Directions.R, Directions.D, Directions.L)
        case Directions.L => List(Directions.D, Directions.L, Directions.U)
        case Directions.R => List(Directions.U, Directions.R, Directions.D)
    }
    
    nextDir
        .map(dir => {
            val nextPos = dir match {
                case Directions.U => Coordinate(state.pos.x, state.pos.y - 1)
                case Directions.D => Coordinate(state.pos.x, state.pos.y + 1)
                case Directions.L => Coordinate(state.pos.x - 1, state.pos.y)
                case Directions.R => Coordinate(state.pos.x + 1, state.pos.y)
            }
            val nextStepsInLine = if state.dir == dir then state.stepsInLine + 1 else 1
            State(nextPos, state.heat, dir, nextStepsInLine, state.pos)
        })
        .filter(nextState => !mx.isOutOfBounds(nextState.pos))
        .filter(nextState => {
            if state.stepsInLine < minSteps then
                state.dir == nextState.dir
            else
                true
        })
        .filter(nextState => { 
            if state.stepsInLine == maxSteps then
                state.dir != nextState.dir
            else
                true
        })
        .map(s => {
            val nextHeat = s.heat + mx.at(s.pos)
            State(s.pos, nextHeat, s.dir, s.stepsInLine, s.prev)
        }) 


class Key(_pos: Coordinate, _dir: Directions.Direction, _stepsInLine: Int) {
    var pos = _pos
    var dir = _dir
    var stepsInLine = _stepsInLine

    override def toString(): String = 
        s"Key($pos, $dir, $stepsInLine)"

    override def equals(x: Any): Boolean =
        x match {
            case that: Key => that.pos == this.pos && that.dir == this.dir && that.stepsInLine == this.stepsInLine
            case _ => false
        }
    
    override def hashCode(): Int =
        s"$pos,$dir,$stepsInLine".hashCode()
}


class State(_pos: Coordinate, _heat: Int, _dir: Directions.Direction, _stepsInLine: Int, _prev: Coordinate) {
    var pos = _pos
    var heat = _heat
    var dir = _dir
    var stepsInLine = _stepsInLine
    var prev = _prev

    override def toString(): String = 
        s"State($pos, $heat, $dir, $stepsInLine, $prev)"
}

class Matrix {
    var mx = Map[Coordinate, Int]()

    lazy val xmax: Int = mx.keys.map(_.x).max

    lazy val ymax: Int = mx.keys.map(_.y).max

    def add(c: Coordinate, v: Int) = mx = mx + (c -> v)

    def at(c: Coordinate): Int = mx(c)

    def isOutOfBounds(c: Coordinate): Boolean = 
        c.x < 0 || c.y < 0 || c.x > xmax || c.y > ymax

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
                    mx.add(Coordinate(x, y), c.asDigit)
                })
            })
    mx

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