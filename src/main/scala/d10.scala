package aoc2023.d10

import scala.collection.mutable.Set
import scala.collection.mutable.Queue

def solution1(filename: String): Int =
    val maze = io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .foldLeft(Maze())(parseLine)

    bfs(maze, maze.start)

def solution2(filename: String): Int =
    var text = Map[Coordinate, Char]()
    io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .foreach(zline => {
            val (line, y) = zline
            line.zipWithIndex
                .foreach(zchar => {
                    val (c, x) = zchar
                    text = text + (Coordinate(x, y) -> c)
                })
            })

    var exp = Map[Coordinate, Char]()
    text.foreach(cchar => {
        var (c, ch) = cchar
        val x = c.x * 2
        val y = c.y
        exp = exp + (Coordinate(x, y) -> ch)
        if ch == 'S' then
            val top = text.get(Coordinate(c.x, c.y - 1)).getOrElse('.')
            val bot = text.get(Coordinate(c.x, c.y + 1)).getOrElse('.')
            val left = text.get(Coordinate(c.x - 1, c.y)).getOrElse('.')
            val right = text.get(Coordinate(c.x + 1, c.y)).getOrElse('.')
            val hasLeft = left == '-' || left == 'L' || left == 'F'
            val hasRight = right == '-' || right == 'J' || right == '7'
            val hasTop = top == '|' || top == '7' || top == 'F'
            val hasBot = bot == '|' || bot == 'L' || bot == 'J'
            if hasLeft && hasRight then
                ch = '-'
            else if hasLeft && hasTop then
                ch = 'J'
            else if hasLeft && hasBot then
                ch = '7'
            else if hasTop && hasRight then
                ch = 'L'
            else if hasBot && hasRight then
                ch = 'F'
            else if hasTop && hasBot then
                ch = '|'
            else
                ch = '.'
        ch match {
            case 'S' => exp = exp + (Coordinate(x + 1, y) -> 'S')
            case '|' => exp = exp + (Coordinate(x + 1, y) -> '.') 
            case '-' => exp = exp + (Coordinate(x + 1, y) -> '-')
            case 'L' => exp = exp + (Coordinate(x + 1, y) -> '-')
            case 'J' => exp = exp + (Coordinate(x + 1, y) -> '.')
            case '7' => exp = exp + (Coordinate(x + 1, y) -> '.')
            case 'F' => exp = exp + (Coordinate(x + 1, y) -> '-')
            case _ => exp = exp + (Coordinate(x + 1, y) -> '.')
        }
    })

    var exp2 = Map[Coordinate, Char]()
    exp.foreach(cchar => {
        var (c, ch) = cchar
        val x = c.x
        val y = c.y * 2
        exp2 = exp2 + (Coordinate(x, y) -> ch)
        if ch == 'S' then
            val top = exp.get(Coordinate(c.x, c.y - 1)).getOrElse('.')
            val bot = exp.get(Coordinate(c.x, c.y + 1)).getOrElse('.')
            val left = exp.get(Coordinate(c.x - 1, c.y)).getOrElse('.')
            val right = exp.get(Coordinate(c.x + 1, c.y)).getOrElse('.')
            val hasLeft = left == '-' || left == 'L' || left == 'F'
            val hasRight = right == '-' || right == 'J' || right == '7'
            val hasTop = top == '|' || top == '7' || top == 'F'
            val hasBot = bot == '|' || bot == 'L' || bot == 'J'
            if hasLeft && hasRight then
                ch = '-'
            else if hasLeft && hasTop then
                ch = 'J'
            else if hasLeft && hasBot then
                ch = '7'
            else if hasTop && hasRight then
                ch = 'L'
            else if hasBot && hasRight then
                ch = 'F'
            else if hasTop && hasBot then
                ch = '|'
            else
                ch = '.'
        ch match {
            case 'S' => exp2 = exp2 + (Coordinate(x, y + 1) -> 'S')
            case '|' => exp2 = exp2 + (Coordinate(x, y + 1) -> '|') 
            case '-' => exp2 = exp2 + (Coordinate(x, y + 1) -> '.')
            case 'L' => exp2 = exp2 + (Coordinate(x, y + 1) -> '.')
            case 'J' => exp2 = exp2 + (Coordinate(x, y + 1) -> '.')
            case '7' => exp2 = exp2 + (Coordinate(x, y + 1) -> '|')
            case 'F' => exp2 = exp2 + (Coordinate(x, y + 1) -> '|')
            case _ => exp2 = exp2 + (Coordinate(x, y + 1) -> '.')
        }
    })

    val xmax = exp2.keys.maxBy(_.x).x
    val ymax = exp2.keys.maxBy(_.y).y 

    // convert exp to maze
    val maze = Maze()

    (0 to ymax).foreach(y => {
        (0 to xmax).foreach(x => {
            maze.add(x, y, exp2.get(Coordinate(x, y)).getOrElse('?'))
        })
    })

    bfs2(maze, maze.start)

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

class Maze {
    var m = Map[Coordinate, List[Coordinate]]()
    var chars = Map[Coordinate, Char]()
    var start = Coordinate(0, 0)

    def add(x: Int, y: Int, c: Char) = {
        chars = chars + (Coordinate(x, y) -> c)
        c match {
            case '.' => m = m + (Coordinate(x, y) -> List()) 
            case 'S' => 
                start = Coordinate(x, y)
                m = m + (Coordinate(x, y) -> List(
                    Coordinate(x, y + 1),
                    Coordinate(x, y - 1),
                    Coordinate(x + 1, y),
                    Coordinate(x - 1, y)
                ))
            case '|' =>
                m = m + (Coordinate(x, y) -> List(
                    Coordinate(x, y + 1),
                    Coordinate(x, y - 1)
                ))
            case '-' =>
                m = m + (Coordinate(x, y) -> List(
                    Coordinate(x + 1, y),
                    Coordinate(x - 1, y)
                ))
            case 'L' =>
                m = m + (Coordinate(x, y) -> List(
                    Coordinate(x, y - 1),
                    Coordinate(x + 1, y)
                ))
            case'J' =>
                m = m + (Coordinate(x, y) -> List(
                    Coordinate(x, y - 1),
                    Coordinate(x - 1, y)
                ))
            case '7' =>
                m = m + (Coordinate(x, y) -> List(
                    Coordinate(x, y + 1),
                    Coordinate(x - 1, y)
                ))
            case 'F' =>
                m = m + (Coordinate(x, y) -> List(
                    Coordinate(x, y + 1),
                    Coordinate(x + 1, y)
                ))
            case _ => m = m + (Coordinate(x, y) -> List())
        }
    }

    def xmax(): Int = m.keys.maxBy(_.x).x
    def ymax(): Int = m.keys.maxBy(_.y).y

    override def toString(): String = {
        (0 to ymax()).map(y => {
            (0 to xmax()).map(x => {
                val l = m.get(Coordinate(x, y)).get
                if (l.isEmpty) {
                    '.'
                } else {
                    '#'
                }
            }).mkString("")
        }).mkString("\n")
    }
}

def parseLine(maze: Maze, lineWithIndext: (String, Int)): Maze = {
    val (line, y) = lineWithIndext
    line.zipWithIndex
        .foldLeft(maze)((maze, charWithIndex) => {
            val (c, x) = charWithIndex 
            maze.add(x, y, c)
            maze
        })
}

def bfs(maze: Maze, start: Coordinate): Int = {
    var visited = Map[Coordinate, Int]()
    val queue = Queue[(Coordinate, Int)]()
    queue.enqueue((start, 0))
    visited = visited + (start -> 0)

    while (!queue.isEmpty) {
        val (current, steps) = queue.dequeue()
        val neighbours = maze.m.get(current).get
        neighbours.foreach(n => {
            if (!visited.contains(n)) {
                if maze.m.get(n).getOrElse(List()).contains(current) then
                    queue.enqueue((n, steps + 1))
                    visited = visited + (n -> (steps + 1))
            }
        })
    }

    visited.values.max
}

def bfs2(maze: Maze, start: Coordinate): Int = {
    val visited = Set[Coordinate]()
    val queue = Queue[Coordinate]()
    queue.enqueue(start)
    visited.add(start)

    while (!queue.isEmpty) {
        val current = queue.dequeue()
        val neighbours = maze.m.get(current).get
        neighbours.foreach(n => {
            if (!visited.contains(n)) {
                if maze.m.get(n).getOrElse(List()).contains(current) then
                    queue.enqueue(n)
                    visited.add(n)
            }
        })
    }

    // fill
    val points = List(
        Range(0, maze.xmax() + 1).map(x => Coordinate(x, 0)),
        Range(0, maze.xmax() + 1).map(x => Coordinate(x, maze.ymax())),
        Range(0, maze.ymax() + 1).map(y => Coordinate(0, y)),
        Range(0, maze.ymax() + 1).map(y => Coordinate(maze.xmax(), y))
    )
        .flatten
        .foreach(x => queue.enqueue(x))

    while (!queue.isEmpty) {
        val current = queue.dequeue()
        if (!visited.contains(current)) && maze.m.contains(current) then {
            visited.add(current)
            val neighbours = List(
                Coordinate(current.x, current.y + 1),
                Coordinate(current.x, current.y - 1),
                Coordinate(current.x + 1, current.y),
                Coordinate(current.x - 1, current.y)
            )
            neighbours.foreach(n => {
                if (!visited.contains(n)) {
                    queue.enqueue(n)
                }
            })
        }
    }

    (0 to maze.ymax())
        .foldLeft(0)((acc, y) => {
            (0 to maze.xmax())
                .foldLeft(acc)((acc, x) => {
                    val c = Coordinate(x, y)
                    if (visited.contains(c)) {
                        acc
                    } else {
                        acc + 1
                    }
                })
        })

    val visited2 = Set[Coordinate]()
    visited.foreach(c => {
        val x = c.x
        val y = c.y / 2
        if c.y % 2 == 0 then
            visited2.add(Coordinate(x, y))
    })

    val visited3 = Set[Coordinate]()
    visited2.foreach(c => {
        val x = c.x / 2
        val y = c.y
        if c.x % 2 == 0 then
            visited3.add(Coordinate(x, y))
    })

    (0 to maze.ymax() / 2)
        .foldLeft(0)((acc, y) => {
            (0 to maze.xmax() / 2)
                .foldLeft(acc)((acc, x) => {
                    val c = Coordinate(x, y)
                    if (visited3.contains(c)) {
                        acc 
                    } else {
                        acc + 1
                    }
            })
    })
}