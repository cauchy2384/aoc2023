package aoc2023.d13

def solution2(filename: String): Long =
    var (mxs, mx, _) = io.Source.fromFile(filename)
        .getLines()
        .foldLeft((List[Matrix](), Matrix(), 0))((acc, line) => {
            var (mats, mat, y) = acc
            if (line == "") {
                (mats :+ mat, Matrix(), 0)
            } else {
                line.zipWithIndex.foreach((c, x) => {
                    mat.add(Coordinate(x, y), c)
                })
                (mats, mat, y + 1)
            }
        })
    mxs = mxs :+ mx

    mxs
        .map(mx => {
            var xs2 = List[Int]()

            (0 to mx.xmax()).foreach(x => {
                (0 to mx.ymax()).foreach(y => {
                    val c = Coordinate(x, y)
                    val nm = mx.switch(c)
                    nm.reflection(true)
                        .map(x => x + 1)
                        .foreach(x => xs2 = xs2 :+ x)
                    nm.reflection(false)
                        .map(x => x + 1)
                        .map(x => x * 100)
                        .foreach(x => xs2 = xs2 :+ x)
                })
            })
            xs2 = xs2.distinct
            
            var xs = List[Int]()
            mx.reflection(true)
                .map(x => x + 1)
                .foreach(x => xs = xs :+ x)
            mx.reflection(false)
                .map(x => x + 1)
                .map(x => x * 100)
                .foreach(x => xs = xs :+ x)
                
            val diff = xs2.diff(xs)
            
            diff
        })
        .flatten
        .foldLeft(0L)(_ + _)

def solution(filename: String): Long =
    var (mxs, mx, _) = io.Source.fromFile(filename)
        .getLines()
        .foldLeft((List[Matrix](), Matrix(), 0))((acc, line) => {
            var (mats, mat, y) = acc
            if (line == "") {
                (mats :+ mat, Matrix(), 0)
            } else {
                line.zipWithIndex.foreach((c, x) => {
                    mat.add(Coordinate(x, y), c)
                })
                (mats, mat, y + 1)
            }
        })
    mxs = mxs :+ mx

    mxs
        .map(mx => {
            var xs = List[Int]()
            mx.reflection(true)
                .map(x => x + 1)
                .foreach(x => xs = xs :+ x)
            mx.reflection(false)
                .map(x => x + 1)
                .map(x => x * 100)
                .foreach(x => xs = xs :+ x)
            if xs.length != 1 then
                println("-----")
                println(mx)
                println(s"xs: $xs")
            xs
        })
        .flatten
        .foldLeft(0L)(_ + _)

class Matrix {
    var mx = Map[Coordinate, Char]()

    def xmax(): Int = mx.keys.map(_.x).max
    def ymax(): Int = mx.keys.map(_.y).max

    def add(c: Coordinate, v: Char) = mx = mx + (c -> v)

    def switch(c: Coordinate): Matrix = {
        val v = mx(c)
        val nv = v match {
            case '.' => '#'
            case '#' => '.'
            case _ => v
        }
        val nmx = mx + (c -> nv)
        val nmat = Matrix()
        nmat.mx = nmx
        nmat
    }

    def reflection(horizontal: Boolean): List[Int] = {
        checkColumns(0, List[Int](), horizontal) 
    }

    private def checkColumns(x: Int, xs: List[Int], horizontal: Boolean): List[Int] = {
        val limit = if horizontal then xmax() else ymax()
        // println(s"checkColumns($x, $horizontal")
        if x == limit then
            xs
        else
            if checkColumn(x, 0, horizontal) then
                checkColumns(x + 1, xs :+ x, horizontal)
            else 
                checkColumns(x + 1, xs, horizontal)
    }

    private def checkColumn(x: Int, y: Int, horizontal: Boolean): Boolean = {
        // println(s"checkColumn($x, $y)")
        val limit = if horizontal then ymax() else xmax()
        if y > limit then
            true
        else
            val rowlimit = if horizontal then xmax() else ymax()
            val delta = (x + 1).min(rowlimit - x)
            // println(s"checkColumn($x, $y) => delta: $delta")
            val ok = (1 to delta)
                .foldLeft(true)((acc, i) => {
                    acc && twoEqual(x - i + 1, x + i, y, horizontal)
                })
            if ok then
                checkColumn(x, y + 1, horizontal)
            else
                false
    }

    private def twoEqual(x1: Int, x2: Int, y: Int, horizontal: Boolean): Boolean = {
        val ok = if horizontal then
            // println(s"($x1, $y), ($x2, $y) => ${mx(Coordinate(x1, y))} & ${mx(Coordinate(x2, y))} => ${mx(Coordinate(x1, y)) == mx(Coordinate(x2, y))}")
            mx(Coordinate(x1, y)) == mx(Coordinate(x2, y))
        else
            // println(s"($y, $x1), ($y, $x2) => ${mx(Coordinate(y, x1))} & ${mx(Coordinate(y, x2))} => ${mx(Coordinate(y, x1)) == mx(Coordinate(y, x2))}")
            mx(Coordinate(y, x1)) == mx(Coordinate(y, x2))
        ok
    }

    override def toString(): String = 
        (0 to ymax())
            .map(y => (0 to xmax())
                .map(x => mx(Coordinate(x, y)))
                .mkString("")
            )
            .mkString("\n")
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