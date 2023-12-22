package aoc2023.d22

import scala.annotation.tailrec

def solution(filename: String, part: Int): Int =
    var bricks = io.Source.fromFile(filename)
        .getLines()
        .zipWithIndex
        .map(zline => {
            val (line, idx) = zline
            val s"$from~$to" = line: @unchecked
            val s"$x1,$y1,$z1" = from: @unchecked
            val s"$x2,$y2,$z2" = to: @unchecked
            val fromCoord = Coordinate(x1.toInt, y1.toInt, z1.toInt)
            val toCoord = Coordinate(x2.toInt, y2.toInt, z2.toInt)
            Brick(idx + 1, fromCoord, toCoord)
        })
        .toList

    var (xmax, ymax, zmax) = bricks
        .foldLeft((0, 0, 0))((acc, brick) => {
            val (xmax, ymax, zmax) = acc
            (
                xmax.max(brick.from.x).max(brick.to.x),
                ymax.max(brick.from.y).max(brick.to.y),
                zmax.max(brick.from.z).max(brick.to.z)
            )
        })
    // println(s"($xmax, $ymax, $zmax)")

    def bricks2matrix(bricks: List[Brick]): Matrix =
        bricks.foldLeft(Matrix(xmax, ymax, zmax))((acc, brick) => {
            val (from, to) = (brick.from, brick.to)
            for
                x <- from.x to to.x
                y <- from.y to to.y
                z <- from.z to to.z
            do
                acc.set(x, y, z, brick.id)
            acc
        })

    // var mx = bricks2matrix(bricks)
    // println(mx)

    @tailrec
    def fall(bricks: List[Brick], affected: Set[Int]): (List[Brick], Set[Int]) =
        val mx = bricks2matrix(bricks)
        // println("------------")
        // println(mx)  

        var affected2 = affected
        val (bricks2, total) = bricks
            .foldLeft(List[Brick](), 0)((acc, brick) => {
                val (res, total) = acc
                val ok = canFall(brick, mx)
                // println(s"${brick.name} can fall: $ok")
                if ok then
                    affected2 = affected2 + brick.id
                    (res :+ brick.fall(), total + 1)
                else
                    (res :+ brick, total)
            })
        if total == 0 then
            (bricks2, affected2)
        else
            fall(bricks2, affected2)

    bricks = fall(bricks, Set[Int]())._1

    if part == 1 then
        bricks
            .map(b =>
                // println(s"checking ${b.name}")
                val bricks2 = bricks.filter(_.id != b.id)
                val mx = bricks2matrix(bricks2)
                bricks2
                    .foldLeft(0)((total, brick) => {
                        val ok = canFall(brick, mx)
                        if ok then 
                            // println(s"${brick.name} can fall")
                            total + 1 
                        else 
                            total
                    })
            )
            .filter(_ == 0)
            .size
    else
        bricks
            .map(b =>
                val bricks2 = bricks.filter(_.id != b.id)
                val total = fall(bricks2, Set[Int]())._2.size
                // println(s"checking ${b.name} => $total")
                total
            )
            .sum 

def canFall(brick: Brick, mx: Matrix): Boolean =
    val (from, to) = (brick.from, brick.to)
    if from.z == to.z then
        (from.x to to.x)
            .foldLeft(true)((acc, x) => {
                (from.y to to.y)
                    .foldLeft(acc)((acc, y) => {
                            val z = from.z
                            if z == 1 then
                                false
                            else
                                acc && mx.get(x, y, z - 1) == 0
                    })
            })
    else if from.z == 1 || to.z == 1 then
        false
    else if from.z < to.z then
        mx.get(from.x, from.y, from.z - 1) == 0
    else
        mx.get(to.x, to.y, to.z - 1) == 0

class Matrix(val xmax: Int, val ymax: Int, val zmax: Int) {
    val matrix = Array.ofDim[Int](xmax + 1, ymax + 1, zmax + 1)

    def get(x: Int, y: Int, z: Int): Int = 
        matrix(x)(y)(z)

    def set(x: Int, y: Int, z: Int, value: Int) = 
        matrix(x)(y)(z) = value
    
    override def toString(): String = 
        matrix
            .map(_.map(_.map(v =>
                if v == 0 then 
                    "." 
                else 
                    (v + 'A'.toInt - 1).toChar
                ).mkString).mkString("\n"))
            .mkString("\n\n")
}

class Coordinate(val x: Int, val y: Int, val z: Int) {
    override def toString(): String = 
        s"($x, $y, $z)"

}

class Brick(val id: Int, val from: Coordinate, val to: Coordinate) {
    val name = (id + 'A'.toInt - 1).toChar

    def fall(): Brick =
        Brick(id, Coordinate(from.x, from.y, from.z - 1), Coordinate(to.x, to.y, to.z - 1))

    override def toString(): String = 
        s"($from) -> ($to)"
}