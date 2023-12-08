package aoc2023.d03

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._ 

def solution1(filename: String): Int =
    val matrix = parseMatrix(filename)
    
    var (nums, _) = matrix.foldLeft((List[Int](), 0))((acc, row) => {
        val (nums, rn) = acc
        val (nums2, _, _, _) = row.foldLeft((nums, 0, -1, -1))((acc, cell) => {
            var (nums, cn, l, r) = acc

            if cell.isDigit then
                if l == -1 then 
                    l = cn
                    r = cn
                else r = cn
            
            if !cell.isDigit || cn == row.length - 1 then
                if l != -1 && r != -1 then
                    if isValidNumber(matrix, rn, l, r) then
                        val number = row.slice(l, r + 1).mkString.toInt
                        nums = nums :+ number
                l = -1
                r = -1

            (nums, cn + 1, l, r)
        })
        (nums2, rn + 1)
    })

    nums.sum

def parseMatrix(filename: String): Array[Array[Char]] =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(_.toCharArray())
        .toArray

def isValidNumber(matrix: Array[Array[Char]], rn: Int, l: Int, r: Int): Boolean =
    // for every cell around
    val w = Array(rn - 1, rn, rn + 1)
        .foldLeft(0)((acc, row) => {
            Range(l - 1, r + 2)
                .foldLeft(acc)((acc, col) => {
                    if row == rn && col >= l && col <= r then 
                        acc
                    else
                        acc + cellWeight(matrix, row, col)
                })
        })
    
    w > 0

def cellWeight(matrix: Array[Array[Char]], row: Int, col: Int): Int =
    // if not in matrix
    if row < 0 || row >= matrix.length || col < 0 || col >= matrix(row).length then return 0

    // if not a dot
    if matrix(row)(col) != '.' then return 1

    return 0

def solution2(filename: String): Int =
    val matrix = parseMatrix(filename)
    
    var (ratios, _) = matrix.foldLeft((List[Int](), 0))((acc, row) => {
        val (ratios, rn) = acc
        val (ratios2, _, _, _) = row.foldLeft((ratios, 0, -1, -1))((acc, cell) => {
            var (ratios, cn, l, r) = acc

            if cell == '*' then
                val nums = numsAround(matrix, rn, cn)
                if nums.length == 2 then
                    val power = nums(0) * nums(1)
                    ratios = ratios :+ power

            (ratios, cn + 1, l, r)
        })
        (ratios2, rn + 1)
    })

    ratios.sum

def numsAround(matrix: Array[Array[Char]], rn: Int, cn: Int): List[Int] =
    val nums = ListBuffer[Int]()

    var visited = Set[(Int, Int)]()

    // for every cell around
    Array(rn - 1, rn, rn + 1)
        .foreach((row) => {
            Range(cn - 1, cn + 2)
                .foreach((col) => {
                    if visited contains (row, col) then 
                        ()
                    else if row < 0 || row >= matrix.length || col < 0 || col >= matrix(row).length then 
                        ()
                    else if !matrix(row)(col).isDigit then
                        ()
                    else
                        val (num, newset) = findNumber(matrix, row, col)
                        visited = visited ++ newset
                        nums.append(num)
                })
        })

    nums.toList

def findNumber(matrix : Array[Array[Char]], row: Int, col: Int): (Int, Set[(Int, Int)]) =
    // go left while digits
    var l = col
    while l >= 1 && matrix(row)(l - 1).isDigit do
        l -= 1
    // go right while digits
    var r = col
    while r < matrix(r).length - 1 && matrix(row)(r + 1).isDigit do
        r += 1

    var set = Set[(Int, Int)]()
    Range(l, r + 1)
        .foreach((c) => {
            set = set + ((row, c))
        })

    val num = matrix(row).slice(l, r + 1).mkString.toInt

    (num, set)