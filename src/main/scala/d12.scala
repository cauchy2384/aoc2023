package aoc2023.d12

def solution(filename: String, folds: Int): Long =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(solveLine(_, folds))
        .foldLeft(0L)(_ + _)

def solveLine(line: String, folds: Int): Long =
    val sp = line.split(" ")
    val left = (sp(0) + "?") * (folds - 1) + sp(0)
    val right = (sp(1) + ",") * (folds - 1) + sp(1)
    val grp = right.split(",").map(_.toInt).toList

    val dp = DP()
    dp.recc(left, 0, 0, grp)

class DP {
    var cache = Map[(String, List[Int]), Long]()

    def recc(line: String, idx: Int, total: Int, grp: List[Int]): Long =
        if cache.contains((line, grp)) then
            return cache((line, grp))
        else
            val res = rec(line, idx, total, grp)
            cache = cache + ((line, grp) -> res)
            return res

    def rec(line: String, idx: Int, total: Int, grp: List[Int]): Long =
        if line.length() == 0 then
            if grp.length == 0 then
                return 1
            else
                return 0
        else if idx == line.length() then
            if grp.length == 1 && grp != Nil && grp.head == total then
                return 1
            else
                return 0
        else if total > 0 && grp.length == 0 then
            return 0
        else if grp != Nil && grp.head < total then
            return 0
        else
            val c = line.charAt(idx)
            if c == '?' then
                val l1 = kek(line, idx, '#', total, grp)
                val l2 = kek(line, idx, '.', total, grp)
                return l1 + l2
            else
                return kek(line, idx, c, total, grp) 

    def kek(line: String, idx: Int, c: Char, total: Int, gpd: List[Int]): Long =
        c match
            case '.' => 
                val l = line.substring(idx + 1, line.length())
                if total == 0 then
                    recc(l, 0, 0, gpd)
                else
                    if gpd.head == total then
                        recc(l, 0, 0, gpd.tail)
                    else    
                        return 0
            case '#' => recc(line, idx + 1, total + 1, gpd)

}