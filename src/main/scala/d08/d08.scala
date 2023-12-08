package aoc2023.d08

def solution1(filename: String): Int =
    val (path, map) = io.Source.fromFile(filename)
        .getLines()
        .toStream
        .foldLeft("", Map[String, (String, String)]())((acc, line) => {
            val (path, m) = acc
            if line == "" then
                acc
            else if path == "" then
                (line, m)
            else
                val s"$from = ($l, $r)" = line : @unchecked
                val newM = m + (from -> (l, r))
                (path, newM)
        })

    run(path, map, "AAA", 0)

def run(path: String, m: Map[String, (String, String)], node: String, steps: Int): Int =
    if node == "ZZZ" then steps
    else
        val i = steps % path.length()
        val (l, r) = m(node)
        path(i) match
            case 'L' => run(path, m, l, steps + 1)
            case 'R' => run(path, m, r, steps + 1)
            case _ => throw new Exception("Invalid path")
    

def solution2(filename: String): BigInt =
    val (path, map) = io.Source.fromFile(filename)
        .getLines()
        .toStream
        .foldLeft("", Map[String, (String, String)]())((acc, line) => {
            val (path, m) = acc
            if line == "" then
                acc
            else if path == "" then
                (line, m)
            else
                val s"$from = ($l, $r)" = line : @unchecked
                val newM = m + (from -> (l, r))
                (path, newM)
        })

    map.keys
        .filter(_.endsWith("A"))
        .map(start => run2(path, map, start, 0, start, List()))
        .map(x => {
            println(x)
            BigInt(x(0))
        })
        .foldLeft(0: BigInt)((acc, x) => {
            if acc == 0 then
                x
            else
                lcm(List(acc, x))
        })

def run2(path: String, m: Map[String, (String, String)], node: String, steps: Int, start: String, zs: List[Int]): List[Int] =
    var stop = false
    var zs2 = zs
    if node.endsWith("Z") then
        zs2 = zs2 :+ steps
        if zs2.length > 1 then
            val f = zs2(0)
            stop = steps % f == 0
            zs2 = zs2.dropRight(1)

    if node == start && steps != 0 then
        zs2
    else if stop then
        zs2
    else
        val i = steps % path.length()
        val (l, r) = m(node)
        path(i) match
            case 'L' => run2(path, m, l, steps + 1, start, zs2)
            case 'R' => run2(path, m, r, steps + 1, start, zs2)
            case _ => throw new Exception("Invalid path")

def lcm(list: Seq[BigInt]):BigInt=list.foldLeft(1:BigInt){
  (a, b) => b * a /
  Stream.iterate((a,b)){case (x,y) => (y, x%y)}.dropWhile(_._2 != 0).head._1.abs
}