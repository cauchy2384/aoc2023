package aoc2023.d25

import java.io.FileWriter
import java.io.File

import scala.collection.mutable.Queue

def solution(filename: String, output: String): Int =
    val graph = parseFile(filename)

    val total = graph.g.keys.size
    println(s"Total: $total")

    // graph.g.foreach((key, values) => 
    //     println(s"$key: $values")
    // )

    // def dfs(node: String, visited: Set[String]): Set[String] =
    //     if visited.contains(node) then
    //         visited
    //     else
    //         val newVisited = visited + node
    //         graph.g(node).foldLeft(newVisited)((acc, x) => dfs(x, acc))

    def countDisconected(graph: Graph): (Int, Int) = 
        var notVisited = graph.g.keys.toSet
        val groups = graph.g.map((key, _) =>
            if !notVisited.contains(key) then
                0
            else
                var visited = Set[String]()
                def dfs(node: String): Boolean = 
                    if !visited.contains(node) then
                        visited = visited + node
                        graph.g(node).foreach(dfs)
                        false
                    else
                        false

                dfs(key)
                notVisited = notVisited -- visited
                visited.size
        )
        .filter(_ > 0)
        .toSet

        val ans = groups.foldLeft(1)((acc, x) => acc * x)
        
        (groups.size, ans)

    def remove(g: Graph, opts: List[(String, String)], removed: List[(String, String)]): Boolean =
        val (v, ans) = countDisconected(g)
        // println(s"separates: $v")
        if removed.size == 3 then
            // println(s"Removed: $removed, Separates: $v")
            if v == 2 then
                println(s"Found!, Removed: $removed, Separates: $v, ans: $ans")
                true
            else
                false
        // else if v > 2 then
        //     false
        else 
            opts.map((key, value) => 
                val g2 = g.deepCopy()
                g2.remove(key, value)
                val opts2 = opts.filter((k, v) => k != key && v != value)
                remove(g2, opts2, removed :+ (key, value))
            )
            // .filter(_ == true)
            .head

    val candidates = graph.candidates()
    // zng fmr
    // krf crg
    // rgv jct
    // println(s"Candidates: $candidates")

    // val start = graph.g.keys.head
    // dfs(start, Set())

    // dist.toList.sortBy((key, value) => key)
    // .foreach((key, value) => 
    //     println(s"$key: $value")
    // )

    // val fileWriter = new FileWriter(new File(output))

    // fileWriter.write("digraph G {\n")
    // fileWriter.write("\tratio=expand\n")
    // graph.g.foreach((key, values) =>
    //     values.foreach(value => 
    //         fileWriter.write(s"\t$key -> $value\n")
    //     )
    // )        
    // fileWriter.write("}")

    // fileWriter.close()


    // candidates
    //     .combinations(3)
    //     .foreach(opts =>
    //         val g2 = graph.deepCopy()
    //         opts.foreach((key, value) => 
    //             g2.remove(key, value)
    //         )
    //         val (v, ans) = countDisconected(g2)
    //         if v == 2 then
    //             println(s"Found!, Removed: $opts, Separates: $v, ans: $ans")
    //             ans
    //         else
    //             0
    //     )

    // zhg fmr
    // krf crg
    // rgv jct
    val toRemove = List(
        ("fmr", "zhg"),
        ("crg", "krf"),
        ("jct", "rgv")
    )
        .foreach((key, value) => 
            graph.g = graph.g + (key -> (graph.g(key) - value))
            graph.g = graph.g + (value -> (graph.g(value) - key))
        )
    val (v, ans) = countDisconected(graph)
    println(s"v: $v, ans: $ans")

    val fileWriter = new FileWriter(new File(output))

    fileWriter.write("digraph G {\n")
    fileWriter.write("\tratio=expand\n")
    graph.g.foreach((key, values) =>
        values.foreach(value => 
            fileWriter.write(s"\t$key -> $value\n")
        )
    )        
    fileWriter.write("}")

    fileWriter.close()

    // val g2 = graph.deepCopy()
    // // hfx/pzl, the wire between bvb/cmg, and the wire between nvd/jqt
    // g2.remove("hfx", "pzl")
    // g2.remove("bvb", "cmg")
    // g2.remove("nvd", "jqt")
    // println(countDisconected(g2))

    // graph.candidates().foreach(println)

    // var visits = Map[(String, String), Int]()

    // def bfs(q: Queue[(String, Set[String])], target: String): Int = 
    //     if q.isEmpty then
    //         0
    //     else
    //         val (node, visited) = q.dequeue()
    //         if node == target then
    //             1
    //         else
    //             val newVisited = visited + node
    //             val newQ = q ++ graph.g(node).map(x => 
    //                 val key = if node < x then (node, x) else (x, node)
    //                 visits = visits + (key -> (visits.getOrElse(key, 0) + 1))
    //                 (x, newVisited)
    //             )
    //             bfs(newQ, target)
        
    // graph.g.keys
    //     .toList
    //     .combinations(2)
    //     .map(list => 
    //         val (a, b) = (list(0), list(1))
    //         if a < b then (a, b) else (b, a)
    //     )
    //     .toSet
    //     .toList
    //     .foreach((a, b) =>
    //         bfs(Queue((a, Set[String]())), b)
    //     )

    // visits.toList
    //     .sortBy((key, value) => value)
    //     .reverse
    //     .foreach(println)

    0

class Graph {
    var g = Map[String, Set[String]]()

    def add(key: String, value: String) =
        _add(key, value)
        _add(value, key)

    def _add(key: String, value: String) =
        if !g.contains(key) then
            g = g + (key -> Set[String](value))
        else 
            g = g + (key -> (g(key) + value))

    def remove(key: String, value: String) =
        _remove(key, value)
        _remove(value, key)

    def _remove(key: String, value: String) =
        if g.contains(key) then
            g = g + (key -> (g(key) - value))

    def deepCopy(): Graph = 
        val g2 = Graph()
        g.foreach((key, values) => 
            values.foreach(value => 
                g2.add(key, value)
            )
        )
        g2

    def candidates(): List[(String, String)] =
        g
            .toList
            .sortBy((key, values) => values.size)
            .reverse
            .map((key, values) => 
                values.map(value => 
                    if key < value then
                        (key, value)
                    else
                        (value, key)
                )
            )
            .flatten
            .toSet
            .toList

    override def toString(): String = 
        g.map((key, values) => 
            s"$key: ${values.mkString(", ")}"
        )
        .mkString("\n")
}

def parseFile(filename: String): Graph =
    val g = Graph()
    
    io.Source.fromFile(filename)
        .getLines()
        .map(line =>
            val splitted = line.split(":")
            val key = splitted(0).trim()
            val values = splitted(1).split(" ")
                .map(_.trim())
                .filter(_.length > 0)
                .toSet
            (key, values)
        )
        .foreach(pair => {
            val (key, values) = pair
            values.foreach(value => 
                g.add(key, value)
            )
        })
    
    g