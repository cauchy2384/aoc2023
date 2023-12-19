package aoc2023.d19

import scala.annotation.tailrec

def solution(filename: String): Long =
    var wfs = parseWorkflows(filename)

    parseParts(filename)
        .map(applyWorkflows(_, "in",wfs))
        .filter(_._2 == "A")
        .map(_._1.rating())
        .sum

def solution2(filename: String): Long =
    val wfs = parseWorkflows(filename)

    var res = List[List[(Rule, Boolean)]]()

    def findAllPaths(node: String, path: List[(Rule, Boolean)]): Boolean =
        // println(s"node: $node, path: $path")
        if node == "A" || node == "R" then
            res = res :+ path
            true
        else
            val p = wfs(node).rules
                .foldLeft(path)((path, rule) => {
                    if rule.check != "" then
                        findAllPaths(rule.workflow, path :+ (rule, true))
                    path :+ (rule, false)
                })
            findAllPaths(p.last._1.workflow, p)
            false

    findAllPaths("in", List[(Rule, Boolean)]())

    res = res
        .filter(_.last._1.workflow == "A")
        .map(l => l.filter(_._1.threshold != 0L))

    res
        .map(l => 
            l.foldLeft(State())((state, rule) => {
                state.applyRule(rule._1, rule._2)
                state
            })
        )
        .map(_.total())
        .sum


class State {
    var limits = Map[String, (Long, Long)](
        "x" -> (1L, 4000L),
        "m" -> (1L, 4000L),
        "a" -> (1L, 4000L),
        "s" -> (1L, 4000L),
    )

    def applyRule(rule: Rule, ok: Boolean) =
        if ok then
            rule.check match
                case "<" =>
                    val (min, max) = limits(rule.category)
                    limits = limits + (rule.category -> (min, rule.threshold - 1))
                case ">" =>
                    val (min, max) = limits(rule.category)
                    limits = limits + (rule.category -> (rule.threshold + 1, max))
                case _ =>
        else
            rule.check match
                case "<" =>
                    val (min, max) = limits(rule.category)
                    limits = limits + (rule.category -> (rule.threshold, max))
                case ">" =>
                    val (min, max) = limits(rule.category)
                    limits = limits + (rule.category -> (min, rule.threshold))
                case _ =>

    def total(): Long = 
        limits.values.map((min, max) => max - min + 1).product
    
    override def toString(): String = 
        s"${limits.toString()} -> ${total()}"
}


@tailrec
def applyWorkflows(part: Part, wf: String, wfs: Map[String, Workflow]): (Part, String) =
    var workflow = wfs(wf)
    val next = workflow.rules
        .map(rule => rule.check(part))
        .filter(_ != "")
        .head

    next match
        case "A" => (part, "A")
        case "R" => (part, "R")
        case _ =>
            applyWorkflows(part, next, wfs)


def parseWorkflows(filename: String): Map[String, Workflow] =
    io.Source.fromFile(filename)
        .getLines()
        .takeWhile(_ != "")
        .foldLeft(Map[String, Workflow]())((m, line) => 
            val s"$name{$rules}" = line: @unchecked
            val rs = rules.split(",")
                .map(rule => {
                    if rule.contains(":") then
                        val category = rule.take(1)
                        val check = rule.drop(1).take(1)
                        val threshold = rule.drop(2).takeWhile(_ != ':').toLong
                        val workflow = rule.dropWhile(_ != ':').drop(1)
                        Rule(category, check, threshold, workflow)
                    else
                        Rule("", "", 0L, rule)
                })
                .toList
            m + (name -> Workflow(name, rs))
        )

def parseParts(filename: String): Iterator[Part] =
    io.Source.fromFile(filename)
        .getLines()
        .dropWhile(_ != "")
        .drop(1)
        .map(line => {
            val s"{$details}" = line: @unchecked
            val categories = details.split(",")
                .map(detail => {
                    val s"$category=$value" = detail: @unchecked
                    (category, value.toLong)
                })
                .toMap
            Part(categories)
        })



class Workflow(val name: String, val rules: List[Rule])

class Rule (val category: String, val check: String, val threshold: Long,  val workflow: String) {
    def check(part: Part): String = 
        // println(s"category: $category, check: $check, threshold: $threshold, workflow: $workflow")
        check match
            case "<" =>
                val v = part.categories(category)
                if v < threshold then workflow else ""
            case ">" =>
                val v = part.categories(category)
                if v > threshold then workflow else ""
            case _ =>
                workflow

    def inverse(): Rule =
        check match
            case "<" => Rule(category, ">", threshold, workflow)
            case ">" => Rule(category, "<", threshold, workflow)
            case _ => Rule(category, check, threshold, workflow)

    override def toString(): String = 
        s"($category$check$threshold,$workflow)"
}

class Part(val categories: Map[String, Long]) {
    def rating(): Long =
        categories.values.sum
}