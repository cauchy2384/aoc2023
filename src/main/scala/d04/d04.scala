package aoc2023.d04

import scala.collection.mutable.Queue

@main def part1() =
    val ans = solution1("src/main/resources/d04.txt") 
    println(ans)

def solution1(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(parseCard)
        .foldLeft(0)((score, card) => score + card.score())

@main def part2() =
    val ans = solution2("src/main/resources/d04.txt") 
    println(ans)        

def solution2(filename: String): Int =
    val cards = io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(parseCard)
        .foldLeft(Map[Int, Card]())({ 
            (m, card) => m + (card.id() -> card)
         })
    
    val q = cards
        .map((id, card) => id)
        .toList
        .sorted

    var scores = q.foldLeft(Map[Int, Int]())((m, id) => m + (id -> 1))

    Range(1, q.max + 1)
        .foreach(id => {
            if cards.contains(id) then
                val card = cards(id)
                val base = scores(id)
                Range(id + 1, id + card.matches() + 1)
                    .foreach(id => {
                        if scores.contains(id) then
                            val score = scores(id)
                            scores = scores + (id -> (score + base))
                        else
                            scores = scores + (id -> base)
                    })
        })

    scores.foldLeft(0)((total, score) => total + score._2)

class Card(_id: Int, winning: List[Int], nums: List[Int]):
    def id(): Int = _id

    def matches(): Int =
        val set = winning.toSet
        nums
            .filter(set.contains(_))
            .foldLeft(0)((x, _) => x + 1)

    def score(): Int =
        math.pow(2, matches() - 1).toInt
    
    override def toString: String =
        s"Card $_id has score ${score()}: winning $winning, nums $nums"

def parseCard(line: String): Card =
    val splitted = line.split(":")
    val id = splitted(0).replace("Card ", "").trim().toInt

    val nums = splitted(1).trim().split("[|]")
    val winning = nums(0).split("""\s+""")
        .map(_.trim().toInt)
        .toList
    val numsList = nums(1).trim().split("""\s+""")
        .map(_.trim().toInt)
        .toList

    Card(id, winning, numsList)