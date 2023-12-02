package aoc2023.d02

import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer 

@main def part1() =
    val ans = solution1("src/main/resources/d02.txt") 
    println(ans)

def solution1(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(isLineValid)
        .reduce((a, b) => a + b)

def isLineValid(line: String): Int =
    val splitted = line.split(":")
    val game = splitted(0).replace("Game ", "").toInt
    val sets = splitted(1).split(";")
    var possible = true
    sets.foreach({
        // parse "7 green, 4 blue, 3 red" in list
        set => {
            val splittedSet = set.split(",")
            splittedSet.foreach({
                item => {
                    val splittedItem = item.trim().split(" ")
                    val count = splittedItem(0).toInt
                    val color = splittedItem(1)
                    // check that there is no more than
                    var limits = Map("green" -> 13, "blue" -> 14, "red" -> 12) 
                    breakable {
                        limits.foreach({
                            case (k, v) => {
                                if (color == k && count > v) {
                                    possible = false
                                    break
                                }
                            }
                        })
                    }
                }
            })
        }
    })

    if !possible then 0 else game

@main def part2() =
    val ans = solution2("src/main/resources/d02.txt") 
    println(ans)

def solution2(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(calcLinePower)
        .reduce((a, b) => a + b)

def calcLinePower(line: String): Int =
    val splitted = line.split(":")
    val sets = splitted(1).split(";")

    var blues = ListBuffer[Int]()
    var greens = ListBuffer[Int]() 
    var reds =  ListBuffer[Int]()
    sets.foreach({
        set => {
            val splittedSet = set.split(",")
            splittedSet.foreach({
                item => {
                    val splittedItem = item.trim().split(" ")
                    val count = splittedItem(0).toInt
                    val color = splittedItem(1)
                    color match {
                        case "blue" => blues += count
                        case "green" => greens += count
                        case "red" => reds += count
                    }
                }
            })
        }
    })

    val power = blues.max * greens.max * reds.max
    power
