package aoc2023.d02

import scala.collection.mutable.ListBuffer

def solution1(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(isLineValid)
        .filter(_._2)
        .map(_._1)
        .reduce((a, b) => a + b)

def isLineValid(line: String): (Int, Boolean) =
    val splitted = line.split(":")
    val game = splitted(0).replace("Game ", "").toInt
    val sets = splitted(1).split(";")

    val isValid = sets
        .map(set => set.trim().split(",")
            .map(reveal => reveal.trim().split(" "))
            .map(reveal => (reveal(0).toInt, reveal(1).trim()))
            .map(reveal => reveal._2 match {
                case "green" => reveal._1 <= 13
                case "blue" => reveal._1 <= 14
                case "red" => reveal._1 <= 12
            })
            .fold(true)(_ && _)
        )
        .fold(true)(_ && _)

    return (game, isValid)

def solution2(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(power)
        .map((a, b, c) => a * b * c)
        .reduce((a, b) => a + b)

def power(line: String): (Int, Int, Int) =
    val splitted = line.split(":")
    val sets = splitted(1).split(";")

    sets
      .map(set => set.trim().split(",")
        .map(reveal => reveal.trim().split(" "))
        .map(reveal => (reveal(0).toInt, reveal(1).trim()))
      )
      .flatten
      .foldLeft((0, 0, 0)) { (acc: (Int, Int, Int), x: (Int, String)) =>
        val (green, blue, red) = acc
        val (num, color) = x
        color match {
          case "green" => (green.max(num), blue, red)
          case "blue" => (green, blue.max(num), red)
          case "red" => (green, blue, red.max(num))
        }
      }  