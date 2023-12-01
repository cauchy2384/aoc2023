package aoc2023.d01

import scala.util.control.Breaks._

@main def part1() =
    val ans = solution1("src/main/resources/d01.txt") 
    println(ans)

def solution1(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(fix)
        .reduce(_ + _)

def fix(line: String): Int =
    val digits = line
        .filter(_.isDigit)
        .map(_.asDigit)
    
    digits.head * 10 + digits.last

@main def part2wrong() =
    val ans = solution2("src/main/resources/d01.txt") 
    println(ans)

def solution2(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(parseDigits)
        .map(fix)
        .reduce(_ + _)

def parseDigits(line: String): String =
    val mappings = List(
        ("one", "1"),
        ("two", "2"),
        ("three", "3"),
        ("four", "4"),
        ("five", "5"),
        ("six", "6"),
        ("seven", "7"),
        ("eight", "8"),
        ("nine", "9")
    )

    line.foldLeft("") { (acc, char) =>
        mappings.foldLeft(acc + char) { (acc, mapping) =>
            acc.replaceAll(mapping._1, mapping._2)
        }
    }

@main def part2() =
    val ans = solution3("src/main/resources/d01.txt") 
    println(ans)

def solution3(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(naebka)
        .map(x => x._1 * 10 + x._2)
        .reduce(_ + _)

def naebka(line: String): (Int, Int) =
    val mappings = Map(
        "one" -> 1,
        "two" -> 2,
        "three" -> 3,
        "four" -> 4,
        "five" -> 5,
        "six" -> 6,
        "seven" -> 7,
        "eight" -> 8,
        "nine" -> 9
    )

    var s = ""
    var result = (0, 0)
    breakable {
        for(i <- 0 until line.length){
            for digit <- mappings.keys do
                if s.contains(digit) then
                    result = (mappings(digit), mappings(digit))
                    break

            val c = line(i)
            if !c.isDigit then
                s += c
            else {
                result = (c.asDigit, c.asDigit)
                break
            }
        }
    }

    s = ""
    breakable {
        for(i <- line.length - 1 to 0 by -1){
            for digit <- mappings.keys do
                if s.contains(digit) then
                    result = (result._1, mappings(digit))
                    break
                
            val c = line(i)
            if !c.isDigit then
                s = c + s
            else {
                result = (result._1, c.asDigit)
                break
            }
        }
    }

    result  
    