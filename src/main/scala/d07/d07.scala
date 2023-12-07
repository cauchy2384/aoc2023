package aoc2023.d07

def solution1(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(parseLine(_, false))
        .sortBy(_._1)(Ordering[Hand].reverse)
        .foldLeft((1, 0))((acc, x) => {
            val (hand, value) = x
            val (rank, sum) = acc
            (rank + 1, sum + rank * value)
        })
        ._2

def solution2(filename: String): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(parseLine(_, true))
        .sortBy(_._1)(Ordering[Hand].reverse)
        .foldLeft((1, 0))((acc, x) => {
            val (hand, value) = x
            val (rank, sum) = acc
            (rank + 1, sum + rank * value)
        })
        ._2

class Hand(input: String, useJokers: Boolean) extends Ordered[Hand] {
    val s: String = input
    var withJokers: Boolean = useJokers

    val normalized: String = {
        if !withJokers then
            s
        else
            var x = s
                .toCharArray()
                .filter(_ != 'J')
                .groupBy(identity)
                .mapValues(_.length)
                .toList
                .sortBy(_._2)(Ordering[Int].reverse)
                .map(_._1)
                .take(1)
                .mkString
            if x == "" then x = "J"
            s.replace("J", x)
    }

    val t: Types.Value = {
        var m = normalized
            .toCharArray()
            .groupBy(identity)
            .mapValues(_.length)
            .toList
            .sortBy(_._2)(Ordering[Int].reverse)

        m.size match {
            case 1 => Types.Fives
            case 2 => {
                val (k, v) = m.head
                if (v == 4) Types.Fours else Types.FullHouse
            }
            case 3 => {
                val (k, v) = m.head
                if (v == 3) Types.Threes else Types.TwoPair
            }
            case 4 => Types.OnePair
            case 5 => Types.HighCard 
        }
    }

    def strength(idx: Int): Int = 
        val c = s(idx)
        c match {
            case 'A' => 14
            case 'K' => 13
            case 'Q' => 12
            case 'J' => if withJokers then 1 else 11
            case 'T' => 10
            case _ => c.asDigit
        }

    def compare(that: Hand): Int = 
        if (this.t != that.t) {
            this.t.id - that.t.id
        } else {
            val s = Range(0, this.s.length())
                .map(idx => that.strength(idx) - this.strength(idx))
                .filter(_ != 0)
                .headOption
                .getOrElse(0)
            s
        }

    override def toString(): String = 
        s"Hand($s, $t)"
}

object Types extends Enumeration {
  type Type = Value

  val Fives, Fours, FullHouse, Threes, TwoPair, OnePair, HighCard = Value
}

def parseLine(line: String, useJokers: Boolean): (Hand, Int) =
    val splitted = line.split(" ")
    val hand = Hand(splitted(0).trim(), useJokers)
    val value = splitted(1).trim().toInt

    (hand, value)