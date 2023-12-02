import aoc2023.d01

import org.scalatest.prop.TableDrivenPropertyChecks._

class D01 extends munit.FunSuite {
  test("example") {
    val obtained = d01.solution1("src/test/resources/d01.txt")
    val expected = 142
    assertEquals(obtained, expected)
  }

  test("example2") {
    val obtained = d01.solution3("src/test/resources/d01p2.txt")
    val expected = 281
    assertEquals(obtained, expected)
  }

  test("fix") {
    val cases = 
      Table(
        ("line", "expected"),
        ("1abc2", 12),
        ("pqr3stu8vwx", 38),
        ("a1b2c3d4e5f",15),
        ("treb7uchet", 77)
      )

    forAll(cases) { (line, expected) =>
      val obtained = d01.fix(line)
      assertEquals(obtained, expected)
    }
  }
  
  test("parse digits & fix") {
    val cases = 
      Table(
        ("line", "expected"),
        ("two1nine", 29),
        ("eightwothree", 83),
        ("eightwo", 88),
        ("abcone2threexyz", 13),
        ("xtwone3four", 24),
        ("4nineeightseven2", 42),
        ("zoneight234", 14),
        ("7pqrstsixteen", 76),
        ("3nine4fourjclspd152rpv", 32)
      )

    forAll(cases) { (line, expected) =>
      val parsed = d01.parseDigits(line)
      val obtained = d01.fix(parsed)
      assertEquals(obtained, expected)
    }
  }

  test("naebka") {
    val cases = 
      Table(
        ("line", "expected"),
        ("xonex", 11),
        ("eightwo", 82),
        ("two1nine", 29),
        ("eightwothree", 83),
        ("abcone2threexyz", 13),
        ("xtwone3four", 24),
        ("4nineeightseven2", 42),
        ("zoneight234", 14),
        ("7pqrstsixteen", 76),
        ("3nine4fourjclspd152rpv", 32)
      )

    forAll(cases) { (line, expected) =>
      val x = d01.naebka(line)
      val obtained = x._1 * 10 + x._2
      assertEquals(obtained, expected)
    }
  }
}
