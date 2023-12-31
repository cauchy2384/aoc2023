import aoc2023.d02

import org.scalatest.prop.TableDrivenPropertyChecks

class D02 extends munit.FunSuite {
  test("example") {
    val obtained = d02.solution1("src/test/resources/d02.txt")
    val expected = 8
    assertEquals(obtained, expected)
  }

  test("part1") {
    val obtained = d02.solution1("src/main/resources/d02.txt")
    val expected = 2447
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d02.solution2("src/test/resources/d02.txt")
    val expected = 2286
    assertEquals(obtained, expected)
  }

  test("part2") {
    val obtained = d02.solution2("src/main/resources/d02.txt")
    val expected = 56322
    assertEquals(obtained, expected)
  }
}
