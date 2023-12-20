import aoc2023.d20

import org.scalatest.prop.TableDrivenPropertyChecks

class D20 extends munit.FunSuite {
  test("example") {
    val obtained = d20.solution("src/test/resources/d20.txt")
    val expected = 32000000L
    assertEquals(obtained, expected)
  }

  test("example 2") {
    val obtained = d20.solution("src/test/resources/d20e2.txt")
    val expected = 11687500L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d20.solution("src/main/resources/d20.txt")
    val expected = 841763884L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d20.solution2("src/main/resources/d20.txt")
    val expected = 246006621493687L
    assertEquals(obtained, expected)
  }
}