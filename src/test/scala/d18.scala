import aoc2023.d18

import org.scalatest.prop.TableDrivenPropertyChecks

class D18 extends munit.FunSuite {
  test("example") {
    val obtained = d18.solution("src/test/resources/d18.txt", 1)
    val expected = 62L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d18.solution("src/main/resources/d18.txt", 1)
    val expected = 70026L
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d18.solution("src/test/resources/d18.txt", 2)
    val expected  = 952408144115L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d18.solution("src/main/resources/d18.txt", 2)
    val expected = 68548301037382L
    assertEquals(obtained, expected)
  }
}