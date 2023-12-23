import aoc2023.d23

import org.scalatest.prop.TableDrivenPropertyChecks._

class D23 extends munit.FunSuite {
  test("example") {
    val obtained = d23.solution("src/test/resources/d23.txt")
    val expected = 94L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d23.solution("src/main/resources/d23.txt")
    val expected = 2318L
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d23.solution2("src/test/resources/d23.txt")
    val expected = 154L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d23.solution2("src/main/resources/d23.txt")
    val expected = 6426L
    assertEquals(obtained, expected)
  }
}