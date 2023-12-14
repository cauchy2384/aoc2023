import aoc2023.d14

import org.scalatest.prop.TableDrivenPropertyChecks

class D14 extends munit.FunSuite {
  test("example") {
    val obtained = d14.solution("src/test/resources/d14.txt")
    val expected = 136L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d14.solution("src/main/resources/d14.txt")
    val expected = 110274L
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d14.solution2("src/test/resources/d14.txt", 20)
    val expected  = 64L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d14.solution2("src/main/resources/d14.txt", 500)
    val expected = 90982L
    assertEquals(obtained, expected)
  }
}