import aoc2023.d22

import org.scalatest.prop.TableDrivenPropertyChecks._

class D22 extends munit.FunSuite {
  test("example") {
    val obtained = d22.solution("src/test/resources/d22.txt", 1)
    val expected = 5
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d22.solution("src/main/resources/d22.txt", 1)
    val expected = 522
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d22.solution("src/test/resources/d22.txt", 2)
    val expected = 7
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d22.solution("src/main/resources/d22.txt", 2)
    val expected = 83519
    assertEquals(obtained, expected)
  }
}