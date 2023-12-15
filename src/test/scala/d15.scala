import aoc2023.d15

import org.scalatest.prop.TableDrivenPropertyChecks

class D15 extends munit.FunSuite {
  test("example") {
    val obtained = d15.solution("src/test/resources/d15.txt")
    val expected = 1320L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d15.solution("src/main/resources/d15.txt")
    val expected = 514281L
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d15.solution2("src/test/resources/d15.txt")
    val expected  = 145L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d15.solution2("src/main/resources/d15.txt")
    val expected = 244199L
    assertEquals(obtained, expected)
  }
}