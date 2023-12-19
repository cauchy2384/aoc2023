import aoc2023.d19

import org.scalatest.prop.TableDrivenPropertyChecks

class D19 extends munit.FunSuite {
  test("example") {
    val obtained = d19.solution("src/test/resources/d19.txt")
    val expected = 19114L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d19.solution("src/main/resources/d19.txt")
    val expected = 406934L
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d19.solution2("src/test/resources/d19.txt")
    val expected  = 167409079868000L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d19.solution2("src/main/resources/d19.txt")
    val expected = 131192538505367L
    assertEquals(obtained, expected)
  }
}