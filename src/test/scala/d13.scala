import aoc2023.d13

import org.scalatest.prop.TableDrivenPropertyChecks

class D13 extends munit.FunSuite {
  test("example") {
    val obtained = d13.solution("src/test/resources/d13.txt")
    val expected = 405L
    assertEquals(obtained, expected)
  }

  test("example2") {
    val obtained = d13.solution("src/test/resources/d13e2.txt")
    val expected = 400L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d13.solution("src/main/resources/d13.txt")
    val expected = 35521L
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d13.solution2("src/test/resources/d13.txt")
    val expected  = 400L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d13.solution2("src/main/resources/d13.txt")
    val expected = 0L
    assertEquals(obtained, expected)
  }
}


