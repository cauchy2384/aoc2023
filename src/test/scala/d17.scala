import aoc2023.d17

import org.scalatest.prop.TableDrivenPropertyChecks

class D17 extends munit.FunSuite {
  test("example") {
    val obtained = d17.solution("src/test/resources/d17.txt", 0, 3)
    val expected = 102
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d17.solution("src/main/resources/d17.txt", 0, 3)
    val expected = 674
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d17.solution("src/test/resources/d17.txt", 4, 10)
    val expected  = 94
    assertEquals(obtained, expected)
  }

  test("example2 p2") {
    val obtained = d17.solution("src/test/resources/d17e2.txt", 4, 10)
    val expected  = 71
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d17.solution("src/main/resources/d17.txt", 4, 10)
    val expected = 773
    assertEquals(obtained, expected)
  }
}