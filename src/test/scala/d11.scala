import aoc2023.d11

import org.scalatest.prop.TableDrivenPropertyChecks

class D11 extends munit.FunSuite {
  test("example") {
    val obtained = d11.solution("src/test/resources/d11.txt", 2)
    val expected = 374L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d11.solution("src/main/resources/d11.txt", 2)
    val expected = 9684228L
    assertEquals(obtained, expected)
  }

  test("example p2e1") {
    val obtained = d11.solution("src/test/resources/d11.txt", 10)
    val expected  = 1030L
    assertEquals(obtained, expected)
  }

  test("example p2e2") {
    val obtained = d11.solution("src/test/resources/d11.txt", 100)
    val expected  = 8410L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d11.solution("src/main/resources/d11.txt", 1000000)
    val expected = 483844716556L
    assertEquals(obtained, expected)
  }
}


