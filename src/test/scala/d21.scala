import aoc2023.d21

import org.scalatest.prop.TableDrivenPropertyChecks._

class D21 extends munit.FunSuite {
  test("example") {
    val obtained = d21.solution("src/test/resources/d21.txt", 6L)
    val expected = 16L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d21.solution("src/main/resources/d21.txt", 64)
    val expected = 3776L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d21.solution2("src/main/resources/d21.txt", 26501365)
    val expected = 625587097150084L
    assertEquals(obtained, expected)
  }
}