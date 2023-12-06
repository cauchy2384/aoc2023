import aoc2023.d06

import org.scalatest.prop.TableDrivenPropertyChecks

class D06 extends munit.FunSuite {
  test("example") {
    val obtained = d06.solution1("src/test/resources/d06.txt")
    val expected = 288 
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d06.solution1("src/main/resources/d06.txt")
    val expected = 170000
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d06.solution2("src/test/resources/d06.txt")
    val expected = 71503 
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d06.solution2("src/main/resources/d06.txt")
    val expected = 20537782
    assertEquals(obtained, expected)
  }
}


