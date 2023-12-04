import aoc2023.d04

import org.scalatest.prop.TableDrivenPropertyChecks

class D04 extends munit.FunSuite {
  test("example") {
    val obtained = d04.solution1("src/test/resources/d04.txt")
    val expected = 13 
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d04.solution1("src/main/resources/d04.txt")
    val expected = 15205 
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d04.solution2("src/test/resources/d04.txt")
    val expected = 30 
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d04.solution2("src/main/resources/d04.txt")
    val expected = 6189740
    assertEquals(obtained, expected)
  }
}


