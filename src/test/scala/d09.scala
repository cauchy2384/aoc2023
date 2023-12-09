import aoc2023.d09

import org.scalatest.prop.TableDrivenPropertyChecks

class D09 extends munit.FunSuite {
  test("example1") {
    val obtained = d09.solution1("src/test/resources/d09.txt")
    val expected = 114 
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d09.solution1("src/main/resources/d09.txt")
    val expected = 1647269739
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d09.solution2("src/test/resources/d09.txt")
    val expected  = 2
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d09.solution2("src/main/resources/d09.txt")
    val expected = 864
    assertEquals(obtained, expected)
  }
}


