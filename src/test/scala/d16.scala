import aoc2023.d16

import org.scalatest.prop.TableDrivenPropertyChecks

class D16 extends munit.FunSuite {
  test("example") {
    val obtained = d16.solution("src/test/resources/d16.txt")
    val expected = 46
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d16.solution("src/main/resources/d16.txt")
    val expected = 8901 
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d16.solution2("src/test/resources/d16.txt")
    val expected  = 51
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d16.solution2("src/main/resources/d16.txt")
    val expected = 9064
    assertEquals(obtained, expected)
  }
}