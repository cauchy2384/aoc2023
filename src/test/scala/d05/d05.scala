import aoc2023.d05

import org.scalatest.prop.TableDrivenPropertyChecks

class D05 extends munit.FunSuite {
  test("example") {
    val obtained = d05.solution1("src/test/resources/d05.txt")
    val expected: Long = 35 
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d05.solution1("src/main/resources/d05.txt")
    val expected: Long = 3374647
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d05.solution2("src/test/resources/d05.txt")
    val expected: Long = 46 
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d05.solution2("src/main/resources/d05.txt")
    val expected: Long = 6082852
    assertEquals(obtained, expected)
  }
}


