import aoc2023.d03

import org.scalatest.prop.TableDrivenPropertyChecks

class D03 extends munit.FunSuite {
  test("example") {
    val obtained = d03.solution1("src/test/resources/d03.txt")
    val expected = 4361 
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d03.solution1("src/main/resources/d03.txt")
    val expected = 556367
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d03.solution2("src/test/resources/d03.txt")
    val expected = 467835 
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d03.solution2("src/main/resources/d03.txt")
    val expected = 89471771
    assertEquals(obtained, expected)
  }
}


