import aoc2023.d07

import org.scalatest.prop.TableDrivenPropertyChecks

class D07 extends munit.FunSuite {
  test("example") {
    val obtained = d07.solution1("src/test/resources/d07.txt")
    val expected = 6440 
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d07.solution1("src/main/resources/d07.txt")
    val expected = 250370104 
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d07.solution2("src/test/resources/d07.txt")
    val expected = 5905 
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d07.solution2("src/main/resources/d07.txt")
    val expected = 251735672
    assertEquals(obtained, expected)
  }
}


