import aoc2023.d08

import org.scalatest.prop.TableDrivenPropertyChecks

class D08 extends munit.FunSuite {
  test("example1") {
    val obtained = d08.solution1("src/test/resources/d08e1.txt")
    val expected = 2 
    assertEquals(obtained, expected)
  }

  test("example2") {
    val obtained = d08.solution1("src/test/resources/d08e2.txt")
    val expected = 6
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d08.solution1("src/main/resources/d08.txt")
    val expected = 17141 
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d08.solution2("src/test/resources/d08p2.txt")
    val expected: BigInt = 6 
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d08.solution2("src/main/resources/d08.txt")
    val expected = BigInt("10818234074807")
    assertEquals(obtained, expected)
  }
}


