import aoc2023.d12

import org.scalatest.prop.TableDrivenPropertyChecks

class D12 extends munit.FunSuite {
  test("example") {
    val obtained = d12.solution("src/test/resources/d12.txt", 1)
    val expected = 21L
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d12.solution("src/main/resources/d12.txt", 1)
    val expected = 7173L
    assertEquals(obtained, expected)
  }

  test("example p2") {
    val obtained = d12.solution("src/test/resources/d12.txt", 5)
    val expected  = 525152L
    assertEquals(obtained, expected)
  }

  test("p2") {
    val obtained = d12.solution("src/main/resources/d12.txt", 5)
    val expected = 29826669191291L
    assertEquals(obtained, expected)
  }

  // test("grouped") {
  //   val line = "####?...."
  //   val obtained = d12.grouped(line)
  //   val expected = List(4)
  //   assertEquals(obtained, expected)
  // }
}


