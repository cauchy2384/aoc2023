import aoc2023.d24

import org.scalatest.prop.TableDrivenPropertyChecks._

class D24 extends munit.FunSuite {
  test("example") {
    val obtained = d24.solution("src/test/resources/d24.txt", 7F, 24F)
    val expected = 2
    assertEquals(obtained, expected)
  }

  test("p1") {
    val obtained = d24.solution("src/main/resources/d24.txt", 200000000000000F, 400000000000000F)
    val expected = 16939 
    assertEquals(obtained, expected)
  }

  test("example p2") {
    d24.solution2("src/test/resources/d24.txt", "src/main/scala/d24p2example.py")
  }

  test("p2") {
    d24.solution2("src/main/resources/d24.txt", "src/main/scala/d24p2.py")
  }
}