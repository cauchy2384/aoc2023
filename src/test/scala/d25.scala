import aoc2023.d25

import org.scalatest.prop.TableDrivenPropertyChecks._

class D25 extends munit.FunSuite {
  // test("example") {
  //   val obtained = d25.solution("src/test/resources/d25.txt", "example.dot")
  //   val expected = 0
  //   assertEquals(obtained, expected)
  // }

  test("p1") {
    val obtained = d25.solution("src/main/resources/d25.txt", "part1.dot")
    val expected = 0 
    assertEquals(obtained, expected)
  }

  // test("example p2") {
  //   d25.solution2("src/test/resources/d25.txt", "src/main/scala/d25p2example.py")
  // }

  // test("p2") {
  //   d25.solution2("src/main/resources/d25.txt", "src/main/scala/d25p2.py")
  // }
}