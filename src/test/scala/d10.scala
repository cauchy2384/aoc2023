import aoc2023.d10

import org.scalatest.prop.TableDrivenPropertyChecks

class D10 extends munit.FunSuite {
  // test("example1") {
  //   val obtained = d10.solution1("src/test/resources/d10e1.txt")
  //   val expected = 4 
  //   assertEquals(obtained, expected)
  // }

  // test("example2") {
  //   val obtained = d10.solution1("src/test/resources/d10e2.txt")
  //   val expected = 8
  //   assertEquals(obtained, expected)
  // }

  test("p1") {
    val obtained = d10.solution1("src/main/resources/d10.txt")
    val expected = 6599
    assertEquals(obtained, expected)
  }

  test("example1 p2") {
    val obtained = d10.solution2("src/test/resources/d10p2e1.txt")
    val expected  = 4
    assertEquals(obtained, expected)
  }

  test("example2 p2") {
    val obtained = d10.solution2("src/test/resources/d10p2e2.txt")
    val expected  = 4
    assertEquals(obtained, expected)
  }  

  test("example3 p2") {
    val obtained = d10.solution2("src/test/resources/d10p2e3.txt")
    val expected  = 8
    assertEquals(obtained, expected)
  } 

  test("example4 p2") {
    val obtained = d10.solution2("src/test/resources/d10p2e4.txt")
    val expected  = 10
    assertEquals(obtained, expected)
  }  

  test("p2") {
    val obtained = d10.solution2("src/main/resources/d10.txt")
    val expected = 477
    assertEquals(obtained, expected)
  }
}


