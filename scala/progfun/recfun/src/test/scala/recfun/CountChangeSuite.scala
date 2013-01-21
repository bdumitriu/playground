package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  test("example from instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

  test("no money no denomination") {
    assert(countChange(0,List()) === 0)
  }

  test("no money denomination zero") {
    assert(countChange(0,List(0)) === 0)
  }

  test("no money denomination non zero") {
    assert(countChange(0,List(10)) === 0)
  }

  test("no money with denominations including zero") {
    assert(countChange(0,List(0, 1, 10)) === 0)
  }

  test("no money with denominations excluding zero") {
    assert(countChange(0,List(1, 10)) === 0)
  }

  test("negative money") {
    assert(countChange(-10,List(1, 2)) === 0)
  }

  test("money no denomination") {
    assert(countChange(10,List()) === 0)
  }
}
