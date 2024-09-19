package de.secretj12
package tasks

import org.scalatest.funsuite.AnyFunSuite

class TreeTest extends AnyFunSuite {
  test("testEquality") {
    val tree1 = NODE(ID("hello") :: ID("world") :: Nil)
    val tree2 = NODE(ID("hello") :: ID("world") :: Nil)
    val tree3 = NODE(Nil)
    val tree4 = NODE(Nil)

    assert(tree1 == tree2)
    assert(tree3 == tree4)
  }
  test("testInEquality") {
    val tree1 = NODE(ID("hello") :: Nil)
    val tree2 = NODE(ID("hello") :: ID("world") :: Nil)
    val tree3 = NODE(ID("Hello") :: ID("world") :: Nil)

    assertResult(false)(tree1 == tree2)
    assertResult(false)(tree2 == tree3)
  }
  test("testStringRepresentation") {
    val tree1 = NODE(ID("hello") :: ID("world") :: Nil)
    val tree2 = NODE(ID("hello") :: Nil)
    val tree3 = NODE(Nil)
    val tree4 = NODE(NODE(NODE(ID("Jetbrains") :: Nil) :: Nil) :: Nil)

    assert(tree1.toString == "(hello world)")
    assert(tree2.toString == "(hello)")
    assert(tree3.toString == "()")
    assert(tree4.toString == "(((Jetbrains)))")
  }
  test("testParser") {
    val tree1 = NODE(ID("hello") :: ID("world") :: Nil)
    val tree2 = NODE(Nil)
    val s1 = "((a bb) ccc ddd)"
    val s2 = "((()) ())"

    assert(tree1 == parse(tree1.toString))
    assert(tree2 == parse(tree2.toString))
    assert(s1 == parse(s1).toString)
    assert(s2 == parse(s2).toString)
  }
  test("testParserInvalid") {
    val inv1 = "a b c"
    val inv2 = "(((()) ())"
    assertThrows[ParseException] {
      parse(inv1)
    }
    assertThrows[ParseException] {
      parse(inv2)
    }
  }
  test("testReplace1") {
    val tree = "((a b) a)"
    val sTree = "a"
    val r = "c"
    val exp = "((c b) c)"

    assert(replace(parse(tree), parse(sTree), parse(r)).toString == exp)
  }
  test("testReplace2") {
    val tree = "(((a d) b) a)"
    val sTree = "(a d)"
    val r = "c"
    val exp = "((c b) a)"

    assert(replace(parse(tree), parse(sTree), parse(r)).toString == exp)
  }
  test("testReplace3") {
    val tree = "((a b) a)"
    val sTree = "(a d)"
    val r = "c"
    val exp = "((a b) a)"

    assert(replace(parse(tree), parse(sTree), parse(r)).toString == exp)
  }
}
