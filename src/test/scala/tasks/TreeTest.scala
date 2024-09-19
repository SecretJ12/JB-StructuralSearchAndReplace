package de.secretj12
package tasks

import org.scalatest.funsuite.AnyFunSuite

class TreeTest extends AnyFunSuite {
  test("testEquality") {
    val tree1 = new NODE(new ID("hello") :: new ID("world") :: Nil)
    val tree2 = new NODE(new ID("hello") :: new ID("world") :: Nil)
    val tree3 = new NODE(Nil)
    val tree4 = new NODE(Nil)

    assert(tree1 == tree2)
    assert(tree3 == tree4)
  }
  test("testInEquality") {
    val tree1 = new NODE(new ID("hello") :: Nil)
    val tree2 = new NODE(new ID("hello") :: new ID("world") :: Nil)
    val tree3 = new NODE(new ID("Hello") :: new ID("world") :: Nil)

    assertResult(false)(tree1 == tree2)
    assertResult(false)(tree2 == tree3)
  }
  test("testStringRepresentation") {
    val tree1 = new NODE(new ID("hello") :: new ID("world") :: Nil)
    val tree2 = new NODE(new ID("hello") :: Nil)
    val tree3 = new NODE(Nil)
    val tree4 = new NODE(new NODE(new NODE(new ID("Jetbrains") :: Nil) :: Nil) :: Nil)

    assert(tree1.toString == "(hello world)")
    assert(tree2.toString == "(hello)")
    assert(tree3.toString == "()")
    assert(tree4.toString == "(((Jetbrains)))")
  }
}
