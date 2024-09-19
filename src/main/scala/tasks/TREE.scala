package de.secretj12
package tasks

import scala.annotation.tailrec

/**
 * Define tree and with equals method
 */
sealed abstract class TREE:
  def equals(t: Any): Boolean

  def toString: String

/**
 * Node of tree
 */
case class NODE(trees: List[TREE]) extends TREE:
  override def equals(t: Any): Boolean = {
    t.isInstanceOf[NODE]
      && trees.corresponds(t.asInstanceOf[NODE].trees)((t1, t2) => t1.equals(t2))
  }

  override def toString: String =
    "(" + trees.mkString(" ") + ")"

/**
 * Id of tree
 * For simplicity saves id in a String
 */
case class ID(id: String) extends TREE:
  override def equals(t: Any): Boolean = t.isInstanceOf[ID] && id.equals(t.asInstanceOf[ID].id)

  override def toString: String = id

class ParseException(msg: String) extends RuntimeException(msg)

/**
 * Splits a String up by a space as long as all brackets are closed
 *
 * @param s    The text left
 * @param num  Number of open brackets
 * @param cur  Current word
 * @param list Already completed words
 * @return
 */
@tailrec
def split(s: String, num: Int, cur: String, list: List[String]): List[String] = s match {
  case "" =>
    if num != 0
    then throw new ParseException("Invalid node content")
    else if cur == ""
    then list else list ::: cur :: Nil
  case s =>
    s.head match {
      case '(' => split(s.tail, num + 1, cur + '(', list)
      case ')' => split(s.tail, num - 1, cur + ')', list)
      case ' ' =>
        if num == 0
        then split(s.tail, num, "", list ::: cur :: Nil)
        else split(s.tail, num, cur + " ", list)
      case c => split(s.tail, num, cur + c, list)
    }
}

/**
 * Differ between node and ID
 */
val node_pattern = "\\((.*)\\)".r
val id_pattern = "([a-zA-Z0-9]+)".r
def parse(tl: String): TREE = tl match {
  case node_pattern(cont) =>
    NODE(split(cont, 0, "", Nil).map(parse))
  case id_pattern(cont) => ID(cont)
  case s => throw new ParseException(s"Invalid string: \"$s\"")
}

def replace(tree: TREE, searchTree: TREE, replacement: TREE): TREE = {
  if tree == searchTree then return replacement

  tree match {
    case NODE(children) => NODE(children.map(t => replace(t, searchTree, replacement)))
    case ID(id) => ID(id)
  }
}