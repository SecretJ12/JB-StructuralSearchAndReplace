package de.secretj12
package tasks

/**
 * Define tree and with equals method
 */
abstract class TREE:
  def equals(t: Any): Boolean

  def toString: String

/**
 * Node of tree
 */
class NODE(val trees: List[TREE]) extends TREE:
  override def equals(t: Any): Boolean = {
    t.isInstanceOf[NODE]
      && trees.corresponds(t.asInstanceOf[NODE].trees)((t1, t2) => t1.equals(t2))
  }

  override def toString: String =
    "(" + trees.mkString(" ") + ")"

/**
 * Id of tree
 * For simplicity saves id in a String
 * (probably add sanitizing later)
 */
class ID(val id: String) extends TREE:
  override def equals(t: Any): Boolean = t.isInstanceOf[ID] && id.equals(t.asInstanceOf[ID].id)

  override def toString: String = id