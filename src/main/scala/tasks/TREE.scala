package de.secretj12
package tasks

/**
 * Define tree and with equals method
 */
abstract class TREE:
  def equals(t: Any): Boolean

/**
 * Implement node of the tree with the specified equals method
 */
class NODE(val trees: List[TREE]) extends TREE:
  override def equals(t: Any): Boolean = {
    t.isInstanceOf[NODE]
      && trees.corresponds(t.asInstanceOf[NODE].trees)((t1, t2) => t1.equals(t2))
  }
class ID(val id: String) extends TREE:
  override def equals(t: Any): Boolean = t.isInstanceOf[ID] && id.equals(t.asInstanceOf[ID].id)
