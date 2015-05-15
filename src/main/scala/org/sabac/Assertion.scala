package org.sabac

case class Assertion(left: String, predicate: String, right: String)

object Assertion {
  def fromMap(map: Map[String, Map[String, String]]): Option[Assertion] = {
    val left = map.keys.headOption
    val rightExp = map.getOrElse(left, Map())
    val predicate = rightExp.keys.headOption
    val right = rightExp.get(predicate)

    (left, predicate, right) match {
      case (Some(x), Some(y), Some(z)) => Some(Assertion(x, y, z))
      case _ => None
    }
  }
}
