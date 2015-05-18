package org.sabac

case class Assertion(left: String, predicate: String, right: String)

object Assertion {

  import PolicySchema._

  def fromMap(map: AssertionMap): Option[Assertion] = {
    map.toList match {
      case List((left, rightExpr)) => 
        rightExpr.toList match {
          case List((predicate, right)) => 
            Some(Assertion(left, predicate, right))
          case _ => None
        }
        case _ => None
    }
  }
}
