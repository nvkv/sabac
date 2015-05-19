package org.sabac.policy

case class Assertion(left: String, predicate: String, right: String)

object Assertion {

  import PolicySchema._

  def fromMap(m: AssertionMap): Option[List[Assertion]] = {
    val assertions =  m map { case (left, rightExpr) => 
      rightExpr.toList match { case List((predicate, right)) => 
        new Assertion(left, predicate, right)
      }
    }
    Some(assertions.toList)
  }
}
