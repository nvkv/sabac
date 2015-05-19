package org.sabac.policy

trait Assertion {
  val left: String
  val right: String
  val predicate: String

  def apply(l: Any, r: Any): Result 
}


case class Is(left: String, right: String) extends Assertion {
  val predicate = "is"
  def apply(l: Any, r: Any): Result = if (l == r) Allow else Deny("Assertion failed")
}


object Assertion {

  import PolicySchema._

  def fromMap(m: AssertionMap): Option[List[Assertion]] = {
    val assertions =  m map { case (left, rightExpr) => 
      rightExpr.toList match { case List((predicate, right)) => 
        /** TODO: Create meaningful set of predicates here */
        Is(left, right)
      }
    }
    Some(assertions.toList)
  }
}
