package org.sabac.policy

import java.util.ArrayList
import scala.collection.JavaConversions._

trait Assertion {
  val left: String
  val right: Any

  def apply(l: Any, r: Any): Result
}

case class Is(left: String, right: Any) extends Assertion {
  def apply(l: Any, r: Any): Result = {
    if (l == r)
      Allow
    else
      Deny(s"Is: Assertion failed: ${l} not ${r}")
  }
}

case class Compare(left: String, right: Any, predicate: Int ⇒ Boolean) extends Assertion {

  import java.lang.Number

  def apply(l: Any, r: Any): Result =
    (l, r) match {
      case (l: Number, r: Number) ⇒
        if (predicate(l.doubleValue compare r.doubleValue))
          Allow
        else
          Deny("Compare: Assertion failed")
      case _ ⇒ NotApplicable
    }
}

case class In(left: String, right: Any) extends Assertion {

  def apply(l: Any, r: Any): Result = {

    def check(what: Any, where: Seq[Any]): Result =
      if (where contains what) Allow
      else Deny(s"In: Assertion failed: ${what} not in ${where}")

    r match {
      case lst: ArrayList[_] ⇒ check(l, lst.toList)
      case lst: Seq[_]       ⇒ check(l, lst)
      case any               ⇒ Is(left, right)(l, any)
    }
  }
}

object UnknownPredicate extends Assertion {
  val left = ""
  val right = ""

  def apply(l: Any, r: Any): Result = NotApplicable
}

object Assertion {

  import PolicySchema._

  def fromMap(m: AssertionMap): Option[List[Assertion]] = {
    val assertions = m map {
      case (left, rightExpr) ⇒
        rightExpr.toList match {
          case List((predicate, right)) ⇒
            predicate match {
              case "is"               ⇒ Is(left, right)
              case "less"             ⇒ Compare(left, right, r ⇒ r < 0)
              case "less-or-equal"    ⇒ { Compare(left, right, r ⇒ r < 0 || r == 0) }
              case "greater"          ⇒ Compare(left, right, r ⇒ r > 0)
              case "greater-or-equal" ⇒ Compare(left, right, r ⇒ r == 0 || r > 0)
              case "in"               ⇒ In(left, right)
              case _                  ⇒ UnknownPredicate
            }
        }
    }
    Some(assertions.toList)
  }
}
