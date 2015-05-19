package org.sabac.policy

case class Rule(assertions: List[Assertion])

object Rule {

  import PolicySchema._

  def fromMapAssertionsList(list: List[AssertionMap]): Option[Rule] = {
    val assertions = list map(Assertion.fromMap(_)) flatMap(a => a)
    if (assertions.length > 0) Some(new Rule(assertions.flatten)) else None
  }

  def fromList(list: List[RuleMap]): Option[List[Rule]] =
    Some(
      list map { 
        _.toList match {
          case List((_, assertions)) => fromMapAssertionsList(assertions) 
          case _ => None
        }
      } flatMap(a => a))
}
