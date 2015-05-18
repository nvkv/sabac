package org.sabac

case class Rule(assertions: List[Assertion])

object Rule {

  import PolicySchema._

  def fromMapAssertionsList(list: List[AssertionMap]): Option[Rule] = {
    val assertions = list map { amap: AssertionMap => 
      Assertion.fromMap(amap) 
    } flatMap(a => a)
    Some(new Rule(assertions))
  }

  def fromList(list: List[RuleMap]): Option[List[Rule]] =
    Some(
      list map { rmap => 
        rmap.toList match {
          case List((_, assertions: List[AssertionMap])) => 
            fromMapAssertionsList(assertions) 
          case _ => None
        }
      } flatMap(a => a))
}
