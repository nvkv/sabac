package org.sabac.policy

import org.sabac.attributes._

case class Rule(assertions: List[Assertion]) {

  trait AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes): Option[Any]
  }
  
  case class Obj(v: String) extends AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes) =
      obj.get(v)
  }

  case class Subj(v: String) extends AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes) = 
      subj.get(v)
  }

  case class Env(v: String) extends AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes) =
      env.get(v)
  }
  case class Value(v: String) extends AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes) =
      Some(v)
  }

  private def trimSource(attrName: String): String = 
    attrName.split("\\.").last

  private def sourceAttr(attr: String): AttributeSource = 
    attr match {
      case _ if attr.startsWith("subj") => Subj(trimSource(attr))
      case _ if attr.startsWith("obj") => Obj(trimSource(attr))
      case _ if attr.startsWith("env") => Env(trimSource(attr))
      case _ => Value(attr)
    }

  def apply(subj: Attributes, obj: Attributes, env: Attributes): Result = 
    assertions.foldLeft(NotApplicable: Result) { (acc, assert) =>
      acc match {
        case Deny(m) => acc
        case _ => {
          val left = sourceAttr(assert.left).valueFromContext(subj, obj, env)
          val right = sourceAttr(assert.right).valueFromContext(subj, obj, env)
          if (left == None || right == None) 
            NotApplicable
          else
            assert(left, right)
        }
      }
    }
}


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
