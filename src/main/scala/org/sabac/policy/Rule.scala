package org.sabac.policy

import org.sabac.attributes._

case class Rule(assertions: List[Assertion]) {

  private trait AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes): Option[Any]
  }

  private case class Obj(v: Any) extends AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes) = obj.get(v)
  }

  private case class Subj(v: Any) extends AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes) = subj.get(v)
  }

  private case class Env(v: Any) extends AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes) = env.get(v)
  }

  private case class Value(v: Any) extends AttributeSource {
    def valueFromContext(subj: Attributes, obj: Attributes, env: Attributes) = Some(v)
  }

  private def trimSource(attrName: String): String = attrName.split("\\.").last

  private def sourceAttr(attr: Any): AttributeSource = attr match {
    case attr: String if attr.startsWith("subject.") ⇒ Subj(trimSource(attr))
    case attr: String if attr.startsWith("object.") ⇒ Obj(trimSource(attr))
    case attr: String if attr.startsWith("environment.") ⇒ Env(trimSource(attr))
    case _ ⇒ Value(attr)
  }

  def apply(subj: Attributes, obj: Attributes, env: Attributes): Result =
    assertions.foldLeft(NotApplicable: Result) { (acc, assert) ⇒
      acc match {
        case Deny(m) ⇒ acc
        case _ ⇒ {
          val left = sourceAttr(assert.left).valueFromContext(subj, obj, env)
          val right = sourceAttr(assert.right).valueFromContext(subj, obj, env)
          (left, right) match {
            case (Some(l), Some(r)) ⇒ assert(l, r)
            case _                  ⇒ NotApplicable
          }
        }
      }
    }
}

object Rule {

  import PolicySchema._

  private def fromMapAssertionsList(list: List[AssertionMap]): Option[Rule] = {
    val assertions = list map (Assertion.fromMap(_)) flatMap (a ⇒ a)
    if (assertions.length > 0) Some(new Rule(assertions.flatten)) else None
  }

  def fromList(list: List[RuleMap]): Option[List[Rule]] =
    Some(
      list map {
        _.toList match {
          case List((_, assertions)) ⇒ fromMapAssertionsList(assertions)
          case _                     ⇒ None
        }
      } flatMap (a ⇒ a))

}
