import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.sabac.policy._
import org.sabac.attributes._
import scala.language.reflectiveCalls

class RuleSpec extends FlatSpec with Matchers {
  
  def fixture = new {
  }

  "Rule" should "be created from List of RuleMaps" in {
    val rulemap = Map(
      "rule" -> List(
        Map("42" -> Map("is" -> "42"))))

    Rule.fromList(List(rulemap)) should matchPattern {
      case Some(List(Rule(List(Is("42","42"))))) =>
    }
  }

  it should "be able to work with empty list of assertions" in {
    new Rule(List()) should not be null
  }

  it should "be have apply method" in {    
    new Rule(List())(
      new Attributes(),
      new Attributes(),
      new Attributes) should matchPattern { case NotApplicable => }
  }

  "Rule execution" should "return Deny if any assertion returns deny" in {
    val rule = new Rule(List(Is("subject.name", "Vasiliy")))
    rule(
      new Attributes("name" -> "Innokentiy"),
      new Attributes(),
      new Attributes()) should matchPattern { case Deny(_) => }
  }

  it should "be able to see difference between prefixed values and attribute references" in {
    val rule = new Rule(List(Is("subject.name", "subject")))
    rule(
      new Attributes("name" -> "Abraham"),
      new Attributes(),
      new Attributes()) should matchPattern { case Deny(_) => }
  }

  it should "return Not Applicable if any assertion say so" in {
    val rule = new Rule(List(Compare("subject.name", 42, r => r > 0)))
    rule(
      new Attributes("name" -> "Innokentiy"),
      new Attributes(),
      new Attributes()) should matchPattern { case NotApplicable => }
  }
}
