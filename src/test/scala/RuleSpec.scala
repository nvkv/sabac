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
    (pending)
  }
}
