import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.sabac.policy._
import org.sabac.attributes._
import scala.language.reflectiveCalls

class AssertionSpec extends FlatSpec with Matchers {
  
  "Assertions" should "be created from Assertion Map" in {
    val map = Map("subject.clearance" -> Map("is" -> 1))
    val assert = Assertion.fromMap(map)
    assert should matchPattern { case Some(_) => }
  }

  it should "handle 'is' predicate" in {
    val map = Map("" -> Map("is" -> ""))
    Assertion.fromMap(map) match {
      case Some(List(assertion)) => {
        assertion(1, 1) should matchPattern { case Allow => }
        assertion("platypus", "platypus") should matchPattern { case Allow => }
        assertion(None, None) should matchPattern { case Allow => }
        assertion("ducky", "platypus") should matchPattern { case Deny(_) => }
        assertion("1", 1) should matchPattern { case Deny(_) => }
      }
      case _ => fail()
    }
  }

  it should "handle comparation predicates" in {
    Assertion.fromMap(
      Map("" -> Map("less" ->""))) match {
        case Some(List(assertion)) => {
          assertion(1, 2) should equal(Allow)
          assertion(2, 1) should matchPattern { case Deny(_) => }
          assertion(1, 1) should matchPattern { case Deny(_) => }
          assertion("nothin special", 42) should matchPattern { case NotApplicable => }
          assertion(42, "something special") should matchPattern { case NotApplicable => }
        }
        case _ => fail()
      }

    Assertion.fromMap(
      Map("" -> Map("less-or-equal" ->""))) match {
        case Some(List(assertion)) => {
          assertion(1, 2) should matchPattern { case Allow => }
          assertion(2, 1) should matchPattern { case Deny(_) => }
          assertion(1, 1) should matchPattern { case Allow => }
          assertion("nothin special", 42) should matchPattern { case NotApplicable => }
          assertion(42, "something special") should matchPattern { case NotApplicable => }
        }
        case _ => fail()
      }

    Assertion.fromMap(
      Map("" -> Map("greater" ->""))) match {
        case Some(List(assertion)) => {
          assertion(1, 2) should matchPattern { case Deny(_) => }
          assertion(2, 1) should matchPattern { case Allow => }
          assertion(1, 1) should matchPattern { case Deny(_) => }
          assertion("nothin special", 42) should matchPattern { case NotApplicable => }
          assertion(42, "something special") should matchPattern { case NotApplicable => }
        }
        case _ => fail()
      }

    Assertion.fromMap(
      Map("" -> Map("greater-or-equal" ->""))) match {
        case Some(List(assertion)) => {
          assertion(1, 2) should matchPattern { case Deny(_) => }
          assertion(2, 1) should matchPattern { case Allow => }
          assertion(1, 1) should matchPattern { case Allow => }
          assertion("nothin special", 42) should matchPattern { case NotApplicable => }
          assertion(42, "something special") should matchPattern { case NotApplicable => }
        }
        case _ => fail()
      }
  }

  it should "handle 'in' predicate" in {
    (pending)
  }
}
