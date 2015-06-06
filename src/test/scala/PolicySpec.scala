import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.sabac.policy._
import org.sabac.attributes._
import scala.language.reflectiveCalls

class PolicySpec extends FlatSpec with Matchers {

  def fixture = new {
    val testPolicy = Policy.fromTestFile("/test-policy.yml") match {
      case Right(p) ⇒ p
      case _        ⇒ fail()
    }
  }

  "Policy" should "be created from YAML policy file" in {
    val policy = Policy.fromTestFile("/test-policy.yml")
    policy should matchPattern { case Right(_) ⇒ }
    val badPolicy = Policy.fromTestFile("no-such-file-lol")
    badPolicy should matchPattern { case Left(_) ⇒ }
  }

  it should "create valid policy from source" in {
    val policy = Policy.fromTestFile("/test-policy.yml")
    policy match {
      case Right(p) ⇒ {
        p.name should equal(Some("sexist"))
        p.rules should not be None
        val rules = p.rules.getOrElse(List())
        rules.length should equal(1)
        val assertions = rules.head.assertions
        assertions.length should equal(7)
        val act = assertions.head
        act.left should equal("subject.action")
        act.right should equal("view")
      }
      case Left(_) ⇒ fail()
    }
  }

  it should "parse different policy specifications" in {
    Policy.fromTestFile("/minimal-policy.yml") match {
      case Right(policy) ⇒ {
        policy.name should equal(Some("minimal"))
        policy.rules.getOrElse(List()).length should equal(1)
      }
      case _ ⇒ fail()
    }
  }

  it should "give some yelling about wrong Policy format" in {
    Policy.fromTestFile("/bad-policy.yml") match {
      case Right(policy) ⇒ {
        fail("Accepted bad policy")
      }
      case _ ⇒ assert(true)
    }
  }

  "Policy execution" should "hanle not applicable" in {
    val subj = new Attributes("action" -> "view")
    val obj = new Attributes("secLevel" -> 8)
    val env = new Attributes("weather" -> "clean")

    fixture.testPolicy(subj, obj, env) should matchPattern { case NotApplicable ⇒ }
  }

  it should "handle Allow" in {
    val env = new Attributes()
    val subj = new Attributes(
      "action" -> "view",
      "clearance" -> 11,
      "sex" -> "Female",
      "group" -> "G1",
      "hairColor" -> "Blond",
      "hair" -> "None")

    val obj = new Attributes("secLevel" -> 10)
    fixture.testPolicy(subj, obj, env) should matchPattern { case Allow ⇒ }
  }

  it should "handle Deny" in {
    val env = new Attributes()
    val obj = new Attributes("secLevel" -> 10)
    val subj = new Attributes(
      "action" -> "view",
      "clearance" -> 0,
      "sex" -> "Female",
      "group" -> "X1",
      "hairColor" -> "Blond",
      "hair" -> "None")

    fixture.testPolicy(subj, obj, env) should matchPattern { case Deny(_) ⇒ }

    val obj2 = new Attributes("secLevel" -> 0)
    fixture.testPolicy(subj, obj2, env) should matchPattern { case Deny(_) ⇒ }
  }
}

