import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.sabac.policy._
import org.sabac.attributes._

class PolicySpec extends FlatSpec with Matchers {

  "Policy" should "be created from YAML policy file" in {
    val policy = Policy.fromTestFile("/test-policy.yml")
    policy should matchPattern { case Right(_) => } 
    val badPolicy = Policy.fromTestFile("no-such-file-lol")
    badPolicy should matchPattern { case Left(_) => }
  }


  it should "create valid policy from source" in {
    val policy = Policy.fromTestFile("/test-policy.yml")
    policy match {
      case Right(p) => {
        p.name should equal(Some("sexist"))
        p.rules should not be None
        val rules = p.rules.getOrElse(List())
        rules.length should equal(1) 
        val assertions = rules.head.assertions
        assertions.length should equal(6)
        val act = assertions.head
        act.left should equal("subject.action")
        act.predicate should equal("is")
        act.right should equal("view")
      }
      case Left(_) => fail()
    }
  }

  it should "parse different policy specifications" in {
    Policy.fromTestFile("/minimal-policy.yml") match {
      case Right(policy) => {
        policy.name should equal(Some("minimal"))
        policy.rules.getOrElse(List()).length should equal (1)
      }
      case _ => fail()
    }
  }

  "Policy execution" should "be" in {
    val policy = Policy.fromTestFile("/test-policy.yml") match {
      case Right(p) => p
      case _ => fail()
    }

    val subj = new Attributes(Map("action" -> "view")) 
    val obj = new Attributes(Map("secLevel" -> 8))
    val env = new Attributes(Map("weather" -> "clean"))

    policy.apply(subj, obj, env) should matchPattern { case NotApplicable => }
  }
}

