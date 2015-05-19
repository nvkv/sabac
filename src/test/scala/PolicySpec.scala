import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.sabac.policy._

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
      case _ => fail()
    }
  }
}

