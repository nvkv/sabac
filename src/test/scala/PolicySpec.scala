import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.sabac._

class PolicySpec extends FlatSpec with Matchers {

  "Policy" should "be created from YAML policy file" in {
    val policy = Policy.from("/test-policy.yml")
    policy should matchPattern { case Right(_) => } 

     policy match {
      case Right(p) => {
        p.name should equal(Some("sexist"))
        p.rules should not be None
        val rules = p.rules.getOrElse(List())
        rules.length should equal(1) 
        println(rules)
        rules.head.assertions.length should equal(6)
      }
      case _ => fail()
    }

    val badPolicy = Policy.from("no-such-file-lol")
    badPolicy should matchPattern { case Left(_) => }
  }
}
