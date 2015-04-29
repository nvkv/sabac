import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.sabac._

class PolicySpec extends FlatSpec with Matchers {

  "Policy" should "be created from YAML policy file" in {
    val policy = Policy.from("/test-policy.yml")
    policy should matchPattern { case Right(_) => } 

    val badPolicy = Policy.from("no-such-file-lol")
    badPolicy should matchPattern { case Left(_) => }
  }
}
