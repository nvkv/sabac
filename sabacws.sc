import org.yaml.snakeyaml.Yaml

object sabacws {
  val yamlStr = """{ policy: minimal, rules: [ rule: { subject.action: {is: view} } ] }"""
                                                  //> yamlStr  : String = { policy: minimal, rules: [ rule: { subject.action: {is:
                                                  //|  view} } ] }
  val yaml = new Yaml()                           //> yaml  : org.yaml.snakeyaml.Yaml = Yaml:1917513796
  val policyMap = yaml.load(yamlStr) match {
    case hashMap: java.util.HashMap[_, _] ⇒ Some(hashMap)
    case _                                ⇒ None
  }                                               //> policyMap  : Option[java.util.HashMap[_,_]] forSome { type _; type _ } = Som
                                                  //| e({policy=minimal, rules=[{rule={subject.action={is=view}}}]})
}