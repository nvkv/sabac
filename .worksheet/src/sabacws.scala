import org.yaml.snakeyaml.Yaml

object sabacws {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(139); 
  val yamlStr = """{ policy: minimal, rules: [ rule: { subject.action: {is: view} } ] }""";System.out.println("""yamlStr  : String = """ + $show(yamlStr ));$skip(24); 
  val yaml = new Yaml();System.out.println("""yaml  : org.yaml.snakeyaml.Yaml = """ + $show(yaml ));$skip(156); 
  val policyMap = yaml.load(yamlStr) match {
    case hashMap: java.util.HashMap[_, _] ⇒ Some(hashMap)
    case _                                ⇒ None
  };System.out.println("""policyMap  : Option[java.util.HashMap[_,_]] forSome { type _; type _ } = """ + $show(policyMap ))}
}
