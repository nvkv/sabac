package org.sabac.policy

import org.yaml.snakeyaml.Yaml
import java.util.{ArrayList, LinkedHashMap}
import java.net.URL
import scala.collection._
import scala.io.Source
import scala.collection.JavaConverters._
import scala.language.existentials


object PolicySchema {
  type RuleMap = Map[String, List[AssertionMap]]
  type AssertionMap = Map[String, AssertionPredicate]
  type AssertionPredicate = Map[String, String]
}


class Policy(policyMap: Map[String, Any]) {

  import PolicySchema._

  val name = policyMap.get("policy").asInstanceOf[Option[String]]
  val rules = extractRules(policyMap)
  
  private def extractRules(map: Map[String, Any]): Option[List[Rule]] = 
    map.get("rules") match {
      case Some(array) => {
        type RawAssertion = LinkedHashMap[String, LinkedHashMap[String, String]]
        type RawRule = LinkedHashMap[String, ArrayList[RawAssertion]]
        type RawRules = ArrayList[RawRule]

        val rawRules = array.asInstanceOf[RawRules].asScala.toList.filter { m =>
          m.containsKey("rule")
        }

        val rules = rawRules.map(
            _.asScala.mapValues(
              _.asScala.toList.map(
                _.asScala.mapValues(_.asScala))))
        Rule.fromList(rules)
      }
      case None => None
    }
}


object Policy {

  /**
   * Create Policy object from URL 
   * */
  def fromUrl(policyUrl: URL): Either[String, Policy] = {
    if (policyUrl == null) {
      return Left("Policy file not found")
    }
    val yamlStr = Source.fromURL(policyUrl).mkString
    val yaml = new Yaml()
    val policyMap = yaml.load(yamlStr) match {
       case hashMap: java.util.HashMap[_, _] => Some(hashMap.asScala)
       case _ => None
    }
    policyMap match {
      case Some(map) => Right(new Policy(map.asInstanceOf[Map[String, Any]]))
      case None => Left("Bad policy file")
    }
  }

  /**
   * Create Policy object with bundled fixture
   */
  def fromTestFile(fileName: String): Either[String, Policy] = {
    val policyUrl = getClass.getResource(fileName)
    fromUrl(policyUrl)
  }
}
