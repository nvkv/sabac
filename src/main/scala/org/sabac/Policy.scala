package org.sabac

import org.yaml.snakeyaml.Yaml
import scala.collection._
import scala.io.Source
import scala.collection.JavaConverters._
import scala.language.existentials

object Policy {

  def from(fileName: String): Either[String, String] = {
    val policyUrl = getClass.getResource(fileName)
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
      case Some(map) => Right(yamlStr)
      case None => Left("Bad policy file")
    }
  }
}
