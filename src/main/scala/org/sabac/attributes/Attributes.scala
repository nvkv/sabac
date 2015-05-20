package org.sabac.attributes

class Attributes(as: (String, Any)*) {

  val attrs = as.groupBy(_._1).map { case (k, v) => (k, v.map(_._2).last) }

  def get(key: String): Option[Any] = {
    attrs.get(key)
  }
}
