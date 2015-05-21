package org.sabac.attributes

class Attributes(as: (Any, Any)*) {

  val attrs = as.groupBy(_._1).map { case (k, v) => (k, v.map(_._2).last) }

  def get(key: Any): Option[Any] = {
    attrs.get(key)
  }
}
