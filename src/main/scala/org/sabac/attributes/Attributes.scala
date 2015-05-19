package org.sabac.attributes

class Attributes(val attrs: Map[String, Any]) {

  def get(key: String): Option[Any] = {
    attrs.get(key)
  }
}
