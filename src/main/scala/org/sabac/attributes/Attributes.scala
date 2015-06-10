package org.sabac.attributes

class Attributes(as: (Any, Any)*) {

  val attrs = as.groupBy(_._1).map { case (k, v) ⇒ (k, v.map(_._2).last) }

  def get(key: Any): Option[Any] = {
    attrs.get(key)
  }

  def merge(other: Attributes): Attributes = {

    def flatten(ls: List[Any]): List[Any] = ls flatMap {
      case i: List[_] ⇒ flatten(i)
      case e          ⇒ List(e)
    }

    val mergedMap = ((this.attrs.keySet ++ other.attrs.keySet) map { i ⇒
      i -> (this.attrs.get(i).toList ::: other.attrs.get(i).toList)
    })
    val result = mergedMap.toMap.map {
      case (k, values) ⇒
        if (values.length == 1)
          (k, values.head)
        else
          (k, flatten(values))
    }
    new Attributes(result.toSeq: _*)
  }
}
