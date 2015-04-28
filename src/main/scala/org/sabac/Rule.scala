package org.sabac

case class Rule(
  name: Option[String],
  assertions: List[Assertion]);
