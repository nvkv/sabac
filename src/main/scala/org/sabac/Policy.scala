package org.sabac

case class Policy(
  name: String, 
  author: Option[String], 
  rules: List[Rule]);
