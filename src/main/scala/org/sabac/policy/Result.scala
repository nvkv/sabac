package org.sabac.policy

trait Result

object Allow extends Result
case class Deny(message: String) extends Result 
object NotApplicable extends Result



