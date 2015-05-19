package org.sabac.policy

abstract class Result

object Allow extends Result

case class Deny(message: String) extends Result 

object NotApplicable extends Result



