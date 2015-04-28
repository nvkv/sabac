package org.sabac

case class Assertion(
  leftOperand: Attribute,
  rightOperand: Attribute, 
  operator: Operator);
