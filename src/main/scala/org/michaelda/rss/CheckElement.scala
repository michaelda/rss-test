package org.michaelda.rss

import scala.xml.Elem

object CheckElement {

  abstract class CheckFailure[A](a: A)

  type CheckResult[A] = List[CheckFailure[A]]

  type Check[A] = A => CheckResult[A]

  // ------------------------------------------------------------------------------------------------------------------
  // check element
  // ------------------------------------------------------------------------------------------------------------------

  case class UnexpectedLabel(e: Elem, expectedLabel: String) extends CheckFailure[Elem](e)

  def checkLabel(expectedLabel: String): Check[Elem] = e =>
    if (e.label == expectedLabel) Nil else List(UnexpectedLabel(e, expectedLabel))

  case class InvalidText(e: Elem, reason: String) extends CheckFailure[Elem](e)

  def checkText(pred: String => Boolean, reason: String): Check[Elem] = e =>
    if (pred(e.text)) Nil else List(InvalidText(e, reason))

  case class UnexpectedText(e: Elem, expectedText: String) extends CheckFailure[Elem](e)

  def checkText(expectedText: String): Check[Elem] = e =>
    if (e.text == expectedText) Nil else List(UnexpectedText(e, expectedText))
}