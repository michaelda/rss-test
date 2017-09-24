package org.michaelda.rss

import cats.implicits._
import org.michaelda.rss.CheckElement._
import org.michaelda.xml.Element
import org.specs2.mutable.SpecificationWithJUnit

class CheckElementTest extends SpecificationWithJUnit {

  "check label" >> {
    val (a, b) = (Element("a"), Element("b"))
    val check = checkLabel("a")

    check(a) must beEmpty
    check(b) must beEqualTo(List(UnexpectedLabel(b, expectedLabel = "a")))
  }

  "check text" >> {
    val (abc, xyz) = (Element("a", "abc"), Element("a", "xyz"))
    val reason = "it does not start with \"abc\""
    val check = checkText(_.startsWith("abc"), reason)

    check(abc) must beEmpty
    check(xyz) must beEqualTo(List(InvalidText(xyz, reason)))
  }

  "check label and text" >> {
    val (ax, bx, bz) = (Element("a", "x"), Element("b", "x"), Element("b", "z"))
    val check = checkLabel("a") |+| checkText("x")

    check(ax) must beEmpty

    check(bx) must beEqualTo(List(UnexpectedLabel(bx, expectedLabel = "a")))
    check(bz) must beEqualTo(List(UnexpectedLabel(bz, expectedLabel = "a"), UnexpectedText(bz, expectedText = "x")))
  }
}
