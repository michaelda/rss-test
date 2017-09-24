package org.michaelda.xml

import scala.xml.{Elem, Null, Text, TopScope}

object Element {

  private def apply(label: String, textOpt: Option[String], children: List[Elem]): Elem =
    new Elem(null, label, Null, TopScope, minimizeEmpty = true, children ++ textOpt.map(Text.apply): _*)

  // ------------------------------------------------------------------------------------------------------------------
  // constructors
  // ------------------------------------------------------------------------------------------------------------------

  def apply(label: String): Elem = apply(label, None, Nil)

  def apply(label: String, text: String): Elem = apply(label, Some(text), Nil)

  def apply(label: String, children: Elem*): Elem = apply(label, None, children.toList)

  def apply(label: String, text: String, children: Elem*): Elem = apply(label, Some(text), children.toList)
}
