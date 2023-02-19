package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

trait RegularLanguage:
  val language: String
case object Empty extends RegularLanguage:
  val language = ""
case object Epsilon extends RegularLanguage:
  val language = "epsilon"
case class Character(char: Char) extends RegularLanguage:
  val language = char.toString()
case class Union(lang1: RegularLanguage, lang2: RegularLanguage) extends RegularLanguage:
  val language = "(" ++ lang1.language ++ "|" ++ lang2.language ++ ")"
case class Concat(lang1: RegularLanguage, lang2: RegularLanguage) extends RegularLanguage:
  val language = "(" ++ lang1.language ++ lang2.language ++ ")"
case class Star(lang: RegularLanguage) extends RegularLanguage:
  val language = lang.language ++ "*"

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match
  case Concat(Epsilon, l) => simplify(l)
  case Concat(l, Epsilon) => simplify(l)
  case Concat(Empty, l) => Empty
  case Concat(l, Empty) => Empty
  case Concat(l1, l2) => Concat(simplify(l1), simplify(l2))
  case Union(Empty, l) => simplify(l)
  case Union(l, Empty) => simplify(l)
  case Union(l1, l2) => Union(simplify(l1), simplify(l2))
  case Star(Epsilon) => Epsilon
  case Star(Empty) => Empty
  case Star(l) => Star(simplify(l))
  case l => l

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = lang match
  case Epsilon => true
  case Star(l) => true
  case Concat(l1, l2) => nullable(l1) && nullable (l2)
  case Union(l1, l2) => nullable(l1) || nullable (l2)
  case _ => false


/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = l match
  case Empty => Empty
  case Epsilon => Empty
  case Character(d) => if c == d then Epsilon else Empty
  case Union(l1, l2) => Union(derivative(l1)(c), derivative(l2)(c))
  case Concat(l1, l2) => if !nullable(l1) then Concat(derivative(l1)(c), l2)
                          else Union(Concat(derivative(l1)(c), l2), derivative(l2)(c))
  case Star(l0) => Concat(derivative(l0)(c),Star(l0))

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
