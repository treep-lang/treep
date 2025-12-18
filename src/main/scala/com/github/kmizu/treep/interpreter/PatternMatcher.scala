package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.east.Element
import com.github.kmizu.treep.interpreter.Values.*

object PatternMatcher:
  def matchPattern(pat: Element, v: Value): Option[Map[String, Value]] = pat.kind match
    case "pint"  =>
      val expected = pat.getAttrOrElse("value", "0")
      Some(Map.empty).filter(_ => v match { case VInt(n) => n.toString == expected; case _ => false })
    case "pstr"  =>
      val expected = pat.getAttrOrElse("value", "")
      Some(Map.empty).filter(_ => v match { case VString(s) => s == expected; case _ => false })
    case "pbool" =>
      val expected = pat.getAttrOrElse("value", "false")
      Some(Map.empty).filter(_ => v match { case VBool(b) => b.toString == expected; case _ => false })
    case "pvar"  =>
      val varName = pat.name.getOrElse(throw MissingAttribute("name", "pattern variable"))
      Some(Map(varName -> v))
    case "pwild" => Some(Map.empty)
    case _        => None

