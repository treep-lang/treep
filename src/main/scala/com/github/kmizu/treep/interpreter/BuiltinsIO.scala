package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Values.*

object BuiltinsIO:
  val names: Set[String] = Set("print", "println")

  def eval(name: String, args: List[Value]): Value = name match
    case "print" => args match
      case List(value) =>
        scala.Predef.print(ValueFormatter.formatValue(value))
        VUnit
      case _ => throw ArityMismatch(1, args.length, Some("print"))
    case "println" => args match
      case List(value) =>
        scala.Predef.println(ValueFormatter.formatValue(value))
        VUnit
      case _ => throw ArityMismatch(1, args.length, Some("println"))
