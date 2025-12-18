package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Values.*

object ValueFormatter:
  def formatValue(v: Value): String = v match
    case VUnit         => "()"
    case VInt(n)       => n.toString
    case VBool(b)      => b.toString
    case VString(s)    => s
    case VList(xs)     => xs.map(formatValue).mkString("[", ", ", "]")
    case VDict(m) =>
      val entries = m.toList.map { case (k, value) => s"${formatValue(k)}: ${formatValue(value)}" }
      entries.mkString("{", ", ", "}")
    case vIter: VIter  => s"<iter ${vIter.remaining.length} remaining>"
    case VTuple2(a, b) => s"(${formatValue(a)}, ${formatValue(b)})"
    case VFunc(_, _, _) => "<function>"

