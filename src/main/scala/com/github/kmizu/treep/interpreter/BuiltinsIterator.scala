package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Values.*

object BuiltinsIterator:
  val names: Set[String] = Set("iter", "hasNext", "next")

  def eval(name: String, args: List[Value]): Value = name match
    case "iter" => args match
      case List(VList(xs)) => VIter(xs, 0)
      case List(VDict(m)) => VIter(m.toList.map { case (k, v) => VTuple2(k, v) }, 0)
      case _ => throw TypeMismatchError("List or Dict", args.toString, Some("iter"))
    case "hasNext" => args match
      case List(it: VIter) => VBool(it.hasNext)
      case _ => VBool(false)
    case "next" => args match
      case List(it: VIter) => it.next()
      case _ => throw TypeMismatchError("Iterator", args.toString, Some("next"))
