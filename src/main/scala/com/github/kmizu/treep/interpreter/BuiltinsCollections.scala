package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Values.*

object BuiltinsCollections:
  val names: Set[String] = Set("push", "keys", "hasKey", "fst", "snd")

  def eval(name: String, args: List[Value]): Value = name match
    case "push" => args match
      case List(VList(xs), v) => VList(xs :+ v)
      case _ => throw TypeMismatchError("(List, value)", args.toString, Some("push"))
    case "keys" => args match
      case List(VDict(m)) => VList(m.keys.toList)
      case _ => throw TypeMismatchError("Dict", args.toString, Some("keys"))
    case "hasKey" => args match
      case List(VDict(m), k) => VBool(m.contains(k))
      case _ => VBool(false)
    case "fst" => args match
      case List(VTuple2(a, _)) => a
      case _ => throw TypeMismatchError("Tuple2", args.toString, Some("fst"))
    case "snd" => args match
      case List(VTuple2(_, b)) => b
      case _ => throw TypeMismatchError("Tuple2", args.toString, Some("snd"))
