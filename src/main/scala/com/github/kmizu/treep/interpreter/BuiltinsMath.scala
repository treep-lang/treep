package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Values.*

object BuiltinsMath:
  val names: Set[String] = Set("abs", "min", "max", "pow")

  def eval(name: String, args: List[Value]): Value = name match
    case "abs" => args match
      case List(VInt(n)) => VInt(math.abs(n))
      case _ => throw TypeMismatchError("Int", args.toString, Some("abs"))
    case "min" => args match
      case List(VInt(a), VInt(b)) => VInt(math.min(a, b))
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("min"))
    case "max" => args match
      case List(VInt(a), VInt(b)) => VInt(math.max(a, b))
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("max"))
    case "pow" => args match
      case List(VInt(base), VInt(exp)) =>
        if exp < 0 then throw InvalidOperation("pow", "negative exponent not supported for integers")
        else VInt(math.pow(base.toDouble, exp.toDouble).toInt)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("pow"))
