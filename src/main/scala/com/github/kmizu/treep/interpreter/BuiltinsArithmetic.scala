package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Values.*

object BuiltinsArithmetic:
  val names: Set[String] = Set("+", "-", "*", "/", "%")

  def eval(name: String, args: List[Value]): Value = name match
    case "+" => args match
      case List(VInt(a), VInt(b)) => VInt(a + b)
      case List(VString(a), VString(b)) => VString(a + b)
      case _ => throw TypeMismatchError("(Int, Int) or (String, String)", args.toString, Some("+"))
    case "-" => args match
      case List(VInt(a), VInt(b)) => VInt(a - b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("-"))
    case "*" => args match
      case List(VInt(a), VInt(b)) => VInt(a * b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("*"))
    case "/" => args match
      case List(VInt(_), VInt(0)) => throw DivisionByZero()
      case List(VInt(a), VInt(b)) => VInt(a / b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("/"))
    case "%" => args match
      case List(VInt(_), VInt(0)) => throw DivisionByZero()
      case List(VInt(a), VInt(b)) => VInt(a % b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("%"))
