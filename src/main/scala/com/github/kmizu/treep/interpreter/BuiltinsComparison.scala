package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Values.*

object BuiltinsComparison:
  val names: Set[String] = Set("==", "!=", ">", ">=", "<", "<=", "&&", "||", "!")

  def eval(name: String, args: List[Value]): Value = name match
    case "==" => args match
      case List(a, b) => VBool(eqValue(a, b))
      case _ => throw ArityMismatch(2, args.length, Some("=="))
    case "!=" => args match
      case List(a, b) => VBool(!eqValue(a, b))
      case _ => throw ArityMismatch(2, args.length, Some("!="))
    case ">"  => args match
      case List(VInt(a), VInt(b)) => VBool(a > b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some(">"))
    case ">=" => args match
      case List(VInt(a), VInt(b)) => VBool(a >= b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some(">="))
    case "<"  => args match
      case List(VInt(a), VInt(b)) => VBool(a < b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("<"))
    case "<=" => args match
      case List(VInt(a), VInt(b)) => VBool(a <= b)
      case _ => throw TypeMismatchError("(Int, Int)", args.toString, Some("<="))
    case "&&" => args match
      case List(VBool(a), VBool(b)) => VBool(a && b)
      case _ => throw TypeMismatchError("(Bool, Bool)", args.toString, Some("&&"))
    case "||" => args match
      case List(VBool(a), VBool(b)) => VBool(a || b)
      case _ => throw TypeMismatchError("(Bool, Bool)", args.toString, Some("||"))
    case "!" => args match
      case List(VBool(b)) => VBool(!b)
      case _ => throw TypeMismatchError("Bool", args.toString, Some("!"))

  private def eqValue(a: Value, b: Value): Boolean = (a, b) match
    case (VInt(x), VInt(y)) => x == y
    case (VBool(x), VBool(y)) => x == y
    case (VString(x), VString(y)) => x == y
    case (VList(xs), VList(ys)) => xs.length == ys.length && xs.zip(ys).forall(eqValue.tupled)
    case (VDict(mx), VDict(my)) =>
      mx.keySet == my.keySet && mx.forall { case (k, vx) => my.get(k).exists(eqValue(vx, _)) }
    case (VUnit, VUnit) => true
    case _ => false
