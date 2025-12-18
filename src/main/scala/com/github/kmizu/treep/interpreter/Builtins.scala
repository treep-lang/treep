package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Values.*
import com.github.kmizu.treep.interpreter.Runtime.Env

object Builtins:
  def evalCall(
    env: Env,
    name: String,
    args: List[Value],
    callFunc: (VFunc, List[Value]) => Value
  ): Value =
    if Set("+", "-", "*", "/", "%").contains(name) then
      evalArithmeticOp(name, args)
    else if Set("==", "!=", ">", ">=", "<", "<=", "&&", "||", "!").contains(name) then
      evalComparisonOp(name, args)
    else if Set("iter", "hasNext", "next").contains(name) then
      evalIteratorFunc(name, args)
    else if Set("print", "println").contains(name) then
      evalIOFunc(name, args)
    else if Set("push", "keys", "hasKey", "fst", "snd").contains(name) then
      evalCollectionFunc(name, args)
    else if Set("abs", "min", "max", "pow").contains(name) then
      evalMathFunc(name, args)
    else
      env.get(name) match
        case Some(fn: VFunc) => callFunc(fn, args)
        case _ => throw UndefinedFunction(name)

  def evalMethodCall(
    env: Env,
    name: String,
    recv: Value,
    args: List[Value],
    callFunc: (VFunc, List[Value]) => Value
  ): Value =
    (name, recv, args) match
      // String methods
      case ("split", VString(s), List(VString(delimiter))) =>
        VList(s.split(java.util.regex.Pattern.quote(delimiter), -1).toList.map(VString(_)))
      case ("length", VString(s), Nil) => VInt(s.length)
      case ("substring", VString(s), List(VInt(start), VInt(end))) =>
        val validStart = math.max(0, math.min(start, s.length))
        val validEnd = math.max(validStart, math.min(end, s.length))
        VString(s.substring(validStart, validEnd))
      case ("contains", VString(s), List(VString(substr))) => VBool(s.contains(substr))
      // List methods
      case ("push", VList(xs), List(v)) => VList(xs :+ v)
      case ("iter", VList(xs), Nil) => VIter(xs, 0)
      case ("length", VList(xs), Nil) => VInt(xs.length)
      case ("head", VList(xs), Nil) => xs.headOption.getOrElse(VUnit)
      case ("tail", VList(xs), Nil) => VList(if xs.nonEmpty then xs.tail else Nil)
      case ("append", VList(xs), List(v)) => VList(xs :+ v)
      case ("concat", VList(xs), List(VList(ys))) => VList(xs ++ ys)
      // Dict methods
      case ("keys", VDict(m), Nil) => VList(m.keys.toList)
      case ("get", VDict(m), List(k)) => m.getOrElse(k, VUnit)
      case ("iter", VDict(m), Nil) => VIter(m.toList.map { case (k, v) => VTuple2(k, v) }, 0)
      case ("size", VDict(m), Nil) => VInt(m.size)
      case ("values", VDict(m), Nil) => VList(m.values.toList)
      case ("entries", VDict(m), Nil) => VList(m.toList.map { case (k, v) => VTuple2(k, v) })
      case ("put", VDict(m), List(k, v)) => VDict(m.updated(k, v))
      case ("remove", VDict(m), List(k)) => VDict(m - k)
      case ("getOrElse", VDict(m), List(k, d)) => m.getOrElse(k, d)
      // Iter methods
      case ("hasNext", it: VIter, Nil) => VBool(it.hasNext)
      case ("next", it: VIter, Nil) => it.next()
      case ("toList", it: VIter, Nil) => VList(it.remaining)
      case _ =>
        env.extensions.find(_.methodName == name) match
          case Some(ext) => callFunc(ext.func, recv :: args)
          case None =>
            recv match
              case VDict(m) =>
                m.get(VString(name)) match
                  case Some(fn: VFunc) =>
                    val arity = fn.params.length
                    if arity == args.length + 1 then callFunc(fn, recv :: args)
                    else if arity == args.length then callFunc(fn, args)
                    else throw ArityMismatch(arity, args.length, Some(s"method $name"))
                  case _ =>
                    env.get(name) match
                      case Some(fn: VFunc) => callFunc(fn, recv :: args)
                      case _ => throw InvalidOperation(s"method call $name", "method not found")
              case _ =>
                env.get(name) match
                  case Some(fn: VFunc) => callFunc(fn, recv :: args)
                  case _ => throw InvalidOperation(s"method call $name", "method not found")

  private def evalArithmeticOp(name: String, args: List[Value]): Value = name match
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

  private def evalComparisonOp(name: String, args: List[Value]): Value = name match
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

  private def evalIteratorFunc(name: String, args: List[Value]): Value = name match
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

  private def evalIOFunc(name: String, args: List[Value]): Value = name match
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

  private def evalCollectionFunc(name: String, args: List[Value]): Value = name match
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

  private def evalMathFunc(name: String, args: List[Value]): Value = name match
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

  private def eqValue(a: Value, b: Value): Boolean = (a, b) match
    case (VInt(x), VInt(y)) => x == y
    case (VBool(x), VBool(y)) => x == y
    case (VString(x), VString(y)) => x == y
    case (VList(xs), VList(ys)) => xs.length == ys.length && xs.zip(ys).forall(eqValue.tupled)
    case (VDict(mx), VDict(my)) => mx.keySet == my.keySet && mx.forall { case (k, vx) => my.get(k).exists(eqValue(vx, _)) }
    case (VUnit, VUnit) => true
    case _ => false

