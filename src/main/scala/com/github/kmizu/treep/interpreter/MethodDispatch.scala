package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Runtime.Env
import com.github.kmizu.treep.interpreter.Values.*

object MethodDispatch:
  def dispatch(
    env: Env,
    name: String,
    recv: Value,
    args: List[Value],
    callFunc: (VFunc, List[Value]) => Value
  ): Value =
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
