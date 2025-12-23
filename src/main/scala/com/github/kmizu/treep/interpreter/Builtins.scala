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
    if BuiltinsArithmetic.names.contains(name) then
      BuiltinsArithmetic.eval(name, args)
    else if BuiltinsComparison.names.contains(name) then
      BuiltinsComparison.eval(name, args)
    else if BuiltinsIterator.names.contains(name) then
      BuiltinsIterator.eval(name, args)
    else if BuiltinsIO.names.contains(name) then
      BuiltinsIO.eval(name, args)
    else if BuiltinsCollections.names.contains(name) then
      BuiltinsCollections.eval(name, args)
    else if BuiltinsMath.names.contains(name) then
      BuiltinsMath.eval(name, args)
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
    BuiltinsMethods.eval(name, recv, args) match
      case Some(value) => value
      case None =>
        MethodDispatch.dispatch(env, name, recv, args, callFunc)
