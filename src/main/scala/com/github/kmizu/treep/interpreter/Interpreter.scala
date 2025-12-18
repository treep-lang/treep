package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.east.Element
import com.github.kmizu.treep.interpreter.Runtime.*
import com.github.kmizu.treep.interpreter.Values.*

object Interpreter:
  export Values.*
  export Runtime.{Env, RuntimeExtension}

  private val evaluator = new Evaluator

  def run(program: Element): Unit =
    val env = ProgramLoader.loadProgram(program, evaluator)
    env.get("main") match
      case Some(fn: VFunc) if fn.params.isEmpty =>
        val res = evaluator.callFunc(fn, Nil)
        res match
          case VInt(n) => println(s"[treep] exit ${n}")
          case _       => println("[treep] ok")
      case _ => println("[treep] ok")

  def evalFunction(program: Element, name: String, args: List[Value] = Nil): Value =
    val env = ProgramLoader.loadProgram(program, evaluator)
    env.get(name) match
      case Some(fn: VFunc) => evaluator.callFunc(fn, args)
      case _ => throw UndefinedFunction(name)

