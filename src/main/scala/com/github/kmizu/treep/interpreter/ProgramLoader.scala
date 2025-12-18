package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.interpreter.Runtime.Env
import com.github.kmizu.treep.interpreter.Values.*

object ProgramLoader:
  def loadProgram(program: Element, evaluator: Evaluator): Env =
    var env = Env.empty

    def evalTop(el: Element): Unit = el.kind match
      case "def" =>
        val fname = el.name.getOrElse(throw MissingAttribute("name", "function definition"))
        val params = el.getAttr("params").filter(_.nonEmpty)
          .map(ParamParser.parseParamNames)
          .getOrElse(Nil)
        env.set(fname, VFunc(params, el.children.headOption.getOrElse(Element("block")), env))
      case "const" =>
        val cname = el.name.getOrElse(throw MissingAttribute("name", "const declaration"))
        val init = el.getChild("init").getOrElse(Element("unit"))
        val v = evaluator.evalExpr(env, init)
        env.set(cname, v)
      case "module" => el.children.foreach(evalTop)
      case "struct" => ()
      case _ => ()

    program.children.foreach(evalTop)
    env

