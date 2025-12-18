package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.interpreter.Values.*
import com.github.kmizu.treep.interpreter.Runtime.*

object Interpreter:
  export Values.*
  export Runtime.{Env, RuntimeExtension}

  def run(program: Element): Unit =
    val env = loadProgram(program)
    // call main() if present
    env.get("main") match
      case Some(fn: VFunc) if fn.params.isEmpty =>
        try
          val res = callFunc(fn, Nil)
          res match
            case VInt(n) => println(s"[treep] exit ${n}")
            case VUnit   => println("[treep] ok")
            case _       => println("[treep] ok")
        catch
          case _: Return => println("[treep] returned unexpectedly")
      case _ => println("[treep] ok")

  def evalFunction(program: Element, name: String, args: List[Value] = Nil): Value =
    val env = loadProgram(program)
    env.get(name) match
      case Some(fn: VFunc) => callFunc(fn, args)
      case _ => throw UndefinedFunction(name)

  private def loadProgram(program: Element): Env =
    var env = Env.empty
    def evalTop(el: Element): Unit = el.kind match
      case "def" =>
        val fname = el.name.getOrElse(throw MissingAttribute("name", "function definition"))
        val params: List[String] =
          el.getAttr("params").filter(_.nonEmpty)
            .map(ParamParser.parseParamNames).getOrElse(Nil)
        env.set(fname, VFunc(params, el.children.headOption.getOrElse(Element("block")), env))
      case "const" =>
        val cname = el.name.getOrElse(throw MissingAttribute("name", "const declaration"))
        val init = el.getChild("init").getOrElse(Element("unit"))
        val v = evalExpr(env, init)
        env.set(cname, v)
      case "module" => el.children.foreach(evalTop)
      case "struct" => () // ignore for now
      case other => ()
    program.children.foreach(evalTop)
    env

  private def callFunc(fn: VFunc, args: List[Value]): Value =
    if fn.params.length != args.length then
      throw ArityMismatch(fn.params.length, args.length)
    var callEnv = Env(scala.collection.mutable.Map.from(fn.env.scopes.head) :: fn.env.scopes.tail, fn.env.extensions) // shallow copy head, preserve extensions
    callEnv = callEnv.push()
    fn.params.zip(args).foreach { case (n, v) => callEnv.set(n, v) }
    try
      evalBlock(callEnv, fn.body)
      VUnit
    catch
      case Return(v) => v

  private def evalBlock(env0: Env, block: Element): Unit =
    var env = env0.push()
    block.children.foreach { st => evalStmt(env, st) }
    ()

  // Evaluate statements without pushing a new scope (used for loop bodies)
  private def evalBlockInPlace(env: Env, block: Element): Unit =
    block.children.foreach { st => evalStmt(env, st) }

  private def evalStmt(env: Env, st: Element): Unit = st.kind match
    case "let" =>
      val name = st.name.getOrElse(throw MissingAttribute("name", "let statement"))
      val init = st.getChild("init").getOrElse(throw MissingElement("init", "let statement"))
      val v = evalExpr(env, init)
      env.set(name, v)
    case "assign" =>
      val name = st.name.getOrElse(throw MissingAttribute("name", "assignment"))
      val rhs = st.children.headOption.getOrElse(throw MissingElement("rhs", "assignment"))
      val v = evalExpr(env, rhs)
      env.assign(name, v)
    case "return" =>
      val v = st.children.headOption.map(evalExpr(env, _)).getOrElse(VUnit)
      throw Return(v)
    case "block" => evalBlock(env, st)
    case "expr" =>
      st.children.headOption.foreach(evalExpr(env, _))
    case "if" =>
      val cond = st.getChild("cond").getOrElse(throw MissingElement("cond", "if statement"))
      val c = asBool(evalExpr(env, cond))
      val others = st.children.filter(_.kind == "block")
      val thenBlk = others.headOption.getOrElse(Element("block"))
      val elseBlk = if others.size > 1 then Some(others(1)) else None
      if c then evalBlock(env, thenBlk) else elseBlk.foreach(evalBlock(env, _))
    case "while" =>
      val cond = st.getChild("cond").getOrElse(throw MissingElement("cond", "while loop"))
      val body = st.children.find(_.kind == "block").getOrElse(Element("block"))
      while asBool(evalExpr(env, cond)) do
        evalBlockInPlace(env, body)
    case "match" =>
      val tgt = st.getChild("target").getOrElse(throw MissingElement("target", "match statement"))
      val v = evalExpr(env, tgt)
      val cases = st.children.filter(_.kind == "case")
      val hit = cases.collectFirst { c =>
        PatternMatcher.matchPattern(c.children.head, v).map { binds => (c.children(1), binds) }
      }.flatten
      val chosen = hit.orElse {
        cases.find(_.children.headOption.exists(_.kind.trim == "pwild")).map(c => (c.children(1), Map.empty[String, Value]))
      }
      chosen match
        case Some((body, binds)) =>
          val env2 = env.push(); binds.foreach { case (n, vv) => env2.set(n, vv) }
          evalBlock(env2, body)
        case None => ()
    case _ => ()

  private def evalExpr(env: Env, e: Element): Value = e.kind match
    case "unit"   => VUnit
    case "int"    => VInt(e.getAttrOrElse("value", "0").toInt)
    case "bool"   => VBool(e.getAttr("value").contains("true"))
    case "string" => VString(e.getAttrOrElse("value", ""))
    case "var"    =>
      val varName = e.name.getOrElse(throw MissingAttribute("name", "variable reference"))
      env.get(varName).getOrElse(throw UndefinedVariable(varName))
    case "list"   => VList(e.children.map(ch => evalExpr(env, ch)))
    case "dict"   =>
      val pairs: Map[Value, Value] = e.children.map { p =>
        val k = evalExpr(env, p.children.head)
        val v = evalExpr(env, p.children(1))
        k -> v
      }.toMap
      VDict(pairs)
    case "index"  =>
      val t = e.getChild("target").getOrElse(throw MissingElement("target", "index expression"))
      val k = e.getChild("key").getOrElse(throw MissingElement("key", "index expression"))
      (evalExpr(env, t), evalExpr(env, k)) match
        case (VDict(m), key) => m.getOrElse(key, VUnit)
        case (VList(xs), VInt(i)) => xs.lift(i).getOrElse(VUnit)
        case (recvType, _) => throw IndexError(recvType.getClass.getSimpleName, "not indexable")
    case "field" =>
      val t = e.children.headOption.getOrElse(throw MissingElement("target", "field access"))
      val n = e.getAttr("name").getOrElse(throw MissingAttribute("name", "field access"))
      evalExpr(env, Element("index", children = List(Element("target", children = List(t)), Element("key", children = List(Element("string", attrs = List(Attr("value", n))))))))
    case "call"   =>
      val name = e.name.getOrElse(throw MissingAttribute("name", "function call"))
      if name == "=" then
        // assignment: left must be var; evaluate rhs then assign
        val lhs = e.children.headOption.getOrElse(throw MissingElement("lhs", "assignment"))
        val rhs = e.children.lift(1).getOrElse(throw MissingElement("rhs", "assignment"))
        val rhsV = evalExpr(env, rhs)
        lhs.kind match
          case "var" =>
            val varName = lhs.name.getOrElse(throw MissingAttribute("name", "assignment target"))
            env.assign(varName, rhsV)
            rhsV
          case _ => throw AssignmentError("assignment target must be a variable")
      else if name == "&&" then
        val l = evalExpr(env, e.children.head)
        if !asBool(l) then VBool(false) else VBool(asBool(evalExpr(env, e.children(1))))
      else if name == "||" then
        val l = evalExpr(env, e.children.head)
        if asBool(l) then VBool(true) else VBool(asBool(evalExpr(env, e.children(1))))
      else
        val args = e.children.map(ch => evalExpr(env, ch))
        Builtins.evalCall(env, name, args, callFunc)
    case "mcall" =>
      val name = e.name.getOrElse(throw MissingAttribute("name", "method call"))
      val recv = e.children.headOption.map(evalExpr(env, _)).getOrElse(throw MissingElement("receiver", "method call"))
      val args = e.children.tail.map(ch => evalExpr(env, ch))
      Builtins.evalMethodCall(env, name, recv, args, callFunc)
    case "lambda" =>
      val body = e.children.headOption.getOrElse(Element("block"))
      val params = e.getAttr("params")
        .map(ParamParser.parseParamNames)
        .orElse(e.getAttr("param").map(a => List(a)))
        .getOrElse(Nil)
      VFunc(params, body, env)
    case "block" =>
      // Evaluate block as expression (needed for macro expansion)
      evalBlock(env, e)
      VUnit
    case other => VUnit

  private def asBool(v: Value): Boolean = v match
    case VBool(b) => b
    case VInt(n)  => n != 0
    case VUnit    => false
    case _        => true
