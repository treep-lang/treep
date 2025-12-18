package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.interpreter.Runtime.*
import com.github.kmizu.treep.interpreter.Values.*

final class Evaluator:

  def callFunc(fn: VFunc, args: List[Value]): Value =
    if fn.params.length != args.length then
      throw ArityMismatch(fn.params.length, args.length)
    val callEnv0 = Env(scala.collection.mutable.Map.from(fn.env.scopes.head) :: fn.env.scopes.tail, fn.env.extensions)
    val callEnv = callEnv0.push()
    fn.params.zip(args).foreach { case (n, v) => callEnv.set(n, v) }
    try
      evalBlock(callEnv, fn.body)
      VUnit
    catch
      case Return(v) => v

  def evalBlock(env0: Env, block: Element): Unit =
    val env = env0.push()
    block.children.foreach { st => evalStmt(env, st) }

  def evalStmt(env: Env, st: Element): Unit = st.kind match
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
    case "expr" => st.children.headOption.foreach(evalExpr(env, _))
    case "if" => evalIf(env, st)
    case "while" => evalWhile(env, st)
    case "match" => evalMatch(env, st)
    case _ => ()

  def evalExpr(env: Env, e: Element): Value = e.kind match
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
    case "index"  => evalIndex(env, e)
    case "field"  => evalField(env, e)
    case "call"   => evalCallExpr(env, e)
    case "mcall"  => evalMethodCallExpr(env, e)
    case "lambda" => evalLambda(env, e)
    case "block" =>
      // Evaluate block as expression (needed for macro expansion)
      evalBlock(env, e)
      VUnit
    case _ => VUnit

  private def evalBlockInPlace(env: Env, block: Element): Unit =
    block.children.foreach { st => evalStmt(env, st) }

  private def evalIf(env: Env, st: Element): Unit =
    val cond = st.getChild("cond").getOrElse(throw MissingElement("cond", "if statement"))
    val c = asBool(evalExpr(env, cond))
    val blocks = st.children.filter(_.kind == "block")
    val thenBlk = blocks.headOption.getOrElse(Element("block"))
    val elseBlk = blocks.lift(1)
    if c then evalBlock(env, thenBlk) else elseBlk.foreach(evalBlock(env, _))

  private def evalWhile(env: Env, st: Element): Unit =
    val cond = st.getChild("cond").getOrElse(throw MissingElement("cond", "while loop"))
    val body = st.children.find(_.kind == "block").getOrElse(Element("block"))
    while asBool(evalExpr(env, cond)) do
      // do not push new scopes per-iteration (avoid OOM)
      evalBlockInPlace(env, body)

  private def evalMatch(env: Env, st: Element): Unit =
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

  private def evalIndex(env: Env, e: Element): Value =
    val t = e.getChild("target").getOrElse(throw MissingElement("target", "index expression"))
    val k = e.getChild("key").getOrElse(throw MissingElement("key", "index expression"))
    (evalExpr(env, t), evalExpr(env, k)) match
      case (VDict(m), key) => m.getOrElse(key, VUnit)
      case (VList(xs), VInt(i)) => xs.lift(i).getOrElse(VUnit)
      case (recvType, _) => throw IndexError(recvType.getClass.getSimpleName, "not indexable")

  private def evalField(env: Env, e: Element): Value =
    val t = e.children.headOption.getOrElse(throw MissingElement("target", "field access"))
    val n = e.getAttr("name").getOrElse(throw MissingAttribute("name", "field access"))
    evalExpr(env, Element("index", children = List(
      Element("target", children = List(t)),
      Element("key", children = List(Element("string", attrs = List(Attr("value", n)))))
    )))

  private def evalCallExpr(env: Env, e: Element): Value =
    val name = e.name.getOrElse(throw MissingAttribute("name", "function call"))
    if name == "=" then
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

  private def evalMethodCallExpr(env: Env, e: Element): Value =
    val name = e.name.getOrElse(throw MissingAttribute("name", "method call"))
    val recv = e.children.headOption.map(evalExpr(env, _)).getOrElse(throw MissingElement("receiver", "method call"))
    val args = e.children.tail.map(ch => evalExpr(env, ch))
    Builtins.evalMethodCall(env, name, recv, args, callFunc)

  private def evalLambda(env: Env, e: Element): Value =
    val body = e.children.headOption.getOrElse(Element("block"))
    val params = e.getAttr("params")
      .map(ParamParser.parseParamNames)
      .orElse(e.getAttr("param").map(a => List(a)))
      .getOrElse(Nil)
    VFunc(params, body, env)

  private def asBool(v: Value): Boolean = v match
    case VBool(b) => b
    case VInt(n)  => n != 0
    case VUnit    => false
    case _        => true

