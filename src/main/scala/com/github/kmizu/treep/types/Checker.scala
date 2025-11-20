package com.github.kmizu.treep.types

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.east.ParamParser
import com.github.kmizu.treep.types.Type as T
import com.github.kmizu.treep.types.HM
import com.github.kmizu.treep.types.HM.applyTo

object Checker:
  final case class Diag(msg: String, path: List[String] = Nil)

  final case class Ctx(vars: Map[String, T], funs: Map[String, (List[T], T)])
  object Ctx:
    val empty: Ctx = Ctx(Map.empty, Map.empty)

  def check(prog: Element): List[Diag] =
    val diags = scala.collection.mutable.ListBuffer.empty[Diag]
    var tvCounter = -1000  // Type variable counter for argument type inference

    def parseType(s: String): Option[T] =
      s.trim match
        case "Int"    => Some(T.TInt)
        case "Bool"   => Some(T.TBool)
        case "String" => Some(T.TString)
        case "Unit"   => Some(T.TUnit)
        case t if t.startsWith("List[") && t.endsWith("]") =>
          parseType(t.stripPrefix("List[").stripSuffix("]")).map(T.TList(_))
        case t if t.startsWith("Dict[") && t.endsWith("]") =>
          val inner = t.stripPrefix("Dict[").stripSuffix("]")
          val parts = inner.split(",").map(_.trim).toList
          parts match
            case k :: v :: Nil =>
              for
                kt <- parseType(k)
                vt <- parseType(v)
              yield T.TDict(kt, vt)
            case _ => None
        case _ => None

    def addDiag(msg: String, path: List[String]): Unit = diags += Diag(msg, path)

    // Infer return type from function body by analyzing return statements
    def inferReturnTypeFromBody(fnBody: Element, paramTypes: List[T], paramNames: List[String]): Option[T] =
      val returns = scala.collection.mutable.ListBuffer.empty[Element]

      def collectReturns(e: Element): Unit =
        if e.kind == "return" then returns += e
        e.children.foreach(collectReturns)

      collectReturns(fnBody)

      if returns.isEmpty then
        Some(T.TUnit) // No return statement means Unit
      else
        // Try to infer types from return expressions
        val paramMap = paramNames.zip(paramTypes).toMap
        val returnTypes = returns.flatMap { ret =>
          ret.children.headOption.flatMap { expr =>
            tryInferSimpleExpr(expr, paramMap)
          }
        }

        if returnTypes.isEmpty then
          None // Cannot infer
        else if returnTypes.tail.forall(_ == returnTypes.head) then
          Some(returnTypes.head) // All returns have same type
        else
          None // Inconsistent return types

    // Simple expression type inference for literals and basic operations
    def tryInferSimpleExpr(e: Element, params: Map[String, T]): Option[T] = e.kind match
      case "int" => Some(T.TInt)
      case "bool" => Some(T.TBool)
      case "string" => Some(T.TString)
      case "var" =>
        // Check if it's a parameter
        e.name.flatMap(params.get)
      case "list" =>
        val elemTypes = e.children.flatMap(ch => tryInferSimpleExpr(ch, params))
        if elemTypes.nonEmpty && elemTypes.tail.forall(_ == elemTypes.head) then
          Some(T.TList(elemTypes.head))
        else
          None
      case "call" if e.name.contains("+") || e.name.contains("-") || e.name.contains("*") || e.name.contains("/") =>
        // Binary arithmetic operations return Int
        val argTypes = e.children.flatMap(ch => tryInferSimpleExpr(ch, params))
        if argTypes.forall(_ == T.TInt) then Some(T.TInt) else None
      case "call" if e.name.contains("==") || e.name.contains("!=") || e.name.contains("<") || e.name.contains(">") =>
        Some(T.TBool) // Comparison operations return Bool
      case "call" if e.name.contains("&&") || e.name.contains("||") =>
        Some(T.TBool) // Logical operations return Bool
      case _ => None // Cannot infer for complex expressions

    def unify(t1: T, t2: T, path: List[String]): Unit =
      Infer.unify(t1, t2) match
        case Left(err) => addDiag(s"type mismatch: ${t1} vs ${t2}", path)
        case Right(_) => ()

    def typeOfExpr(ctx: Ctx, e: Element, path: List[String]): T = e.kind match
      case "int"    => T.TInt
      case "bool"   => T.TBool
      case "string" => T.TString
      case "var"    => e.name.flatMap(ctx.vars.get).getOrElse(T.TVar(-1))
      case "list"   =>
        val ts = e.children.map(ch => typeOfExpr(ctx, ch, path :+ "list-elem"))
        ts.headOption match
          case Some(t0) => ts.tail.foreach(ti => unify(t0, ti, path :+ "list-elem")); T.TList(ts.headOption.getOrElse(T.TVar(-1)))
          case None     => T.TList(T.TVar(-1))
      case "dict"   =>
        val ks = scala.collection.mutable.ListBuffer.empty[T]
        val vs = scala.collection.mutable.ListBuffer.empty[T]
        e.children.foreach { pair =>
          val k = typeOfExpr(ctx, pair.children.headOption.getOrElse(Element("unit")), path :+ "dict-key")
          val v = typeOfExpr(ctx, pair.children.lift(1).getOrElse(Element("unit")), path :+ "dict-val")
          ks += k; vs += v
        }
        ks.headOption.foreach(k0 => ks.tail.foreach(k => unify(k0, k, path :+ "dict-key")))
        vs.headOption.foreach(v0 => vs.tail.foreach(v => unify(v0, v, path :+ "dict-val")))
        T.TDict(ks.headOption.getOrElse(T.TVar(-1)), vs.headOption.getOrElse(T.TVar(-1)))
      case "index"  =>
        val t = e.getChild("target").map(typeOfExpr(ctx, _, path :+ "index-target")).getOrElse(T.TVar(-1))
        val k = e.getChild("key").map(typeOfExpr(ctx, _, path :+ "index-key")).getOrElse(T.TVar(-1))
        t match
          case T.TDict(kk, vv) => unify(kk, k, path :+ "index"); vv
          case T.TList(v) => unify(k, T.TInt, path :+ "index"); v
          case _ => addDiag("index on non-dict/list", path); T.TVar(-1)
      case "call"   =>
        val name = e.name.getOrElse("?")
        val args = e.children.map(ch => typeOfExpr(ctx, ch, path :+ s"arg"))
        name match
          case "fst" =>
            val a = T.TVar(-101); val b = T.TVar(-102)
            unify(args.head, T.TTuple2(a, b), path)
            a
          case "snd" =>
            val a = T.TVar(-103); val b = T.TVar(-104)
            unify(args.head, T.TTuple2(a, b), path)
            b
          case ">" | ">=" | "<" | "<=" => args.foreach(unify(_, T.TInt, path)); T.TBool
          case "==" | "!=" =>
            if args.length == 2 then unify(args.head, args(1), path)
            T.TBool
          case "&&" | "||" => args.foreach(unify(_, T.TBool, path)); T.TBool
          case "iter" =>
            args.headOption match
              case Some(T.TList(elem)) => T.TVar(-1) // iterator abstract
              case Some(T.TDict(_, v)) => T.TVar(-1)
              case Some(other) => addDiag(s"iter expects list/dict, got ${other}", path); T.TVar(-1)
              case None => T.TVar(-1)
          case "hasNext" => T.TBool
          case "next" => T.TVar(-1)
          case "print" =>
            if args.length != 1 then addDiag("print expects exactly one argument", path)
            T.TUnit
          case "println" =>
            if args.length != 1 then addDiag("println expects exactly one argument", path)
            T.TUnit
          case other =>
            ctx.funs.get(other) match
              case Some((ps, ret)) =>
                if ps.length != args.length then addDiag(s"arity mismatch: ${other}", path)
                ps.zipAll(args, T.TVar(-1), T.TVar(-1)).foreach { case (p, a) => unify(p, a, path :+ other) }
                ret
              case None => addDiag(s"unknown function: ${other}", path); T.TVar(-1)
      case other => T.TVar(-1)

    def checkBlock(ctx0: Ctx, blk: Element, path: List[String], expectRet: Option[T]): Ctx =
      var ctx = ctx0.copy(vars = ctx0.vars)
      blk.children.foreach { st =>
        st.kind match
          case "let" =>
            val name = st.name.getOrElse("?")
            val initE = st.getChild("init").getOrElse(Element("unit"))
            val t = typeOfExpr(ctx, initE, path :+ s"let ${name}")
            ctx = ctx.copy(vars = ctx.vars + (name -> t))
          case "assign" =>
            val name = st.name.getOrElse("?")
            val rhs = st.children.headOption.getOrElse(Element("unit"))
            val t = typeOfExpr(ctx, rhs, path :+ s"assign ${name}")
            ctx.vars.get(name).foreach(unify(_, t, path :+ s"assign ${name}"))
          case "expr" => st.children.headOption.foreach(e => typeOfExpr(ctx, e, path))
          case "if" =>
            val cond = st.getChild("cond").getOrElse(Element("bool", attrs = List(Attr("value", "true"))))
            unify(typeOfExpr(ctx, cond, path :+ "if-cond"), T.TBool, path)
            val blocks = st.children.filter(_.kind=="block")
            blocks.foreach(b => checkBlock(ctx, b, path :+ "if-branch", expectRet))
          case "while" =>
            val cond = st.getChild("cond").getOrElse(Element("bool", attrs = List(Attr("value", "true"))))
            unify(typeOfExpr(ctx, cond, path :+ "while-cond"), T.TBool, path)
            val body = st.children.find(_.kind=="block").getOrElse(Element("block"))
            checkBlock(ctx, body, path :+ "while-body", None)
          case "return" =>
            val tv = st.children.headOption.map(e => typeOfExpr(ctx, e, path :+ "return")).getOrElse(T.TUnit)
            expectRet.foreach(er => unify(er, tv, path :+ "return"))
          case "match" =>
            val tgt = st.getChild("target").getOrElse(Element("unit"))
            val tt = typeOfExpr(ctx, tgt, path :+ "match-target")
            st.children.filter(_.kind=="case").foreach { cs =>
              val pat = cs.children.headOption.getOrElse(Element("pwild"))
              val body = cs.children.lift(1).getOrElse(Element("block"))
              // very shallow: verify literal pattern types match target
              pat.kind match
                case "pint"  => unify(tt, T.TInt, path :+ "case")
                case "pstr"  => unify(tt, T.TString, path :+ "case")
                case "pbool" => unify(tt, T.TBool, path :+ "case")
                case _ => ()
              checkBlock(ctx, body, path :+ "case-body", expectRet)
            }
          case _ => ()
      }
      ctx

    // Build function signatures from top-level defs
    val funSigs: Map[String, (List[T], T)] = prog.children.collect {
      case d if d.kind == "def" =>
        val name = d.name.getOrElse("?")
        // Parse parameters with optional type annotations
        val params: List[T] =
          d.getAttr("params").filter(_.nonEmpty)
            .map { pStr =>
              ParamParser.parseParamsWithOptionalTypes(pStr).map { case (paramName, optType) =>
                optType.flatMap(parseType).getOrElse {
                  // Allocate a fresh type variable for unannotated parameters
                  tvCounter -= 1
                  T.TVar(tvCounter)
                }
              }
            }.getOrElse(Nil)
        val ret: T = d.getAttr("returns").flatMap(parseType).getOrElse {
          // Allocate a fresh type variable for unannotated return type
          tvCounter -= 1
          T.TVar(tvCounter)
        }
        name -> (params -> ret)
    }.toMap

    val base = Ctx.empty.copy(funs = funSigs)
    // HM-based check for function bodies
    var hmEnv: Env = HM.builtinEnv
    // add function signatures to env
    funSigs.foreach { case (n, (ps, r)) => hmEnv = hmEnv.extend(n, Scheme(Set.empty, T.TFun(ps, r))) }

    def isNonExp(e: Element): Boolean = e.kind match
      case "int" | "bool" | "string" => true
      case "var" => true
      case "list" => e.children.forall(isNonExp)
      case "dict" => e.children.forall(p => p.children.headOption.forall(isNonExp))
      case _ => false

    def walkBlockHM(env0: Env, blk: Element, retT: T, fnName: String): (Env, Subst) =
      var env = env0
      var subst = Subst.empty
      blk.children.foreach { st =>
        st.kind match
          case "let" =>
            val initE = st.getChild("init").getOrElse(Element("unit"))
            HM.inferExpr(env, initE) match
              case Right(HM.Result(s, t)) =>
                subst = s.compose(subst)
                env = s.applyTo(env)
                val sch = if isNonExp(initE) then Infer.generalize(env, t) else Scheme(Set.empty, t)
                env = env.extend(st.name.getOrElse("?"), sch)
              case Left(_) => addDiag("type inference failed in let", List(s"def ${fnName}", "let"))
          case "assign" =>
            val rhs = st.children.headOption.getOrElse(Element("unit"))
            HM.inferExpr(env, rhs) match
              case Right(HM.Result(s, t)) =>
                subst = s.compose(subst)
                env = s.applyTo(env)
                st.name.foreach { varName =>
                  env.lookup(varName).foreach { sch =>
                    val tv = HM.inferExpr(env, Element("var", name = Some(varName))).toOption.get.ty
                    Infer.unify(t, tv) match
                      case Left(_) => addDiag("assignment type mismatch", List(s"def ${fnName}", "assign"))
                      case Right(us) =>
                        subst = us.compose(subst)
                        env = us.applyTo(env)
                  }
                }
              case Left(err) => addDiag(s"type inference failed in assign: ${err}", List(s"def ${fnName}", "assign"))
          case "expr" => st.children.headOption.foreach(e => HM.inferExpr(env, e)) // ignore result
          case "return" =>
            st.children.headOption match
              case None => Infer.unify(retT, T.TUnit) match
                case Left(_) => addDiag("return type mismatch", List(s"def ${fnName}", "return"))
                case Right(us) =>
                  subst = us.compose(subst)
              case Some(ex) =>
                HM.inferExpr(env, ex) match
                  case Right(HM.Result(s, t)) =>
                    subst = s.compose(subst)
                    env = s.applyTo(env)
                    Infer.unify(s.apply(retT), s.apply(t)) match
                      case Left(_) => addDiag("return type mismatch", List(s"def ${fnName}", "return"))
                      case Right(us) =>
                        subst = us.compose(subst)
                        env = us.applyTo(env)
                  case Left(err) => addDiag(s"return type inference failed: ${err}", List(s"def ${fnName}", "return"))
          case "if" =>
            val cond = st.getChild("cond").getOrElse(Element("bool", attrs = List(Attr("value", "true"))))
            HM.inferExpr(env, cond).foreach { case HM.Result(s, t) =>
              subst = s.compose(subst)
              env = s.applyTo(env); Infer.unify(t, T.TBool)
            }
            st.children.filter(_.kind=="block").foreach { b =>
              val (newEnv, newSubst) = walkBlockHM(env, b, retT, fnName)
              env = newEnv
              subst = newSubst.compose(subst)
            }
          case "while" =>
            val cond = st.getChild("cond").getOrElse(Element("bool", attrs = List(Attr("value", "true"))))
            HM.inferExpr(env, cond).foreach { case HM.Result(s, t) =>
              subst = s.compose(subst)
              env = s.applyTo(env); Infer.unify(t, T.TBool)
            }
            val body = st.children.find(_.kind=="block").getOrElse(Element("block"))
            val (newEnv, newSubst) = walkBlockHM(env, body, retT, fnName)
            env = newEnv
            subst = newSubst.compose(subst)
          case _ => ()
      }
      (env, subst)

    // Check defs and collect substitutions
    val inferredFunSigs = scala.collection.mutable.Map[String, (List[T], T)]()
    prog.children.foreach { ch =>
      ch.kind match
        case "def" =>
          val name = ch.name.getOrElse("?")
          // Use the inferred return type from funSigs
          val (paramTypes, retT) = funSigs.getOrElse(name, (Nil, T.TUnit))
          val paramNamesAndTypes: List[(String,T)] =
            ch.getAttr("params").filter(_.nonEmpty)
              .map { pStr =>
                val names = ParamParser.parseParamNames(pStr)
                names.zip(paramTypes)
              }.getOrElse(Nil)
          var env = hmEnv
          paramNamesAndTypes.foreach { case (n,t) => env = env.extend(n, Scheme(Set.empty, t)) }
          val body = ch.children.headOption.getOrElse(Element("block"))
          val (_, subst) = walkBlockHM(env, body, retT, name)
          // Apply substitution to parameter and return types
          val concreteParamTypes = paramTypes.map(subst.apply)
          val concreteRetT = subst.apply(retT)
          inferredFunSigs(name) = (concreteParamTypes, concreteRetT)
        case "const" =>
          val cname = ch.name.getOrElse("")
          val init = ch.getChild("init")
          init.foreach { e =>
            HM.inferExpr(hmEnv, e) match
              case Left(err) =>
                addDiag(s"type error in const init: ${err}", List(s"const ${cname}"))
              case Right(HM.Result(s, t)) =>
                val applied = s.apply(t)
                hmEnv = s.applyTo(hmEnv)
                val scheme =
                  if isNonExp(e) then Infer.generalize(hmEnv, applied)
                  else Scheme(Set.empty, applied)
                hmEnv = hmEnv.extend(cname, scheme)
          }
        case "extension" =>
          val receiverParam = ch.getAttr("receiver-param").getOrElse("self")
          val receiverTypeStr = ch.getAttr("receiver-type").getOrElse("?")
          parseType(receiverTypeStr) match
            case Some(receiverType) =>
              ch.children.foreach { methodDef =>
                if methodDef.kind == "def" then
                  val methodName = methodDef.name.getOrElse("?")
                  val retT: T = methodDef.getAttr("returns").flatMap(parseType).getOrElse(T.TUnit)
                  val params: List[(String,T)] =
                    methodDef.getAttr("params").filter(_.nonEmpty)
                      .map(pStr => ParamParser.parseParams(pStr).flatMap { case (n, tStr) =>
                        parseType(tStr).map(t => n -> t)
                      }).getOrElse(Nil)

                  // Create a function type: (receiverType, params...) -> retT
                  val fullParams = receiverType :: params.map(_._2)
                  val funcType = T.TFun(fullParams, retT)

                  // Extract type variables from receiver type for polymorphism
                  val typeVars = Types.ftv(receiverType)
                  val paramScheme = Scheme(typeVars, funcType)

                  val extMethod = ExtensionMethod(methodName, receiverParam, receiverType, paramScheme, retT)
                  hmEnv = hmEnv.addExtension(extMethod)

                  // Also type-check the method body
                  var methodEnv = hmEnv.extend(receiverParam, Scheme(Set.empty, receiverType))
                  params.foreach { case (n,t) => methodEnv = methodEnv.extend(n, Scheme(Set.empty, t)) }
                  val body = methodDef.children.headOption.getOrElse(Element("block"))
                  val (_, _) = walkBlockHM(methodEnv, body, retT, methodName)
              }
            case None =>
              addDiag(s"invalid receiver type: ${receiverTypeStr}", List("extension"))
        case _ => ()
    }

    // Update hmEnv with inferred function signatures
    inferredFunSigs.foreach { case (name, (paramTypes, retType)) =>
      hmEnv = hmEnv.extend(name, Scheme(Set.empty, T.TFun(paramTypes, retType)))
    }

    diags.toList