package com.github.kmizu.treep.types

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.east.ParamParser
import com.github.kmizu.treep.types.Type as T
import java.util.concurrent.atomic.AtomicInteger

object HM:
  final case class Result(subst: Subst, ty: T)
  private type InferResult = Either[TypeError, Result]

  private val nextId = new AtomicInteger(1000)
  private def fresh(): T = T.TVar(nextId.incrementAndGet())
  private val nextRowId = new AtomicInteger(500000)
  private def freshRowId(): Int = nextRowId.incrementAndGet()

  type Recorder = (Element, T) => Unit
  private val NoOpRecorder: Recorder = (_, _) => ()

  final case class TypeTable(types: Map[Int, T], private val idIndex: java.util.IdentityHashMap[Element, Int]):
    def idOf(e: Element): Option[Int] = Option(idIndex.get(e))
    def typeOf(e: Element): Option[T] = idOf(e).flatMap(types.get)
    def entries: Map[Int, T] = types

  private final class TypeAnnotator:
    private val ids = new java.util.IdentityHashMap[Element, Int]()
    private var types: Map[Int, T] = Map.empty
    private var next: Int = 1

    def record(e: Element, t: T): Unit =
      val id = ids.computeIfAbsent(e, _ => { val id = next; next += 1; id })
      types = types.updated(id, t)

    def build(): TypeTable =
      TypeTable(types, ids.clone().asInstanceOf[java.util.IdentityHashMap[Element, Int]])

  private def inst(s: Scheme): T = Infer.instantiate(s, () => nextId.incrementAndGet())

  def builtinEnv: Env =
    BuiltinEnv.base

  def inferExpr(env: Env, e: Element): InferResult =
    given Recorder = NoOpRecorder
    inferExprInternal(env, e)

  def inferExprWithTypes(env: Env, e: Element): Either[TypeError, (Result, TypeTable)] =
    val annot = new TypeAnnotator
    given Recorder = (el: Element, t: T) => annot.record(el, t)
    inferExprInternal(env, e).map { res => (res, annot.build()) }

  private def inferExprInternal(env: Env, e: Element)(using rec: Recorder): InferResult =
    val out: InferResult = e.kind match
      case "int"    => Right(Result(Subst.empty, T.TInt))
      case "bool"   => Right(Result(Subst.empty, T.TBool))
      case "string" => Right(Result(Subst.empty, T.TString))
      case "var"    => inferVariable(env, e)
      case "dict"   => inferDict(env, e)
      case "index"  => inferIndexAccess(env, e)
      case "field"  => inferFieldAccess(env, e)
      case "lambda" => inferLambda(env, e)
      case "list"   => inferList(env, e)
      case "call"   => inferCall(env, e)
      case "mcall"  => inferMethodCall(env, e)
      case _        => Right(Result(Subst.empty, fresh()))
    out.map { res =>
      val finalType = res.subst.apply(res.ty)
      rec(e, finalType)
      res
    }

  private def inferVariable(env: Env, e: Element)(using Recorder): InferResult =
    e.name
      .flatMap(env.lookup)
      .map(inst)
      .toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
      .map(t => Result(Subst.empty, t))

  private def inferDict(env: Env, e: Element)(using Recorder): InferResult =
    def kvOf(pair: Element): Option[(Element, Element)] =
      if pair.children.length >= 2 then Some((pair.children.head, pair.children(1)))
      else
        pair.getAttr("key").map { keyValue =>
          val k = Element("string", attrs = List(Attr("value", keyValue)))
          val v = pair.children.headOption.getOrElse(Element("unit"))
          (k, v)
        }

    val init: Either[TypeError, (Subst, Option[T], Option[T])] = Right(Subst.empty, None, None)
    val res = e.children.foldLeft(init) { case (accE, pair) =>
      kvOf(pair) match
        case None => accE
        case Some((kexpr, vexpr)) =>
          for
            (sAcc, kOpt, vOpt) <- accE
            Result(sK, tK) <- inferExprInternal(sAcc.applyTo(env), kexpr)
            Result(sV, tV) <- inferExprInternal(sK.applyTo(env), vexpr)
            sK2 <- kOpt.map(kk => Infer.unify(sV.apply(kk), sV.apply(tK))).getOrElse(Right(sV))
            sV2 <- vOpt.map(vv => Infer.unify(sK2.apply(vv), sK2.apply(tV))).getOrElse(Right(sK2))
          yield (sV2, Some(sV2.apply(tK)), Some(sV2.apply(tV)))
    }

    res.map { case (s, kOpt, vOpt) => Result(s, T.TDict(kOpt.getOrElse(fresh()), vOpt.getOrElse(fresh()))) }

  private def inferIndexAccess(env: Env, e: Element)(using Recorder): InferResult =
    val tE = e.getChild("target").getOrElse(Element("unit"))
    val kE = e.getChild("key").getOrElse(Element("unit"))
    for
      Result(s1, tT) <- inferExprInternal(env, tE)
      Result(s2, tK) <- inferExprInternal(s1.applyTo(env), kE)
      tv = fresh()
      tryDict = Infer.unify(s2.apply(tT), T.TDict(T.TString, tv)).flatMap { sD => Infer.unify(sD.apply(tK), T.TString).map(sK => sK.compose(sD)) }
      res <- tryDict.orElse {
        for sL <- Infer.unify(s2.apply(tT), T.TList(tv)); sK <- Infer.unify(sL.apply(tK), T.TInt) yield sK.compose(sL)
      }
    yield Result(res.compose(s2).compose(s1), res.apply(tv))

  private def inferFieldAccess(env: Env, e: Element)(using Recorder): InferResult =
    val tgt = e.children.headOption.getOrElse(Element("unit"))
    val key = e.getAttrOrElse("name", "")
    for
      Result(s1, tT) <- inferExprInternal(env, tgt)
      tv = fresh()
      tryRec = Infer.unify(s1.apply(tT), T.TRecord(Map(key -> tv), Some(freshRowId()))).map(s => Result(s.compose(s1), s.apply(tv)))
      out <- tryRec.orElse {
        Infer.unify(s1.apply(tT), T.TDict(T.TString, tv)).map { s => Result(s.compose(s1), s.apply(tv)) }
      }
    yield out

  private def inferLambda(env: Env, e: Element)(using Recorder): InferResult =
    val body = e.children.headOption.getOrElse(Element("block"))
    e.getAttr("params") match
      case Some(sv) => inferTypedLambda(env, body, sv)
      case None     => inferSingleParamLambda(env, body, e)

  private def inferTypedLambda(env: Env, body: Element, params: String)(using Recorder): InferResult =
    val namesAndTypes: List[(String, T)] = ParamParser.parseParams(params).flatMap { case (n, tStr) =>
      parseTypeStr(tStr).map(tt => n -> tt)
    }
    val retVar = fresh()
    val envWithParams = namesAndTypes.foldLeft(env) { case (accEnv, (n, t)) =>
      accEnv.extend(n, Scheme(Set.empty, t))
    }
    inferBlockReturn(envWithParams, body, retVar) match
      case Left(err) => Left(err)
      case Right((s, found)) =>
        val s2 = if !found then Infer.unify(s.apply(retVar), s.apply(T.TUnit)).getOrElse(s) else s
        Right(Result(s2, T.TFun(namesAndTypes.map(nt => s2.apply(nt._2)), s2.apply(retVar))))

  private def inferSingleParamLambda(env: Env, body: Element, e: Element)(using Recorder): InferResult =
    val pName = e.getAttrOrElse("param", "_")
    val pTypeStr = e.getAttr("ptype")
    val pType = pTypeStr.flatMap(parseTypeStr).getOrElse(fresh())
    val retVar = fresh()
    val env1 = env.extend(pName, Scheme(Set.empty, pType))
    inferBlockReturn(env1, body, retVar) match
      case Left(err) => Left(err)
      case Right((s, found)) =>
        val s2 = if !found then Infer.unify(s.apply(retVar), s.apply(T.TUnit)).getOrElse(s) else s
        Right(Result(s2, T.TFun(List(s2.apply(pType)), s2.apply(retVar))))

  private def inferList(env: Env, e: Element)(using Recorder): InferResult =
    val init: Either[TypeError, (Subst, Option[T])] = Right(Subst.empty, None)
    val res = e.children.foldLeft(init) { case (accE, ch) =>
      for
        (sAcc, tOpt) <- accE
        Result(sE, tE) <- inferExprInternal(sAcc.applyTo(env), ch)
        sU <- tOpt.map(t0 => Infer.unify(sE.apply(t0), sE.apply(tE))).getOrElse(Right(sE))
      yield (sU, Some(sU.apply(tE)))
    }
    res.map { case (s, tOpt) => Result(s, T.TList(tOpt.getOrElse(fresh()))) }

  private def inferCall(env: Env, e: Element)(using Recorder): InferResult =
    val name = e.name.getOrElse("?")
    if name == "iter" then
      inferIteratorCall(env, e.children.headOption)
    else
      inferRegularCall(env, name, e.children)

  private def inferIteratorCall(env: Env, argOpt: Option[Element])(using Recorder): InferResult =
    argOpt match
      case None => Left(TypeError.Mismatch(T.TVar(-1), T.TUnit))
      case Some(argE) =>
        for
          Result(s1, tArg) <- inferExprInternal(env, argE)
          a = fresh(); k = fresh(); v = fresh()
          tryList = Infer.unify(s1.apply(tArg), T.TList(a)).map(s => Result(s, T.TIter(s.apply(a))))
          tryDict = Infer.unify(s1.apply(tArg), T.TDict(k, v)).map(s => Result(s, T.TIter(T.TTuple2(s.apply(k), s.apply(v)))))
          out <- tryList.orElse(tryDict).left.map(_ => TypeError.Mismatch(T.TList(fresh()), tArg))
        yield out

  private def inferRegularCall(env: Env, name: String, args: List[Element])(using Recorder): InferResult =
    inferArgs(env, args).flatMap { case (sArgs, tArgs) =>
      env.lookup(name).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2))).flatMap { fT =>
        val beta = fresh()
        Infer.unify(sArgs.apply(fT), T.TFun(tArgs.map(sArgs.apply), beta)).map { sFn =>
          Result(sFn.compose(sArgs), sFn.apply(beta))
        }
      }
    }

  private def inferArgs(env: Env, args: List[Element], initial: Subst = Subst.empty)(using Recorder): Either[TypeError, (Subst, List[T])] =
    val init: Either[TypeError, (Subst, List[T])] = Right((initial, Nil))
    args.foldLeft(init) { (accE, ex) =>
      for
        (sAcc, ts) <- accE
        Result(sX, tX) <- inferExprInternal(sAcc.applyTo(env), ex)
      yield (sX.compose(sAcc), ts :+ tX)
    }

  private def inferMethodCall(env: Env, e: Element)(using Recorder): InferResult =
    val name = e.name.getOrElse("?")
    val recvE = e.children.headOption.getOrElse(Element("unit"))
    val argsE = e.children.tail
    for
      Result(sRecv, tRecv) <- inferExprInternal(env, recvE)
      out <- inferMethodWithArgs(env, name, argsE, sRecv, tRecv)
    yield out

  private def inferMethodWithArgs(env: Env, name: String, argsE: List[Element], subst: Subst, recvType: T)(using Recorder): InferResult =
    inferArgs(env, argsE, subst).flatMap { case (sArgs, argTypes) =>
      name match
        case "push" | "append" => inferListAppendLike(sArgs, recvType, argTypes)
        case "concat"          => inferListConcat(sArgs, recvType, argTypes)
        case "length"          => inferLength(sArgs, recvType)
        case "head"            => inferListHead(sArgs, recvType)
        case "tail"            => inferListTail(sArgs, recvType)
        case "iter"            => inferIterableIterator(sArgs, recvType)
        case "size"            => inferDictSize(sArgs, recvType)
        case "hasKey"          => inferDictHasKey(sArgs, recvType, argTypes)
        case "keys"            => inferDictKeys(sArgs, recvType)
        case "values"          => inferDictValues(sArgs, recvType)
        case "entries"         => inferDictEntries(sArgs, recvType)
        case "get"             => inferDictGet(sArgs, recvType, argTypes)
        case "getOrElse"       => inferDictGetOrElse(sArgs, recvType, argTypes)
        case "put"             => inferDictPut(sArgs, recvType, argTypes)
        case "remove"          => inferDictRemove(sArgs, recvType, argTypes)
        case "hasNext"         => inferIteratorHasNext(sArgs, recvType)
        case "next"            => inferIteratorNext(sArgs, recvType)
        case "toList"          => inferIteratorToList(sArgs, recvType)
        case "split"           => inferStringSplit(sArgs, recvType, argTypes)
        case "substring"       => inferStringSubstring(sArgs, recvType, argTypes)
        case "contains"        => inferStringContains(sArgs, recvType, argTypes)
        case "join"            => inferListJoin(sArgs, recvType, argTypes)
        case _                 => inferMethodFallback(env, name, sArgs, recvType, argTypes)
    }

  private def withList(subst: Subst, recv: T)(f: (Subst, T) => InferResult): InferResult =
    val a = fresh()
    Infer.unify(subst.apply(recv), T.TList(a)).flatMap { sList =>
      f(sList, sList.apply(a))
    }

  private def withDict(subst: Subst, recv: T)(f: (Subst, T, T) => InferResult): InferResult =
    val k = fresh()
    val v = fresh()
    Infer.unify(subst.apply(recv), T.TDict(k, v)).flatMap { sDict =>
      f(sDict, sDict.apply(k), sDict.apply(v))
    }

  private def withString(subst: Subst, recv: T)(f: Subst => InferResult): InferResult =
    Infer.unify(subst.apply(recv), T.TString).flatMap(f)

  private def inferListAppendLike(subst: Subst, recv: T, args: List[T]): InferResult =
    withList(subst, recv) { (sList, elemType) =>
      unifyOptionalArg(sList, args, 0, elemType).map { sVal =>
        Result(sVal, T.TList(sVal.apply(elemType)))
      }
    }

  private def inferListConcat(subst: Subst, recv: T, args: List[T]): InferResult =
    withList(subst, recv) { (sList, elemType) =>
      val listType = T.TList(elemType)
      unifyOptionalArg(sList, args, 0, listType).map { sArg =>
        Result(sArg, T.TList(sArg.apply(elemType)))
      }
    }

  private def inferLength(subst: Subst, recv: T): InferResult =
    val tryList = withList(subst, recv) { (sList, _) => Right(Result(sList, T.TInt)) }
    tryList.orElse(withString(subst, recv)(s => Right(Result(s, T.TInt))))

  private def inferListHead(subst: Subst, recv: T): InferResult =
    withList(subst, recv) { (sList, elemType) => Right(Result(sList, elemType)) }

  private def inferListTail(subst: Subst, recv: T): InferResult =
    withList(subst, recv) { (sList, elemType) => Right(Result(sList, T.TList(elemType))) }

  private def inferIterableIterator(subst: Subst, recv: T): InferResult =
    val listResult = withList(subst, recv) { (sList, elemType) => Right(Result(sList, T.TIter(elemType))) }
    val dictResult = withDict(subst, recv) { (sDict, keyType, valueType) =>
      Right(Result(sDict, T.TIter(T.TTuple2(keyType, valueType))))
    }
    listResult.orElse(dictResult)

  private def inferDictSize(subst: Subst, recv: T): InferResult =
    withDict(subst, recv) { (sDict, _, _) => Right(Result(sDict, T.TInt)) }

  private def inferDictHasKey(subst: Subst, recv: T, args: List[T]): InferResult =
    withDict(subst, recv) { (sDict, keyType, _) =>
      unifyOptionalArg(sDict, args, 0, keyType).map(sKey => Result(sKey, T.TBool))
    }

  private def inferDictKeys(subst: Subst, recv: T): InferResult =
    withDict(subst, recv) { (sDict, keyType, _) => Right(Result(sDict, T.TList(keyType))) }

  private def inferDictValues(subst: Subst, recv: T): InferResult =
    withDict(subst, recv) { (sDict, _, valueType) => Right(Result(sDict, T.TList(valueType))) }

  private def inferDictEntries(subst: Subst, recv: T): InferResult =
    withDict(subst, recv) { (sDict, keyType, valueType) =>
      Right(Result(sDict, T.TList(T.TTuple2(keyType, valueType))))
    }

  private def inferDictGet(subst: Subst, recv: T, args: List[T]): InferResult =
    withDict(subst, recv) { (sDict, keyType, valueType) =>
      unifyOptionalArg(sDict, args, 0, keyType).map { sKey =>
        Result(sKey, sKey.apply(valueType))
      }
    }

  private def inferDictGetOrElse(subst: Subst, recv: T, args: List[T]): InferResult =
    withDict(subst, recv) { (sDict, keyType, valueType) =>
      for
        sKey <- unifyOptionalArg(sDict, args, 0, keyType)
        sDef <- unifyOptionalArg(sKey, args, 1, valueType)
      yield Result(sDef, sDef.apply(valueType))
    }

  private def inferDictPut(subst: Subst, recv: T, args: List[T]): InferResult =
    withDict(subst, recv) { (sDict, keyType, valueType) =>
      for
        sKey <- unifyOptionalArg(sDict, args, 0, keyType)
        sVal <- unifyOptionalArg(sKey, args, 1, valueType)
      yield Result(sVal, T.TDict(sVal.apply(keyType), sVal.apply(valueType)))
    }

  private def inferDictRemove(subst: Subst, recv: T, args: List[T]): InferResult =
    withDict(subst, recv) { (sDict, keyType, valueType) =>
      unifyOptionalArg(sDict, args, 0, keyType).map { sKey =>
        Result(sKey, T.TDict(sKey.apply(keyType), sKey.apply(valueType)))
      }
    }

  private def inferIteratorHasNext(subst: Subst, recv: T): InferResult =
    val a = fresh()
    for sIter <- Infer.unify(subst.apply(recv), T.TIter(a)) yield Result(sIter, T.TBool)

  private def inferIteratorNext(subst: Subst, recv: T): InferResult =
    val a = fresh()
    for sIter <- Infer.unify(subst.apply(recv), T.TIter(a)) yield Result(sIter, sIter.apply(a))

  private def inferIteratorToList(subst: Subst, recv: T): InferResult =
    val a = fresh()
    for sIter <- Infer.unify(subst.apply(recv), T.TIter(a)) yield Result(sIter, T.TList(sIter.apply(a)))

  private def inferStringSplit(subst: Subst, recv: T, args: List[T]): InferResult =
    withString(subst, recv) { sString =>
      unifyOptionalArg(sString, args, 0, T.TString).map { sDelim =>
        Result(sDelim, T.TList(T.TString))
      }
    }

  private def inferStringSubstring(subst: Subst, recv: T, args: List[T]): InferResult =
    withString(subst, recv) { sString =>
      for
        sStart <- unifyOptionalArg(sString, args, 0, T.TInt)
        sEnd <- unifyOptionalArg(sStart, args, 1, T.TInt)
      yield Result(sEnd, T.TString)
    }

  private def inferStringContains(subst: Subst, recv: T, args: List[T]): InferResult =
    withString(subst, recv) { sString =>
      unifyOptionalArg(sString, args, 0, T.TString).map { sNeedle =>
        Result(sNeedle, T.TBool)
      }
    }

  private def inferListJoin(subst: Subst, recv: T, args: List[T]): InferResult =
    withList(subst, recv) { (sList, _) =>
      unifyOptionalArg(sList, args, 0, T.TString).map { sDelim =>
        Result(sDelim, T.TString)
      }
    }

  private def inferMethodFallback(env: Env, name: String, subst: Subst, recvType: T, argTypes: List[T]): InferResult =
    val tryExtension: Option[InferResult] = env.extensions.find(_.methodName == name).map { ext =>
      val extFuncType = inst(ext.paramScheme)
      extFuncType match
        case T.TFun(recvT :: paramTypes, retType) =>
          for
            sRecv <- Infer.unify(subst.apply(recvType), subst.apply(recvT))
            sArgs <- if paramTypes.length == argTypes.length then
              val init: Either[TypeError, Subst] = Right(sRecv)
              paramTypes.zip(argTypes).foldLeft(init) { case (accE, (pType, aType)) =>
                for
                  acc <- accE
                  s <- Infer.unify(acc.apply(pType), acc.apply(aType))
                yield s.compose(acc)
              }
            else
              Left(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
          yield Result(sArgs.compose(sRecv).compose(subst), sArgs.apply(retType))
        case _ => Left(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
    }

    tryExtension.getOrElse {
      val beta = fresh()
      val tryRecord = Infer.unify(subst.apply(recvType), T.TRecord(Map(name -> T.TFun(argTypes, beta)), Some(freshRowId()))).map { sRec =>
        Result(sRec.compose(subst), sRec.apply(beta))
      }
      tryRecord.orElse {
        val init: Either[TypeError, (Subst, List[T])] = Right((subst, recvType :: argTypes))
        for
          (sAll, allArgs) <- init
          fT <- env.lookup(name).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
          beta = fresh()
          sFn <- Infer.unify(sAll.apply(fT), T.TFun(allArgs.map(sAll.apply), beta))
        yield Result(sFn.compose(sAll), sFn.apply(beta))
      }
    }

  private def unifyOptionalArg(base: Subst, args: List[T], index: Int, expected: T): Either[TypeError, Subst] =
    args.lift(index).map(arg => Infer.unify(base.apply(arg), base.apply(expected))).getOrElse(Right(base))

  extension (s: Subst) def applyTo(env: Env): Env =
    Env(env.table.view.mapValues { sch =>
      sch.copy(body = s.apply(sch.body))
    }.toMap, env.extensions)

  private def parseTypeStr(s: String): Option[T] =
    def trim(s: String) = s.trim
    def parseAtom(s: String): Option[T] =
      if s.startsWith("List[") && s.endsWith("]") then parseTypeStr(s.substring(5, s.length-1)).map(T.TList(_))
      else if s.startsWith("Dict[") && s.endsWith("]") then
        val inner = s.substring(5, s.length-1)
        val parts = inner.split(",").toList.map(_.trim)
        parts match
          case a :: b :: Nil => for ta <- parseTypeStr(a); tb <- parseTypeStr(b) yield T.TDict(ta, tb)
          case _ => None
      else s match
        case "Int" => Some(T.TInt)
        case "Bool" => Some(T.TBool)
        case "String" => Some(T.TString)
        case "Unit" => Some(T.TUnit)
        case _ => None
    // parse right-assoc arrows
    def splitArrow(s: String): List[String] =
      var depth = 0
      val b = new StringBuilder
      val parts = scala.collection.mutable.ListBuffer.empty[String]
      var i = 0
      while i < s.length do
        val c = s.charAt(i)
        if c == '[' then depth += 1
        else if c == ']' then depth -= 1
        if depth == 0 && i+1 < s.length && s.substring(i, i+2) == "->" then
          parts += b.toString
          b.clear()
          i += 2
        else { b.append(c); i += 1 }
      parts += b.toString
      parts.toList.map(_.trim)
    val parts = splitArrow(s)
    parts match
      case a :: b :: Nil => for ta <- parseTypeStr(a); tb <- parseTypeStr(b) yield T.TFun(List(ta), tb)
      case single :: Nil => parseAtom(single)
      case a :: rest =>
        val right = rest.mkString("->")
        for ta <- parseTypeStr(a); tb <- parseTypeStr(right) yield T.TFun(List(ta), tb)
      case Nil => None

  // Infer returns in a block for lambda typing
  private def inferBlockReturn(env0: Env, blk: Element, retVar: T)(using Recorder): Either[TypeError, (Subst, Boolean)] =
    var env = env0
    var subst: Subst = Subst.empty
    var found = false

    def goExpr(e: Element): Either[TypeError, Result] = inferExprInternal(env, e)
    def goBlock(b: Element): Either[TypeError, (Subst, Boolean)] = inferBlockReturn(env, b, retVar)

    def goStmt(st: Element): Either[TypeError, (Subst, Boolean)] = st.kind match
      case "let" =>
        val initE = st.children.find(_.kind=="init").flatMap(_.children.headOption).get
        goExpr(initE).map { case Result(s, t) =>
          env = s.applyTo(env)
          env = env.extend(st.name.get, Scheme(Set.empty, s.apply(t)))
          (s, false)
        }
      case "assign" =>
        val rhs = st.children.head
        goExpr(rhs).map { case Result(s, _) => env = s.applyTo(env); (s, false) }
      case "expr" => goExpr(st.children.head).map { case Result(s, _) => env = s.applyTo(env); (s, false) }
      case "return" =>
        val exprOpt = st.children.headOption
        val retT: T = exprOpt match
          case None => T.TUnit
          case Some(ex) =>
            goExpr(ex) match
              case Left(err) => return Left(err)
              case Right(Result(s, t)) => env = s.applyTo(env); s.apply(t)
        Infer.unify(retVar, retT).map { s2 =>
          subst = s2.compose(subst)
          found = true
          (s2, true)
        }
      case "if" =>
        val cond = st.children.find(_.kind=="cond").flatMap(_.children.headOption).get
        val blocks = st.children.filter(_.kind=="block")
        val thenBlk = blocks.headOption.getOrElse(Element("block"))
        val elseBlk = blocks.lift(1)
        for
          Result(sC, tC) <- goExpr(cond)
          sBool <- Infer.unify(sC.apply(tC), T.TBool)
          _ = { env = sBool.applyTo(sC.applyTo(env)); subst = sBool.compose(subst) }
          rThen <- goBlock(thenBlk)
          rElse <- elseBlk.map(goBlock).getOrElse(Right((Subst.empty, false)))
        yield {
          subst = rElse._1.compose(rThen._1).compose(subst)
          found = found || rThen._2 || rElse._2
          (subst, found)
        }
      case "while" =>
        val cond = st.children.find(_.kind=="cond").flatMap(_.children.headOption).get
        for
          Result(sC, tC) <- goExpr(cond)
          sBool <- Infer.unify(sC.apply(tC), T.TBool)
        yield { env = sBool.applyTo(sC.applyTo(env)); subst = sBool.compose(subst); (subst, false) }
      case "match" =>
        val tgt = st.children.find(_.kind=="target").flatMap(_.children.headOption).get
        for
          Result(sT, _) <- goExpr(tgt)
          _ = { env = sT.applyTo(env); subst = sT.compose(subst) }
          res <- st.children.filter(_.kind=="case").foldLeft(Right((subst, false)): Either[TypeError,(Subst,Boolean)]) { (accE, cs) =>
            accE.flatMap { case (sAcc, fAcc) =>
              goBlock(cs.children(1)).map { case (sB, fB) =>
                (sB.compose(sAcc), fAcc || fB)
              }
            }
          }
        yield { subst = res._1; found = found || res._2; (subst, found) }
      case _ => Right((Subst.empty, false))

    blk.children.foldLeft(Right((Subst.empty, false)): Either[TypeError,(Subst,Boolean)]) { (accE, st) =>
      accE.flatMap { case (sAcc, fAcc) =>
        env = sAcc.applyTo(env)
        goStmt(st).map { case (sS, fS) => (sS.compose(sAcc), fAcc || fS) }
      }
    }
