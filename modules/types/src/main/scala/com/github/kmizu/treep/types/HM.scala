package com.github.kmizu.treep.types

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.types.Type as T

object HM:
  final case class Result(subst: Subst, ty: T)

  private var nextId: Int = 1000
  private def fresh(): T = { val id = nextId; nextId += 1; T.TVar(id) }

  private def inst(s: Scheme): T = Infer.instantiate(s, () => { val id = nextId; nextId += 1; id })

  def builtinEnv: Env =
    def scheme(t: T): Scheme = Scheme(Set.empty, t)
    val ops: Map[String, Scheme] = Map(
      "+" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "-" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "*" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "/" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "%" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      ">" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      ">="-> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "<" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "<="-> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "=="-> Scheme(Set(1), T.TFun(List(T.TVar(1), T.TVar(1)), T.TBool)),
      "!="-> Scheme(Set(2), T.TFun(List(T.TVar(2), T.TVar(2)), T.TBool)),
      // Collections
      "push" -> Scheme(Set(3), T.TFun(List(T.TList(T.TVar(3)), T.TVar(3)), T.TList(T.TVar(3)))),
      // dict helpers, fully polymorphic in key/value
      "keys" -> Scheme(Set(10,11), T.TFun(List(T.TDict(T.TVar(10), T.TVar(11))), T.TList(T.TVar(10)))),
      "hasKey" -> Scheme(Set(12,13), T.TFun(List(T.TDict(T.TVar(12), T.TVar(13)), T.TVar(12)), T.TBool)),
      // Iterators for lists and dicts
      "iter" -> Scheme(Set(14,15,16), T.TFun(List(T.TVar(14)), T.TVar(15))), // specialized in call branch
      "hasNext" -> Scheme(Set(17), T.TFun(List(T.TIter(T.TVar(17))), T.TBool)),
      "next" -> Scheme(Set(18), T.TFun(List(T.TIter(T.TVar(18))), T.TVar(18))),
      // tuple accessors
      "fst" -> Scheme(Set(19,20), T.TFun(List(T.TTuple2(T.TVar(19), T.TVar(20))), T.TVar(19))),
      "snd" -> Scheme(Set(21,22), T.TFun(List(T.TTuple2(T.TVar(21), T.TVar(22))), T.TVar(22)))
    )
    Env(ops)

  def inferExpr(env: Env, e: Element): Either[TypeError, Result] = e.kind match
    case "int"    => Right(Result(Subst.empty, T.TInt))
    case "bool"   => Right(Result(Subst.empty, T.TBool))
    case "string" => Right(Result(Subst.empty, T.TString))
    case "var"    => env.lookup(e.name.get).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2))).map(t => Result(Subst.empty, t))
    case "dict"   =>
      // unify keys and values across pairs
      val init: Either[TypeError, (Subst, Option[T], Option[T])] = Right(Subst.empty, None, None)
      val res = e.children.foldLeft(init) { case (accE, pair) =>
        val kexpr = pair.children.head
        val vexpr = pair.children(1)
        for
          (sAcc, kOpt, vOpt) <- accE
          Result(sK, tK) <- inferExpr(sAcc.applyTo(env), kexpr)
          Result(sV, tV) <- inferExpr(sK.applyTo(env), vexpr)
          sK2 <- kOpt.map(kk => Infer.unify(sV.apply(kk), sV.apply(tK))).getOrElse(Right(sV))
          sV2 <- vOpt.map(vv => Infer.unify(sK2.apply(vv), sK2.apply(tV))).getOrElse(Right(sK2))
        yield (sV2, Some(sV2.apply(tK)), Some(sV2.apply(tV)))
      }
      res.map { case (s, kOpt, vOpt) => Result(s, T.TDict(kOpt.getOrElse(fresh()), vOpt.getOrElse(fresh()))) }
    case "index"  =>
      val tE = e.children.find(_.kind=="target").flatMap(_.children.headOption).get
      val kE = e.children.find(_.kind=="key").flatMap(_.children.headOption).get
      for
        Result(s1, tT) <- inferExpr(env, tE)
        Result(s2, tK) <- inferExpr(s1.applyTo(env), kE)
        tv = fresh()
        // try dict: Dict[String, tv] and key String
        tryDict = Infer.unify(s2.apply(tT), T.TDict(T.TString, tv)).flatMap { sD => Infer.unify(sD.apply(tK), T.TString).map(sK => sK.compose(sD)) }
        res <- tryDict.orElse {
          // try list: List[tv] and key Int
          for sL <- Infer.unify(s2.apply(tT), T.TList(tv)); sK <- Infer.unify(sL.apply(tK), T.TInt) yield sK.compose(sL)
        }
      yield Result(res.compose(s2).compose(s1), res.apply(tv))
    case "field" =>
      val tgt = e.children.head
      val key = e.attrs.find(_.key=="name").map(_.value).getOrElse("")
      for
        Result(s1, tT) <- inferExpr(env, tgt)
        tv = fresh()
        // Prefer record: { key: tv | ρ }
        sRec <- Infer.unify(s1.apply(tT), T.TRecord(Map(key -> tv), Some(999999)))
      yield Result(sRec.compose(s1), sRec.apply(tv))
    case "list"   =>
      val init: Either[TypeError, (Subst, Option[T])] = Right(Subst.empty, None)
      val res = e.children.foldLeft(init) { case (accE, ch) =>
        for
          (sAcc, tOpt) <- accE
          Result(sCh, tCh) <- inferExpr(sAcc.applyTo(env), ch)
          comb <- tOpt match
            case None => Right((sCh, Some(tCh)))
            case Some(t0) => Infer.unify(sCh.apply(t0), sCh.apply(tCh)).map(u => (u.compose(sCh), Some(u.apply(t0))))
        yield comb
      }
      res.map { case (s, topt) => Result(s, T.TList(topt.getOrElse(fresh()))) }
    case "call"   =>
      val name = e.name.get
      val argsE = e.children
      val init: Either[TypeError, (Subst, List[T])] = Right(Subst.empty, Nil)
      val res = argsE.foldLeft(init) { case (accE, ex) =>
        for
          (sAcc, ts) <- accE
          Result(sX, tX) <- inferExpr(sAcc.applyTo(env), ex)
        yield (sX.compose(sAcc), ts :+ tX)
      }
      name match
        case "iter" =>
        // specialize: either List[a] -> Iter[a] or Dict[k,b] -> Iter[(k,b)]
          for
            (sArgs, tArgs) <- res
            tv = fresh()
            choice = Infer.unify(tArgs.headOption.getOrElse(fresh()), T.TList(tv)).map(s => (s, tv)).orElse {
              val k = fresh(); val v = fresh(); Infer.unify(tArgs.headOption.getOrElse(fresh()), T.TDict(k, v)).map(s => (s, T.TTuple2(k, v)))
            }
            (sC, elem) <- choice
          yield Result(sC.compose(sArgs), T.TIter(elem))
        case "length" =>
          val a = fresh(); for sL <- Infer.unify(sArgs.apply(tR), T.TList(a)) yield Result(sL, T.TInt)
        case "head" =>
          val a = fresh(); for sL <- Infer.unify(sArgs.apply(tR), T.TList(a)) yield Result(sL, sL.apply(a))
        case "tail" =>
          val a = fresh(); for sL <- Infer.unify(sArgs.apply(tR), T.TList(a)) yield Result(sL, T.TList(sL.apply(a)))
        case "append" =>
          val a = fresh(); for sL <- Infer.unify(sArgs.apply(tR), T.TList(a)); sV <- if tArgs.nonEmpty then Infer.unify(sL.apply(tArgs.head), sL.apply(a)) else Right(sL) yield Result(sV, T.TList(sV.apply(a)))
        case "concat" =>
          val a = fresh(); val b = fresh(); for sL <- Infer.unify(sArgs.apply(tR), T.TList(a)); sB <- if tArgs.nonEmpty then Infer.unify(sL.apply(tArgs.head), T.TList(a)) else Right(sL) yield Result(sB, T.TList(sB.apply(a)))
        case "size" =>
          val k = fresh(); val v = fresh(); for sD <- Infer.unify(sArgs.apply(tR), T.TDict(k, v)) yield Result(sD, T.TInt)
        case "values" =>
          val k = fresh(); val v = fresh(); for sD <- Infer.unify(sArgs.apply(tR), T.TDict(k, v)) yield Result(sD, T.TList(sD.apply(v)))
        case "entries" =>
          val k = fresh(); val v = fresh(); for sD <- Infer.unify(sArgs.apply(tR), T.TDict(k, v)) yield Result(sD, T.TList(T.TTuple2(sD.apply(k), sD.apply(v))))
        case "put" =>
          val k = fresh(); val v = fresh(); for sD <- Infer.unify(sArgs.apply(tR), T.TDict(k, v)); sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD); sV <- if tArgs.length>=2 then Infer.unify(sK.apply(tArgs(1)), sK.apply(v)) else Right(sK) yield Result(sV, T.TDict(sV.apply(k), sV.apply(v)))
        case "remove" =>
          val k = fresh(); val v = fresh(); for sD <- Infer.unify(sArgs.apply(tR), T.TDict(k, v)); sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD) yield Result(sK, T.TDict(sK.apply(k), sK.apply(v)))
        case "getOrElse" =>
          val k = fresh(); val v = fresh(); for sD <- Infer.unify(sArgs.apply(tR), T.TDict(k, v)); sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD); sDef <- if tArgs.length>=2 then Infer.unify(sK.apply(tArgs(1)), sK.apply(v)) else Right(sK) yield Result(sDef, sDef.apply(v))
        case "toList" =>
          val a = fresh(); for sI <- Infer.unify(sArgs.apply(tR), T.TIter(a)) yield Result(sI, T.TList(sI.apply(a)))
        case _ =>
          for
            (sArgs, tArgs) <- res
            fT <- env.lookup(name).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
            beta = fresh()
            sFn <- Infer.unify(sArgs.apply(fT), T.TFun(tArgs.map(sArgs.apply), beta))
          yield Result(sFn.compose(sArgs), sFn.apply(beta))
    case "mcall" =>
      val name = e.name.get
      val recvE = e.children.head
      val argsE = e.children.tail
      for
        Result(sR, tR) <- inferExpr(env, recvE)
        // infer args under substituted env
        resArgs <- argsE.foldLeft(Right((sR, List.empty[T])): Either[TypeError, (Subst, List[T])]) {
          case (accE, ex) =>
            for
              (sAcc, ts) <- accE
              Result(sX, tX) <- inferExpr(sAcc.applyTo(env), ex)
            yield (sX.compose(sAcc), ts :+ tX)
        }
        (sA, tArgs) = resArgs
        out <- name match
          case "push" =>
            val a = fresh()
            for sL <- Infer.unify(sA.apply(tR), T.TList(a)); sV <- if tArgs.nonEmpty then Infer.unify(sL.apply(tArgs.head), sL.apply(a)) else Right(sL)
            yield Result(sV, T.TList(sV.apply(a)))
          case "iter" =>
            val a = fresh(); val k = fresh(); val v = fresh()
            // List[a] -> Iter[a], Dict[k,v] -> Iter[(k,v)]
            val tryList = Infer.unify(sA.apply(tR), T.TList(a)).map(s => Result(s, T.TIter(s.apply(a))))
            val tryDict = Infer.unify(sA.apply(tR), T.TDict(k, v)).map(s => Result(s, T.TIter(T.TTuple2(s.apply(k), s.apply(v)))))
            tryList.orElse(tryDict)
          case "hasKey" =>
            val k = fresh(); val v = fresh()
            for sD <- Infer.unify(sA.apply(tR), T.TDict(k, v)); sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD)
            yield Result(sK, T.TBool)
          case "keys" =>
            val k = fresh(); val v = fresh()
            for sD <- Infer.unify(sA.apply(tR), T.TDict(k, v)) yield Result(sD, T.TList(sD.apply(k)))
          case "get" =>
            val k = fresh(); val v = fresh()
            for sD <- Infer.unify(sA.apply(tR), T.TDict(k, v)); sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD)
            yield Result(sK, sK.apply(v))
          case "hasNext" =>
            val a = fresh(); for sI <- Infer.unify(sA.apply(tR), T.TIter(a)) yield Result(sI, T.TBool)
          case "next" =>
            val a = fresh(); for sI <- Infer.unify(sA.apply(tR), T.TIter(a)) yield Result(sI, sI.apply(a))
          case _ =>
            // Try record-style method: recv has field `name` of type (args) -> beta
            val beta = fresh()
            val tryRecord = Infer.unify(sA.apply(tR), T.TRecord(Map(name -> T.TFun(tArgs, beta)), Some(0))).map { sRec =>
              Result(sRec.compose(sA), sRec.apply(beta))
            }
            tryRecord.orElse {
              val init: Either[TypeError, (Subst, List[T])] = Right((sA, tR :: tArgs))
              for
                (sAll, allArgs) <- init
                fT <- env.lookup(name).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
                beta = fresh()
                sFn <- Infer.unify(sAll.apply(fT), T.TFun(allArgs.map(sAll.apply), beta))
            yield Result(sFn.compose(sAll), sFn.apply(beta))
            }
      yield out
    case other => Right(Result(Subst.empty, fresh()))

  extension (s: Subst) def applyTo(env: Env): Env =
    Env(env.table.view.mapValues { sch =>
      sch.copy(body = s.apply(sch.body))
    }.toMap)
