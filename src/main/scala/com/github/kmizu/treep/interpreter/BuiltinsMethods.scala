package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.interpreter.Values.*

object BuiltinsMethods:
  def eval(name: String, recv: Value, args: List[Value]): Option[Value] =
    (name, recv, args) match
      // String methods
      case ("split", VString(s), List(VString(delimiter))) =>
        Some(VList(s.split(java.util.regex.Pattern.quote(delimiter), -1).toList.map(VString(_))))
      case ("length", VString(s), Nil) => Some(VInt(s.length))
      case ("substring", VString(s), List(VInt(start), VInt(end))) =>
        val validStart = math.max(0, math.min(start, s.length))
        val validEnd = math.max(validStart, math.min(end, s.length))
        Some(VString(s.substring(validStart, validEnd)))
      case ("contains", VString(s), List(VString(substr))) => Some(VBool(s.contains(substr)))
      // List methods
      case ("push", VList(xs), List(v)) => Some(VList(xs :+ v))
      case ("iter", VList(xs), Nil) => Some(VIter(xs, 0))
      case ("length", VList(xs), Nil) => Some(VInt(xs.length))
      case ("head", VList(xs), Nil) => Some(xs.headOption.getOrElse(VUnit))
      case ("tail", VList(xs), Nil) => Some(VList(if xs.nonEmpty then xs.tail else Nil))
      case ("append", VList(xs), List(v)) => Some(VList(xs :+ v))
      case ("concat", VList(xs), List(VList(ys))) => Some(VList(xs ++ ys))
      // Dict methods
      case ("keys", VDict(m), Nil) => Some(VList(m.keys.toList))
      case ("get", VDict(m), List(k)) => Some(m.getOrElse(k, VUnit))
      case ("iter", VDict(m), Nil) => Some(VIter(m.toList.map { case (k, v) => VTuple2(k, v) }, 0))
      case ("size", VDict(m), Nil) => Some(VInt(m.size))
      case ("values", VDict(m), Nil) => Some(VList(m.values.toList))
      case ("entries", VDict(m), Nil) => Some(VList(m.toList.map { case (k, v) => VTuple2(k, v) }))
      case ("put", VDict(m), List(k, v)) => Some(VDict(m.updated(k, v)))
      case ("remove", VDict(m), List(k)) => Some(VDict(m - k))
      case ("getOrElse", VDict(m), List(k, d)) => Some(m.getOrElse(k, d))
      // Iter methods
      case ("hasNext", it: VIter, Nil) => Some(VBool(it.hasNext))
      case ("next", it: VIter, Nil) => Some(it.next())
      case ("toList", it: VIter, Nil) => Some(VList(it.remaining))
      case _ => None
