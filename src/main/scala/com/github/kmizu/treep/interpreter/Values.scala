package com.github.kmizu.treep.interpreter

import com.github.kmizu.treep.east.Element

/** Runtime values for the treep interpreter */
object Values:
  sealed trait Value
  case object VUnit extends Value
  final case class VInt(v: Int) extends Value
  final case class VBool(v: Boolean) extends Value
  final case class VString(v: String) extends Value
  final case class VList(vs: List[Value]) extends Value
  final case class VDict(m: Map[Value, Value]) extends Value
  final case class VIter(private val data: List[Value], private var ix: Int) extends Value:
    def hasNext: Boolean = ix < data.length
    def next(): Value = { val r = data(ix); ix += 1; r }
    def remaining: List[Value] = data.drop(ix)
  final case class VTuple2(a: Value, b: Value) extends Value
  final case class VFunc(params: List[String], body: Element, env: Runtime.Env) extends Value

