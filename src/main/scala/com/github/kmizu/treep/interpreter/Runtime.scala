package com.github.kmizu.treep.interpreter

import scala.collection.mutable
import com.github.kmizu.treep.interpreter.Values.*

/** Runtime environment and control flow helpers */
object Runtime:
  final case class RuntimeExtension(methodName: String, receiverParam: String, func: VFunc)

  final case class Env(scopes: List[mutable.Map[String, Value]], extensions: List[RuntimeExtension] = Nil):
    def push(): Env = Env(mutable.Map.empty[String, Value] :: scopes, extensions)
    def pop(): Env = Env(scopes.tail, extensions)
    def set(name: String, v: Value): Unit = scopes.head.update(name, v)
    def get(name: String): Option[Value] = scopes.collectFirst(Function.unlift(_.get(name)))
    def assign(name: String, v: Value): Unit =
      val target = scopes.reverse.find(_.contains(name)).getOrElse(scopes.head)
      target.update(name, v)
    def addExtension(ext: RuntimeExtension): Env = Env(scopes, extensions :+ ext)

  object Env:
    def empty: Env = Env(List(mutable.Map.empty[String, Value]), Nil)

  final case class Return(v: Value) extends Throwable

