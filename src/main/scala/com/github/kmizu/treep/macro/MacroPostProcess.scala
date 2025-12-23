package com.github.kmizu.treep.`macro`

import com.github.kmizu.treep.east.*

object MacroPostProcess:
  /**
   * Post-process to unwrap expr(assign(...)) -> assign(...)
   * This is needed because macros can expand to assign statements,
   * but they are called in expr contexts.
   */
  def unwrapExprAssign(el: Element): Element =
    el.kind match
      case "expr" if el.children.size == 1 && el.children.head.kind == "assign" =>
        el.children.head.copy(children = el.children.head.children.map(unwrapExprAssign))
      case _ =>
        el.copy(children = el.children.map(unwrapExprAssign))
