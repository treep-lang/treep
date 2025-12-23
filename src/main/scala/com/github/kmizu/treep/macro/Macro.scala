package com.github.kmizu.treep.`macro`

import com.github.kmizu.treep.east.*

object Macro:
  def expand(tree: Element): Element =
    MacroExpander.expand(tree)
