package com.github.kmizu.treep.`macro`

import com.github.kmizu.treep.east.*
import java.util.concurrent.atomic.AtomicLong

/** Expands both built-in and user-defined macros */
object MacroExpander:
  private val counter = new AtomicLong(0L)

  /** Generate a unique hygienic identifier */
  private def gensym(prefix: String = "__tmp"): String =
    s"${prefix}$$${counter.incrementAndGet()}"

  /** Main expansion entry point */
  def expand(tree: Element): Element =
    // First, extract and register user-defined macros
    val programWithoutMacros = MacroRegistry.extractAndRegister(tree)
    // Then expand all macros (built-in and user-defined)
    val expanded = expandAll(programWithoutMacros)
    // Post-process: unwrap expr(assign(...)) -> assign(...)
    MacroPostProcess.unwrapExprAssign(expanded)

  /** Recursively expand all macros in the tree */
  private def expandAll(el: Element): Element =
    MacroBuiltins.expand(el, expandAll, gensym) match
      case Some(expanded) => expanded
      case None =>
        el.kind match
          case "call" =>
            // Check if this is a user-defined macro invocation
            val name = el.name.getOrElse("")
            MacroRegistry.lookup(name) match
              case Some(macroEntry) =>
                expandUserMacro(macroEntry, el)
              case None => el.copy(children = el.children.map(expandAll))
          case _ => el.copy(children = el.children.map(expandAll))

  /** Expand a user-defined macro */
  private def expandUserMacro(macroEntry: MacroRegistry.MacroEntry, callSite: Element): Element =
    // Parse the pattern
    val parsedPattern = MacroPattern.parsePattern(macroEntry.pattern)

    // Get the expansion template (first child of macro block)
    val expansion = macroEntry.expansion

    // Match and expand
    MacroPattern.expandMacro(parsedPattern, expansion, callSite)

    // Note: We don't recursively expand here to avoid infinite loops.
    // Nested macro expansion will be handled by the outer expandAll loop.

  // Post-processing now lives in MacroPostProcess.
