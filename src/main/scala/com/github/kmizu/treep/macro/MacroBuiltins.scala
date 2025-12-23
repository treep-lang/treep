package com.github.kmizu.treep.`macro`

import com.github.kmizu.treep.east.*

object MacroBuiltins:
  def expand(
    el: Element,
    expandAll: Element => Element,
    gensym: String => String
  ): Option[Element] =
    el.kind match
      case "for" => Some(expandFor(el, expandAll, gensym))
      case _ => None

  private def expandFor(
    node: Element,
    expandAll: Element => Element,
    gensym: String => String
  ): Element =
    val binder = node.attrs.find(_.key == "var").map(_.value).getOrElse("_")
    val iterChild = node.children.find(_.kind == "iter").flatMap(_.children.headOption)
      .map(expandAll)
      .getOrElse(Element("list"))
    val bodyBlock = node.children.find(_.kind == "block").map(expandAll).getOrElse(Element("block"))

    val itName = gensym("__it")

    val iterInit = Element(
      kind = "let",
      name = Some(itName),
      children = List(Element("init", children = List(Element("call", name = Some("iter"), children = List(iterChild)))))
    )

    val cond = Element("cond", children = List(Element("call", name = Some("hasNext"), children = List(Element("var", name = Some(itName))))))

    val bindLet = Element(
      kind = "let",
      name = Some(binder),
      children = List(Element("init", children = List(Element("call", name = Some("next"), children = List(Element("var", name = Some(itName)))))))
    )

    val bodyExpanded = bodyBlock match
      case b if b.kind == "block" =>
        val rest = b.children.map(expandAll)
        Element("block", children = bindLet :: rest)
      case other => Element("block", children = bindLet :: List(expandAll(other)))

    val whileNode = Element("while", children = List(cond, bodyExpanded))

    Element("block", children = List(iterInit, whileNode))
