package com.github.kmizu.treep.parser

import munit.FunSuite
import com.github.kmizu.treep.parser.CST as C

class ParserSpec extends FunSuite {

  test("parse simple function with return") {
    val src =
      """
      |def main() returns: Unit {
      |  return 0
      |}
    |""".stripMargin
    val prog = Parser.parseProgram(src)
    assertEquals(prog.tops.length, 1)
    prog.tops.head match
      case C.FunDef(name, params, ret, body, _) =>
        assertEquals(name, "main")
        assertEquals(params.isEmpty, true)
        assert(ret.exists(_.name == "Unit"))
        assert(body.stmts.nonEmpty)
      case other => fail(s"expected FunDef, got ${other}")
  }

  test("operator precedence: 1 + 2 * 3") {
    val src = "const x: Int = 1 + 2 * 3"
    val prog = Parser.parseProgram(src)
    prog.tops.head match
      case C.ConstDecl(_, _, init, _) =>
        init match
          case C.Binary(op, C.IntLit(1), C.Binary("*", C.IntLit(2), C.IntLit(3))) =>
            assertEquals(op, "+")
          case other => fail(s"unexpected init: ${other}")
      case other => fail(s"expected ConstDecl, got ${other}")
  }

  test("list and dict literals") {
    val src = "const xs = [1,2]\nconst m = { \"a\": 1, b: 2 }"
    val prog = Parser.parseProgram(src)
    assertEquals(prog.tops.length, 2)
    prog.tops.head match
      case C.ConstDecl(_, _, C.ListLit(es), _) => assertEquals(es.length, 2)
      case other => fail(s"expected const list, got ${other}")
    prog.tops(1) match
      case C.ConstDecl(_, _, C.DictLit(ps), _) =>
        assertEquals(ps.length, 2)
        assert(ps.exists { case (C.StrLit("a"), _) => true; case _ => false })
      case other => fail(s"expected const dict, got ${other}")
  }

  test("method-call node: xs.push(1) -> MethodCall(recv=xs, name=push)") {
    val src = "const r = xs.push(1)"
    val prog = Parser.parseProgram(src)
    prog.tops.head match
      case C.ConstDecl(_, _, C.MethodCall(_, name, args), _) =>
        assertEquals(name, "push")
        assertEquals(args.length, 1)
      case other => fail(s"expected const method call, got ${other}")
  }

  test("multi-arg lambda parses with param list") {
    val src = "const f = (x: Int, y: Int) -> { return x + y }"
    val prog = Parser.parseProgram(src)
    prog.tops.head match
      case C.ConstDecl(_, _, C.Lambda(ps, _), _) =>
        assertEquals(ps.length, 2)
        assertEquals(ps.head.name, "x")
      case other => fail(s"expected const lambda, got ${other}")
  }

  test("if/else and for-in in block") {
    val src =
      """
      |def f(xs: List[Int]) returns: Int {
      |  let acc: Int = 0
      |  for (i in xs) { let acc: Int = acc + i }
      |  if (acc > 0) { return acc } else { return 0 }
      |}
    |""".stripMargin
    val prog = Parser.parseProgram(src)
    prog.tops.head match
      case C.FunDef(_, _, _, body, _) =>
        assert(body.stmts.exists(_.isInstanceOf[C.ForInStmt]))
        assert(body.stmts.exists(_.isInstanceOf[C.IfStmt]))
      case other => fail(s"expected FunDef, got ${other}")
  }

  test("error recovery between top-level decls") {
    val src = "const a = 1\n$#$#$\nconst b = 2"
    val (prog, diags) = Parser.parseProgramWithDiagnostics(src)
    assertEquals(prog.tops.length >= 2, true)
    assert(diags.nonEmpty)
  }
}
