package com.github.kmizu.treep.parser

import com.github.kmizu.treep.lexer.Lexer
import com.github.kmizu.treep.parser.CST.Program
import java.util.concurrent.atomic.AtomicReference

object Parser:
  final case class ParseDiag(message: String, line: Int, col: Int)

  private val recentErrors = new AtomicReference[List[ParseDiag]](Nil)
  def lastErrors: List[ParseDiag] = recentErrors.get()

  def parseProgram(src: String, file: String = "<stdin>"): Program =
    val tokens = Lexer.tokenize(src, file).toVector
    val p = new ParserImpl(tokens, file)
    val prog = p.program()
    recentErrors.set(p.errors.toList)
    prog

  def parseProgramWithDiagnostics(src: String, file: String = "<stdin>"): (Program, List[ParseDiag]) =
    val tokens = Lexer.tokenize(src, file).toVector
    val p = new ParserImpl(tokens, file)
    val prog = p.program()
    val errs = p.errors.toList
    recentErrors.set(errs)
    (prog, errs)
