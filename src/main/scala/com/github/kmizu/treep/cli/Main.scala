package com.github.kmizu.treep.cli

import java.nio.file.*
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*
<<<<<<< HEAD

import com.github.kmizu.treep.parser.Parser
import com.github.kmizu.treep.`macro`.Macro
import com.github.kmizu.treep.interpreter.Interpreter
import com.github.kmizu.treep.east.Normalize
import com.github.kmizu.treep.types.Checker

object Main:
=======
import scala.util.Using
import scala.collection.mutable

import com.github.kmizu.treep.parser.Parser
import com.github.kmizu.treep.`macro`.MacroExpander
import com.github.kmizu.treep.interpreter.Interpreter
import com.github.kmizu.treep.east.{Normalize, Element}
import com.github.kmizu.treep.types.Checker

object Main:
  private val cwd = Paths.get("").toAbsolutePath
  private val DefaultRoots: List[Path] = List(Paths.get("samples"), Paths.get(""))
  private val ExcludedDirNames: Set[String] = Set(".git", ".idea", ".bloop", ".metals", "target", "out")

  private final case class Analysis(path: Path, expanded: Element, parseErrors: List[Parser.ParseDiag], typeDiags: List[Checker.Diag])

>>>>>>> aa17f4483079e0ab8e8dc740702d56c5122247d4
  def main(args: Array[String]): Unit =
    if args.isEmpty then
      usage(); return
    args(0) match
      case "new"  => cmdNew(args.drop(1))
      case "build"=> cmdBuild(args.drop(1))
      case "run"  => cmdRun(args.drop(1))
      case "fmt"  => cmdFmt(args.drop(1))
      case "test" => cmdTest(args.drop(1))
      case _      => usage()

  private def usage(): Unit =
    println("treep new|build|run|fmt|test")

  private def cmdNew(rest: Array[String]): Unit =
    val path = Paths.get("samples/hello.treep")
    Files.createDirectories(path.getParent)
    val content = """
<<<<<<< HEAD
def main() returns: Unit {
  return
=======
def main() returns: Int {
  return 0
>>>>>>> aa17f4483079e0ab8e8dc740702d56c5122247d4
}
""".stripLeading()
    Files.writeString(path, content, StandardCharsets.UTF_8)
    println(s"Created ${path}")

  private def cmdBuild(rest: Array[String]): Unit =
<<<<<<< HEAD
    val cwd = Paths.get("")
    val matcher = FileSystems.getDefault.getPathMatcher("glob:**/*.treep")
    val outDir = cwd.resolve("out")
    Files.createDirectories(outDir)
    val stream = Files.walk(cwd)
    try
      val files = stream.iterator().asScala.toList.filter(p => Files.isRegularFile(p) && matcher.matches(p))
      if files.isEmpty then
        println("No .treep files found. Try `treep new`.")
      else
        // Build フェーズは構文/マクロ展開の検証のみ（生成物なし）
        files.foreach { in =>
          val src = Files.readString(in)
          val cst = Parser.parseProgram(src, in.toString)
          val east = Normalize.toEAST(cst)
          val expanded = Macro.expand(east)
          val typeDiags = Checker.check(expanded)
          val errs = Parser.lastErrors
          if errs.nonEmpty then
            println(s"Checked ${in} with ${errs.size} parse error(s):")
            errs.foreach(e => println(s"  [parse] ${e.message} at ${e.line}:${e.col}"))
          if typeDiags.nonEmpty then
            println(s"Typecheck reported ${typeDiags.size} issue(s)")
          else
            println(s"Checked ${in}")
        }
        println("Build succeeded.")
    finally
      stream.close()

  private def cmdRun(rest: Array[String]): Unit =
    val cwd = Paths.get("")
    val matcher = FileSystems.getDefault.getPathMatcher("glob:**/*.treep")
    val stream = Files.walk(cwd)
    try
      val files = stream.iterator().asScala.toList.filter(p => Files.isRegularFile(p) && matcher.matches(p))
      if files.isEmpty then
        println("No .treep files found. Try `treep new`.")
      else
        files.foreach { in =>
          val src = Files.readString(in)
          val cst = Parser.parseProgram(src, in.toString)
          val east = Normalize.toEAST(cst)
          val expanded = Macro.expand(east)
          val typeDiags = Checker.check(expanded)
          val errs = Parser.lastErrors
          if errs.nonEmpty then
            println(s"Parsing had ${errs.size} error(s), attempting to run anyway...")
            errs.foreach(e => println(s"  [parse] ${e.message} at ${e.line}:${e.col}"))
          if typeDiags.nonEmpty then
            println(s"Typecheck reported ${typeDiags.size} issue(s), attempting to run anyway...")
          Interpreter.run(expanded)
        }
    finally
      stream.close()
=======
    val files = collectTreepFiles(rest)
    if files.isEmpty then
      println("No .treep files found. Try `treep new`.")
    else
      files.foreach { path =>
        val analysis = analyzeFile(path)
        val label = relativePath(path)
        if analysis.parseErrors.nonEmpty then
          println(s"Checked ${label} with ${analysis.parseErrors.size} parse error(s):")
          reportParseDiagnostics(analysis.parseErrors)
        if analysis.typeDiags.nonEmpty then
          println(s"Typecheck reported ${analysis.typeDiags.size} issue(s) in ${label}:")
          reportTypeDiagnostics(analysis.typeDiags)
        else if analysis.parseErrors.isEmpty then
          println(s"Checked ${label}")
      }
      println("Build succeeded.")

  private def cmdRun(rest: Array[String]): Unit =
    val files = collectTreepFiles(rest)
    if files.isEmpty then
      println("No .treep files found. Try `treep new`.")
    else
      var executed = false
      files.foreach { path =>
        val analysis = analyzeFile(path)
        val label = relativePath(path)
        val hasMain = hasZeroArityMain(analysis.expanded)
        val canRun = hasMain && analysis.parseErrors.isEmpty

        if analysis.parseErrors.nonEmpty then
          println(s"Parsing had ${analysis.parseErrors.size} error(s) in ${label}.")
          reportParseDiagnostics(analysis.parseErrors)
        if analysis.typeDiags.nonEmpty then
          val suffix = if canRun then ", attempting to run anyway..." else "."
          println(s"Typecheck reported ${analysis.typeDiags.size} issue(s) in ${label}${suffix}")
          reportTypeDiagnostics(analysis.typeDiags)

        if hasMain then
          if canRun then
            executed = true
            Interpreter.run(analysis.expanded)
          else
            println(s"Skipping ${label} due to parse errors.")
        else
          println(s"Skipping ${label}: no zero-argument main() detected.")
      }
      if !executed then
        println("No entry point (def main() returns: ...) found in processed files.")
>>>>>>> aa17f4483079e0ab8e8dc740702d56c5122247d4

  private def cmdFmt(rest: Array[String]): Unit =
    println("Formatter MVP is a no-op for now.")

  private def cmdTest(rest: Array[String]): Unit =
    println("Tests are not wired yet. See modules/tests and tests/golden plan.")

<<<<<<< HEAD
=======
  private def collectTreepFiles(rest: Array[String]): List[Path] =
    val requested: List[Path] =
      if rest.nonEmpty then rest.toList.map(Paths.get(_))
      else DefaultRoots
    val found = mutable.LinkedHashSet.empty[Path]
    val visited = mutable.HashSet.empty[Path]
    requested.foreach { raw =>
      val root = if raw.isAbsolute then raw.normalize() else cwd.resolve(raw).normalize()
      if Files.notExists(root) then
        println(s"[warn] ${relativePath(root)} does not exist; skipping.")
      else if Files.isRegularFile(root) then
        if isTreep(root) then found += root
        else println(s"[warn] ${relativePath(root)} is not a .treep file; skipping.")
      else if Files.isDirectory(root) then
        collectFromDirectory(root, found, visited)
      else
        println(s"[warn] ${relativePath(root)} is not a regular file or directory; skipping.")
    }
    found.toList.sortBy(_.toString)

  private def collectFromDirectory(dir: Path, acc: mutable.LinkedHashSet[Path], visited: mutable.Set[Path]): Unit =
    if shouldSkipDir(dir) then return
    val normalized = dir.toAbsolutePath.normalize()
    if !visited.add(normalized) then return
    try
      Using.resource(Files.newDirectoryStream(dir)) { stream =>
        stream.iterator().asScala.foreach { entry =>
          if Files.isDirectory(entry) then
            if !Files.isSymbolicLink(entry) then
              collectFromDirectory(entry, acc, visited)
          else if isTreep(entry) then
            acc += entry.normalize()
        }
      }
    catch
      case _: java.nio.file.AccessDeniedException =>
        println(s"[warn] Unable to access ${relativePath(dir)}; skipping.")

  private def shouldSkipDir(path: Path): Boolean =
    val name = path.getFileName
    name != null && ExcludedDirNames.contains(name.toString)

  private def isTreep(path: Path): Boolean =
    path.getFileName != null && path.getFileName.toString.endsWith(".treep")

  private def analyzeFile(path: Path): Analysis =
    val src = Files.readString(path)
    val cst = Parser.parseProgram(src, path.toString)
    val east = Normalize.toEAST(cst)
    val expanded = MacroExpander.expand(east)
    Analysis(path, expanded, Parser.lastErrors, Checker.check(expanded))

  private def relativePath(path: Path): String =
    val abs = path.toAbsolutePath.normalize()
    try
      val rel = cwd.relativize(abs).toString
      if rel.isEmpty then "." else rel
    catch
      case _: IllegalArgumentException => abs.toString

  private def reportParseDiagnostics(errors: List[Parser.ParseDiag]): Unit =
    errors.foreach { e =>
      println(s"  [parse] ${e.message} at ${e.line}:${e.col}")
    }

  private def reportTypeDiagnostics(diags: List[Checker.Diag]): Unit =
    diags.foreach { d =>
      val where =
        if d.path.nonEmpty then d.path.mkString(" @ ", " / ", "")
        else ""
      println(s"  [type] ${d.msg}${where}")
    }

  private def hasZeroArityMain(program: Element): Boolean =
    def loop(el: Element): Boolean = el.kind match
      case "def" if el.name.contains("main") =>
        val params = el.attrs.find(_.key == "params").map(_.value.trim).getOrElse("")
        params.isEmpty
      case _ =>
        el.children.exists(loop)
    loop(program)
>>>>>>> aa17f4483079e0ab8e8dc740702d56c5122247d4
