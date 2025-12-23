package com.github.kmizu.treep.types

import com.github.kmizu.treep.types.Type as T

object BuiltinEnv:
  def base: Env =
    def scheme(t: T): Scheme = Scheme(Set.empty, t)
    var nextSchemeVar: Int = 1
    def freshSchemeVar(): Int =
      val id = nextSchemeVar
      nextSchemeVar += 1
      id
    def poly(count: Int)(build: List[T] => T): Scheme =
      val ids = List.fill(count)(freshSchemeVar())
      val vars = ids.map(T.TVar.apply)
      Scheme(ids.toSet, build(vars))

    val ops: Map[String, Scheme] = Map(
      "+" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a, a), a)
      },
      "-" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "*" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "/" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "%" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      ">" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      ">="-> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "<" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "<="-> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "=="-> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a, a), T.TBool)
      },
      "!="-> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a, a), T.TBool)
      },
      "print" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a), T.TUnit)
      },
      "println" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a), T.TUnit)
      },
      // Collections
      "push" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(T.TList(a), a), T.TList(a))
      },
      // dict helpers, fully polymorphic in key/value
      "keys" -> poly(2) { vars =>
        val List(k, v) = vars
        T.TFun(List(T.TDict(k, v)), T.TList(k))
      },
      "hasKey" -> poly(2) { vars =>
        val List(k, v) = vars
        T.TFun(List(T.TDict(k, v), k), T.TBool)
      },
      // Iterators for lists and dicts
      "iter" -> poly(2) { vars =>
        val List(arg, res) = vars
        T.TFun(List(arg), res)
      }, // specialized in call branch
      "hasNext" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(T.TIter(a)), T.TBool)
      },
      "next" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(T.TIter(a)), a)
      },
      // tuple accessors
      "fst" -> poly(2) { vars =>
        val List(a, b) = vars
        T.TFun(List(T.TTuple2(a, b)), a)
      },
      "snd" -> poly(2) { vars =>
        val List(a, b) = vars
        T.TFun(List(T.TTuple2(a, b)), b)
      },
      // Math functions
      "abs" -> scheme(T.TFun(List(T.TInt), T.TInt)),
      "min" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "max" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "pow" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt))
    )
    Env(ops)
