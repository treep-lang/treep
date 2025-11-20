<<<<<<< HEAD
# treep 言語リポジトリ

treep は、表層構文 → EAST 正規化 → マクロ展開 → HM 型推論 → インタプリタ実行、というシンプルで強力なパイプラインを持つミニ言語です。実装は Scala 3、拡張子は `.treep` です。

## クイックスタート
- 依存取得/ビルド: `sbt compile`
- サンプル生成: `sbt "run new"`（`samples/hello.treep`）
- 解析/型チェック（生成物なし）: `sbt "run build"`
- 実行（インタプリタ）: `sbt "run run"`
- テスト: `sbt test`

## 言語の骨子
- マクロ: `for (x in: xs)` は EAST 上で衛生的に `while` + イテレータに展開
- 型推論（HM / Algorithm W）:
  - `List[A]`, `Dict[K,V]`, `Iter[T]`, `(A,B)` を一級サポート
  - `iter(list)` → `Iter[A]`、`iter(dict)` → `Iter[(K,V)]`
  - Row 多相（簡易）によるレコード的アクセス（`p.x` は `{ x: T | ρ }` と単一化）
  - メソッド呼び出しは型で解決（List/Dict/Iter の標準 + レコード/拡張メソッド）
- 関数型とラムダ:
  - 型注釈: `A -> B`（右結合）
  - リテラル: `(x: T) -> { ... }`

## サンプル（抜粋）
```treep
// 1) 辞書と for/in（iter(dict) は (k,v) のペア）
const m = { "a": 1, ("b" + ""): 2 }
def sum() returns: Int {
  let s = 0
  for (p in: m) { s = s + snd(p) } // snd((k,v)) = v
  return s
}

// 2) 標準メソッド（List/Dict/Iter）
const xs = [1,2,3]
def demo() returns: Int {
  return xs.length() + xs.head() + xs.tail().length()
}

// 3) 拡張メソッド（トップレベル関数で拡張）
def inc(self: Int) returns: Int { return self + 1 }
def main() returns: Int { return 1.inc() }

// 4) ラムダと関数型
def apply1(f: Int -> Int, x: Int) returns: Int { return f(x) }
const incF = (x: Int) -> { return x + 1 }
def use() returns: Int { return apply1(incF, 41) }
```

## 標準メソッド（代表）
- List[A]
  - `length(): Int`, `head(): A`, `tail(): List[A]`, `push(a): List[A]`,
    `append(a): List[A]`, `concat(xs: List[A]): List[A]`, `iter(): Iter[A]`
- Dict[K,V]
  - `size(): Int`, `keys(): List[K]`, `values(): List[V]`, `entries(): List[(K,V)]`,
    `hasKey(k: K): Bool`, `get(k: K): V`, `getOrElse(k: K, d: V): V`,
    `put(k: K, v: V): Dict[K,V]`, `remove(k: K): Dict[K,V]`, `iter(): Iter[(K,V)]`
- Iter[T]
  - `hasNext(): Bool`, `next(): T`, `toList(): List[T]`
- Tuple 関数
  - `fst((A,B)): A`, `snd((A,B)): B`

メソッド解決の優先順位: ビルトイン → レコード（`{"m": f}` のような関数フィールド）→ トップレベル関数 `m(recv, ...)`（拡張メソッド）

## 型と Row 多相（概要）
- 主要型: `Int | Bool | String | Unit | List[A] | Dict[K,V] | Iter[T] | (A,B) | A -> B`
- Row 多相レコード（簡易）: `{ x: T | ρ }` のイメージで `p.x` を型付け
  - 片側に不足フィールドがある場合でも、相手側に行変数（ρ）があれば単一化を受け入れる
  - Dict はキーが一般式（Expr）で記述可（`{ 1: "a", true: 2 }` など）。`p.x` は文字列キーの糖衣にも対応

## ディレクトリ構成（抜粋）
- `src/main/scala/com/github/kmizu/treep/lexer` … トークナイザ
- `src/main/scala/com/github/kmizu/treep/parser` … CST + Pratt/LL パーサ（関数型/ラムダ対応）
- `src/main/scala/com/github/kmizu/treep/east` … EAST 定義 + 正規化（mcall/lambda/SourceSpan）
- `src/main/scala/com/github/kmizu/treep/macro` … EAST マクロ展開（for/in → while）
- `src/main/scala/com/github/kmizu/treep/types` … Type/Scheme/Subst/Unify/HM/Checker（Row/メソッド解決）
- `src/main/scala/com/github/kmizu/treep/interpreter` … インタプリタ（List/Dict/Iter/Record メソッド + ラムダ）
- `src/main/scala/com/github/kmizu/treep/cli` … `treep new|build|run|fmt|test`
- `src/test/scala/com/github/kmizu/treep` … MUnit テスト

## 設計ノート
- 位置情報（file:line:col）は EAST `SourceSpan` に保持し、診断に活用（順次整備）
- マクロは EAST に対して適用（衛生的: `__it$N` を gensym）
- メソッドは「型構造での組込み」+「レコード/拡張関数」のハイブリッド解決でサブタイプ不要

より詳しい貢献ルール/コーディング規約は `AGENTS.md` を参照してください。
=======
# treep 言語

![Build Status](https://github.com/kmizu/treep/actions/workflows/ci.yml/badge.svg)

treep は、表層構文 → EAST 正規化 → マクロ展開 → HM 型推論 → インタプリタ実行という学習しやすいパイプラインを備えたミニ言語です。Scala 3 で実装されており、ファイル拡張子は `.treep` を利用します。

---

## 目次
- [概要](#概要)
- [前提条件](#前提条件)
- [クイックスタート](#クイックスタート)
- [CLI コマンド](#cli-コマンド)
- [言語ハイライト](#言語ハイライト)
- [最初のプログラム](#最初のプログラム)
- [ユーザー定義マクロ](#ユーザー定義マクロ)
- [標準ライブラリ早見表](#標準ライブラリ早見表)
- [プロジェクト構成](#プロジェクト構成)
- [開発・テストの進め方](#開発テストの進め方)
- [関連ドキュメント](#関連ドキュメント)

---

## 概要
- **Scala 製の処理系**: ソースは `src/main/scala` にまとまり、CLI からビルド／実行可能。
- **シンプルなパイプライン**: 構文解析 → EAST 正規化 → マクロ展開 → Hindley–Milner 型推論 → インタプリタ実行を一貫して体験できます。
- **教育／実験用途向け**: 拡張メソッド、Row 多相レコード、イテレータなどをコンパクトに確認できます。

## 前提条件
- Java 21 互換 JDK
- [sbt](https://www.scala-sbt.org/) 1.10 以降（動作確認バージョン: 1.10.2）
- macOS / Linux / WSL など POSIX 系シェル環境

---

## クイックスタート
1. 依存解決とビルド
   ```bash
   sbt compile
   ```
2. サンプルを生成（`samples/hello.treep` を作成）
   ```bash
   sbt "run new"
   ```
3. 作成したサンプルを実行
   ```bash
   sbt "run run samples/hello.treep"
   ```
4. 全サンプルを一括で解析／型検査
   ```bash
   sbt "run build"
   ```
5. テストの実行（MUnit）
   ```bash
   sbt test
   ```

---

## CLI コマンド
| コマンド | 説明 |
| --- | --- |
| `sbt "run new"` | `samples/hello.treep` を生成。既存ファイルがある場合は上書き。 |
| `sbt "run build"` | 現在のディレクトリ配下の `.treep` を探索し、構文・マクロ展開・型検査を実施。 |
| `sbt "run run"` | `.treep` を探索し、ゼロ引数 `main` を持つファイルのみ実行。 |
| `sbt "run run path/to/foo.treep"` | 指定したファイルを解析・型検査後に実行。 |
| `sbt "run fmt"` | フォーマッタ (MVP) — 現時点ではノーオペレーション。 |
| `sbt "run test"` | 将来的なテスト統合ポイント（現在は案内メッセージのみ）。 |

---

## 言語ハイライト
- **衛生的マクロ（組み込み＆ユーザー定義）**
  - 組み込みマクロ: `for (x in: xs)` のような糖衣構文を EAST 上で `while` + イテレータへ展開。
  - ユーザー定義マクロ: `macro name { pattern: ..., expand: { ... } }` 構文でカスタムマクロを定義可能。
  - 変数捕捉を避けるため gensym (`__tmp$N`) を採用し、衛生的な展開を保証。
  - EAST 形式を活用した型安全なパターンマッチング＆変数置換。
- **Hindley–Milner 型推論**  
  Algorithm W をベースに `List[A]`, `Dict[K, V]`, `Iter[T]`, タプル、関数型をサポート。拡張メソッドも型推論に統合。
- **Row 多相レコード**  
  `{ x: T | ρ }` 形式でフィールド不足を許したまま `p.x` を扱えるため、レコード拡張や動的辞書アクセスを自然に表現可能。
- **一貫したメソッド解決**  
  ビルトイン → レコード関数フィールド → トップレベル関数 (`recv` を先頭引数に取る) の順で解決。
- **シンプルな関数記法**  
  ラムダは `(x: Int) -> { ... }`、関数型は `Int -> Int` で右結合。関数値を第一級で扱える。

---

## 最初のプログラム
`samples/hello.treep` を立ち上げ、`println` と戻り値を確認してみましょう。

```treep
def main() returns: Int {
  println("hello from treep!")
  return 0
}
```

実行例:

```bash
sbt "run run samples/hello.treep"
```

```
hello from treep!
[treep] exit 0
```

さらに、辞書とイテレータを組み合わせた例です。

```treep
const m = { "a": 1, "b": 2 }

def sumDict() returns: Int {
  let s = 0
  for (pair in: m) {
    println(pair)
    s = s + snd(pair)
  }
  println("total:")
  println(s)
  return s
}

def main() returns: Int { return sumDict() }
```

---

## ユーザー定義マクロ
treep では、EAST 形式を活用した型安全なマクロシステムを提供しています。`macro` キーワードを使って、カスタムマクロを定義できます。

### マクロの定義
```treep
macro unless {
  pattern: unless($cond) { $body }
  expand: {
    if (!$cond) {
      $body
    }
  }
}
```

### マクロの使用
```treep
def test() returns: Unit {
  let x = 5

  // unless マクロを使用
  unless(x > 10) {
    println("x is not greater than 10")
  }

  unless(x == 0) {
    println("x is not zero")
  }
}

def main() returns: Unit {
  test()
}
```

実行例: `sbt "run run samples/unless.treep"`

```
=== Unless Macro Demo ===
x is not greater than 10
x is not zero
=== Done ===
```

### マクロの仕組み
- **パターンマッチング**: `$変数名` でパターン変数を宣言し、呼び出し時の引数をキャプチャ。
- **展開テンプレート**: `expand:` ブロック内で `$変数名` を参照すると、キャプチャした引数に置き換わります。
- **衛生的展開**: マクロ展開時に gensym を使用し、変数捕捉を回避。
- **型安全**: EAST 形式で展開されるため、型検査器が最終的な型の整合性を検証。

---

## 標準ライブラリ早見表

### コレクション
| レシーバ | 主なメソッド |
| --- | --- |
| `List[A]` | `length()`, `head()`, `tail()`, `push(a)`, `append(a)`, `concat(xs)`, `iter()` |
| `Dict[K,V]` | `size()`, `keys()`, `values()`, `entries()`, `hasKey(k)`, `get(k)`, `getOrElse(k, d)`, `put(k, v)`, `remove(k)`, `iter()` |
| `Iter[T]` | `hasNext()`, `next()`, `toList()` |

### タプル・補助
- `fst((A,B))`, `snd((A,B))`
- `print(value)`, `println(value)` — どの値でも表示可能。戻り値は `Unit` (`()` 表記)。

---

## プロジェクト構成
```
.
├─ samples/                  # 言語機能をカバーする .treep サンプル
├─ src/
│  └─ main/scala/com/github/kmizu/treep/
│       ├─ lexer/           # トークナイザ
│       ├─ parser/          # CST と Pratt/LL パーサ
│       ├─ east/            # EAST 定義と正規化、ビルダー API
│       ├─ macro/           # マクロ展開 (MacroExpander, MacroRegistry, MacroPattern)
│       ├─ types/           # Type/Scheme/Subst/Unify/HM/Checker
│       ├─ interpreter/     # インタプリタ
│       └─ cli/             # `treep new|build|run|fmt|test`
└─ src/test/scala/          # MUnit テスト
```

---

## 開発・テストの進め方
- コード変更後は必ず `sbt compile` → `sbt test` の順で確認する。
- CLI での簡易チェック:
  - `sbt "run build"` で型検査まで
  - `sbt "run run"` で実行確認
- サンプル追加時は `samples/` に `.treep` を置き、`main` をゼロ引数で用意すると CLI から実行しやすいです。

---

## 関連ドキュメント
- 開発規約・コントリビューションガイド: [`AGENTS.md`](./AGENTS.md)
- 追加のテスト計画やゴールデンテスト案: `modules/tests`, `tests/`（整備予定）

質問や改善アイデアがあれば Issue / PR でお気軽にどうぞ！
>>>>>>> aa17f4483079e0ab8e8dc740702d56c5122247d4
