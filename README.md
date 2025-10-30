# treep 言語リポジトリ

treep は、表層構文 → EAST 正規化 → マクロ展開 → HM 型推論 → インタプリタ実行、というシンプルで強力なパイプラインを持つミニ言語です。実装は Scala 3、拡張子は `.treep` です。

## クイックスタート
- 依存取得/ビルド: `sbt compile`
- サンプル生成: `sbt 'cli/run new'`（`samples/hello.treep`）
- 解析/型チェック（生成物なし）: `sbt 'cli/run build'`
- 実行（インタプリタ）: `sbt 'cli/run run'`
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
- `modules/lexer` … トークナイザ
- `modules/parser` … CST + Pratt/LL パーサ（関数型/ラムダ対応）
- `modules/east` … EAST 定義 + 正規化（mcall/lambda/SourceSpan）+ マクロ
- `modules/types` … Type/Scheme/Subst/Unify/HM/Checker（Row/メソッド解決）
- `modules/interpreter` … インタプリタ（List/Dict/Iter/Record メソッド + ラムダ）
- `modules/cli` … `treep new|build|run|fmt|test`
- `modules/tests` … MUnit テスト

## 設計ノート
- 位置情報（file:line:col）は EAST `SourceSpan` に保持し、診断に活用（順次整備）
- マクロは EAST に対して適用（衛生的: `__it$N` を gensym）
- メソッドは「型構造での組込み」+「レコード/拡張関数」のハイブリッド解決でサブタイプ不要

より詳しい貢献ルール/コーディング規約は `AGENTS.md` を参照してください。
