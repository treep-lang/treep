# treep 言語リポジトリ

## 概要
- treep は、表層構文 → EAST 正規化 → マクロ展開 → HM 型推論 → インタプリタ実行のパイプラインを持つミニ言語です。
- 実装は Scala 3（sbt マルチモジュール）。ソース拡張子は `.treep`。

## 特長（MVP）
- EAST（Element-AST）への正規化と衛生的マクロ（`for (x in: xs)` → `while`）
- Hindley–Milner（Algorithm W）による型推論
  - List/Dict/Index/Field/CALL/演算子を型付け
  - `iter(list)` → `Iter[a]`、`iter(dict)` → `Iter[(k, v)]`
  - `keys(dict)` は `List[k]`、`hasKey(dict, k)` は `Bool`
  - タプル操作: `fst`, `snd`
- 表層構文の辞書キーは一般式（Expr）を許可（K は多相）

## ディレクトリ構成（抜粋）
- `modules/lexer` … トークナイザ
- `modules/parser` … CST + Pratt/LL パーサ
- `modules/east` … EAST 定義 + 正規化 + マクロ
- `modules/types` … Type/Scheme/Subst/Unify/HM/Checker
- `modules/interpreter` … 素朴インタプリタ
- `modules/cli` … CLI（`treep new|build|run|fmt|test`）
- `modules/tests` … MUnit テスト

## 要件
- Java 21
- sbt 1.10 以降

## セットアップ & ビルド/実行
- 依存取得・ビルド: `sbt compile`
- サンプル生成: `sbt 'cli/run new'`（`samples/hello.treep`）
- 構文/型チェック（生成物なし）: `sbt 'cli/run build'`
- 実行（インタプリタ）: `sbt 'cli/run run'`
- テスト: `sbt test`

## 使い方の例（.treep）
```treep
// 辞書と for/in マクロ（iter(dict) は (k,v) のペア）
const m = { "a": 1, ("b" + ""): 2 }

def sum() returns: Int {
  let s = 0
  for (p in: m) { s = s + snd(p) }
  return s
}
```

## 型のポイント（HM）
- `List[a]`, `Dict[k, v]`, `Iter[t]`, `(a, b)`（タプル）
- `iter(list)` → `Iter[a]`
- `iter(dict)` → `Iter[(k, v)]`
- `keys(dict)` → `List[k]`
- `hasKey(dict, k)` → `Bool`
- 代入は文（`assign`）として正規化し、同名の外側束縛を更新

## 開発メモ
- 位置情報（file:line:col）は EAST `SourceSpan` に保持
- マクロは EAST に対して適用（衛生的。内部で `__it$N` を gensym）
- 辞書キーの等値はインタプリタ上、値の構造等価で判定

---
詳しい実装・規約は `AGENTS.md` を参照してください。
