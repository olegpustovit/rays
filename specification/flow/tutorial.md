# Rays Flow: building with the graph

Four lessons on `examples/recursive_rectangles` (`dune exec rays -- sketches/recursive_rectangles.rays`
runs it; `/ l g` makes a graph panel, `/ j` picks `picture`). Every gesture edits the Lisp; the
Lisp panel shows the text it wrote. Keys are `flow.md` §7.2.

## 1. Read a loop

- `boxes` is a `fold`: a zone whose label row is the accumulator and its source, `depth in range
  iterations` the counter. The begin bar on the left hands `boxes` and `depth` to the body; the
  end bar on the right (`next ↺`) takes the body's result back round.
- `[` and `]` step the zone's probe: the cards inside show that iteration's values.
- Select the card that calls `split-level` and press `i` (or double-click it): its `defn` opens. `u` goes back.
- `ƒ` on a formula row opens the expression as dashed cards in place, so every number is a field;
  `c` on one of them folds it back. `⇧U` makes one its own binding.
- `⌥⇧I` on a `defn` call replaces it with the definition's body, to change one use only.

## 2. Build a loop

- Select a card and press `⇧M`: it becomes the body of a `map` over `items (range 6)`; the body reads
  the item as `i`. Wire `i` into a row, or type `(* i 20)` into its field, to use it.
- `⇧R` feeds a card's result back into an input it reads from outside: a `fold`, 4 runs.
- `r` repeats a card in a `for` and merges the copies (`sop/merge`, `draw/merge`) or sums them.

## 3. Functions

- `l` makes a local `fn` of the selection: the nodes it reads become parameters, the call card is
  wired from the `fn`. A selection that reads nothing is refused; wire an input first.
- `d` makes a `defn` (the dialog types its parameters); `i` on a call opens it, and an edit there
  changes every call.
- A `fn` written in a row (`map (fn [i] ...)`) is a zone: double-click a collapsed one, or `c`, to open it.

## 4. Chains and wires

- A `->` and a nest of draw calls are chains: a column of cards, the source on top, joined by spines. Select a step: `⌥↑` / `⌥↓` move it along the
  chain, `b` bypasses it, `x` removes it and its input takes its place.
- Delete never asks you to disconnect: a node something reads dissolves into its readers. `⌘Z` brings it back.
- Click a wire and press `x` to take it off; the row keeps the value it carried. Hold `Alt` and drag a wire (or an out-port, for all its
  wires) onto another row to move it, or onto empty canvas to take it off.
