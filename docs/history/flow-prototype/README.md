# Rays Flow prototype

A browser page that behaves like the first design of the SOP network canvas. It was a
behavioral study for the editor before it became a workspace of Lisp text.

This is a design artifact, not product code:

- Dune does not build it, nothing links it, and it is never a browser or web
  fallback for Rays (the root `AGENTS.md` rule stands).
- Open `index.html` directly in a browser (`open specification/flow/prototype/index.html`).
  No server, no build step, no network access needed; without network the
  prose falls back from IBM Plex to system fonts. The kit face (Departure
  Mono) is embedded in `font.css`.
- When the prototype and `flow.md` disagree, `flow.md` and the code win. The page is not
  kept in step with them.

## Files

| File | Contents |
|---|---|
| `index.html` | The page: mini editor shell (graph, list and text pane, view, inspector), demos and the text of the original proposal |
| `engine.js` | Everything with behavior: node kinds, evaluation, exposure rule, levels, keys, hints, printer, reader and checker (`window.Flow`) |
| `font.css` | Departure Mono as a data URI (OFL; source `assets/fonts/`) |

## What it is still a reference for

| Behavior | Function or table in `engine.js` | Spec section |
|---|---|---|
| Exposure rule (which rows a card shows) | `shownOnCard`, `rowsOf` | §5.1 |
| Levels of detail | `lodOf`, `RANK` | §6.4 |
| Letter hints | `hintTargets`, `hintKey` | §7.5 |
| Guide strip wording | `guideKeys`, `describe` | §10 |

## What the editor did not keep

The page also shows parts of the first design that the editor does not have. Do not port
them from here:

- value nodes (Time, Value, Math, Combine and Separate XYZ, Remap), the infix expression
  fields and the fold of a math chain into an expression;
- grouping nodes into a shared definition, entering it and exporting a row;
- authored wire bends, wireless wires and the knife;
- the `.` repeat key, the zoom caps on levels, and its key table (`KEYS`, `LEADER`): the
  keys are `flow.md` §7.2;
- its single-graph text form, printer and checker (`toLispLines`, `readSexp`,
  `compileFlow`): the language is `specification/flow.md` §11.

Its catalog is a toy (Grid, Sphere, Noise Displace, Swirl, Transform, Merge, Output); real
SOPs come from `Sop_catalog.Editor.factories`.
