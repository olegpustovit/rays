# The kit's reference sheets

The design of the Rays UI (kit rev 3) as static pages, one per sheet of the "Rays UI Redesign"
canvas, with their renders. They are the ground truth for `Pxui.Theme`, `Pxui.Ui`, `Pxui_shell`,
`Pxui_graph` and the editor's panels: a panel is right when its picture matches its sheet, token
for token and point for point. Like `specification/flow/prototype`, this is a reference to open in
a browser, never product code and never a web fallback.

| Sheet | Size | What it fixes |
|---|---|---|
| `main` | 1440 x 1180 | tokens: colour, type, metrics, lines, marks, states |
| `widgets` | 1440 x 1320 | every control and its states |
| `workspace` | 1440 x 900 | all panels docked: the layout `kit.rays` layout 0 and `sketches/ws_layout` reproduce |
| `overlays` | 1440 x 900 | menus, leader, key sheet, prompt, carry, echo |
| `graph` | 1200 x 760 | the graph panel: cards, ports, wires, zones, levels, states |
| `inspector` | 380 x 760 | the inspector panel |
| `outline` | 380 x 760 | the outline panel |
| `text` | 560 x 760 | the Lisp panel |
| `viewport` | 1200 x 600 | the viewport panel |
| `timeline` | 1200 x 97 | the timeline panel |
| `windows`, `docking` | 1440 x 900, 1440 x 440 | floating windows, dock targets, splitters, collapsed panes |

`kit.css` is the one stylesheet (its last block, "rev 3: lighter", overrides what is above it);
`<sheet>.html` is the markup; `<sheet>@2x.png` is Chrome's Retina render of it
(1 CSS pixel is 1 logical point). `kit.rays` is the fixture: the graph the sheets draw, in layout 0
(the Workspace sheet, 1440 x 900) and layout 1 (the Graph sheet beside the Inspector sheet:
render at 1581 x 784).

    dune build tools/ui_shot.exe
    SDL_VIDEODRIVER=dummy ./_build/default/tools/ui_shot.exe specification/pxui-kit/kit.rays out.png
    UI_SHOT_DO="key:space key:[ key:1 click:490,95" SDL_VIDEODRIVER=dummy \
      ./_build/default/tools/ui_shot.exe specification/pxui-kit/kit.rays out.png 1581 784

The second command selects `scatter1`: the graph panel is the left 1200 points, the inspector the
right 380.
`UI_SHOT_SCALE=2` renders two pixels a point, as a Retina window does: compare that with the
`@2x` renders (at 1x the app's glyph advances are whole pixels, so text runs a little wide).
