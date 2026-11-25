# Sketch faces

The faces `draw/text :font` can name beside the default Pragmasevka and Departure Mono (both in
`pxui/assets/fonts`). All are under the SIL Open Font License; the licence texts are beside them.

| `:font` | File | Source |
| --- | --- | --- |
| `archivo` | ArchivoSheet-Regular.ttf | Archivo, width 100, weight 400 |
| `archivo-semibold` | ArchivoSheet-SemiBold.ttf | Archivo, width 100, weight 600 |
| `archivo-condensed-bold` | ArchivoSheet-CondensedBold.ttf | Archivo, width 75, weight 700 |
| `archivo-expanded-extrabold` | ArchivoSheet-ExpandedExtraBold.ttf | Archivo, width 125, weight 800 |
| `newsreader` | NewsreaderPage-Regular.ttf | Newsreader, optical size 14, weight 400 |
| `newsreader-semibold` | NewsreaderPage-SemiBold.ttf | Newsreader, optical size 16, weight 600 |
| `newsreader-display` | NewsreaderPage-Display.ttf | Newsreader, optical size 60, weight 500 |
| `plex-condensed` | IBMPlexSansCondensed-Regular.ttf | IBM Plex Sans Condensed, as shipped |
| `plex-condensed-semibold` | IBMPlexSansCondensed-SemiBold.ttf | IBM Plex Sans Condensed, as shipped |
| `plex-mono` | IBMPlexMono-Regular.ttf | IBM Plex Mono, as shipped |

The Archivo and Newsreader files are static instances cut from the variable fonts of
`github.com/google/fonts` (`ofl/archivo`, `ofl/newsreader`) with fontTools `varLib.instancer`, subset
to Basic Latin, Latin-1, Latin Extended-A and common punctuation, and renamed (`Archivo Sheet ...`,
`Newsreader Page ...`) so each loads as its own family. The shaper picks a face by load order, not by
weight or width, which is why every weight is a file.
