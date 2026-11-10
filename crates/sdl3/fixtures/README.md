# SDL3_image conformance fixtures

The `sample.*`, `palette.gif`, `rgbrgb.png`, and `svg.svg` files are unmodified
SDL3_image 3.4.4 test assets from commit
`bec9134a26c7d0f31b36d6083c25296e04cabff5` (`release-3.4.4`).  They cover
every still-image format for which that stable upstream release ships a test
fixture.  Rays uses them under SDL_image's zlib license:

> Copyright (C) 1997-2026 Sam Lantinga <slouken@libsdl.org>
>
> This software is provided 'as-is', without any express or implied warranty.
> In no event will the authors be held liable for any damages arising from the
> use of this software.
>
> Permission is granted to anyone to use this software for any purpose,
> including commercial applications, and to alter it and redistribute it
> freely, subject to these restrictions: the origin must not be
> misrepresented; altered source versions must be plainly marked; and this
> notice must not be removed or altered from a source distribution.

`orientation-6.jpg` is the upstream `sample.jpg` byte stream with a minimal EXIF
orientation-6 APP1 segment. `alpha.svg` is a Rays-authored two-pixel alpha fixture;
the minimal ILBM and XV files exercise the corresponding still-image decoders.
Historical generation notes are in [docs/history/ocaml-image-fixtures.md](../../../docs/history/ocaml-image-fixtures.md).

The conformance test decodes every file both by path and from copied bytes,
normalizes to tightly packed RGBA8, compares the common 23×42 source image
across lossless formats, checks alpha independently, and verifies the EXIF
rotate by exact pixel mapping.
