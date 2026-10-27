The web export: a draw graph becomes a standalone browser application. A second run is byte-identical.

  $ rays web ../../../sketches/flow_vectors.rays -o out
  $ find out -type f | sort
  out/data.bin
  out/index.html
  out/kernels.json
  out/kernels.wgsl
  out/program.js
  out/program.map
  out/runtime/gpu.js
  out/runtime/marks.wgsl
  out/runtime/present.js
  out/runtime/prims.js
  out/runtime/region_bindings.js
  out/runtime/resident.js
  out/runtime/runtime.js
  out/runtime/shape2.js
  out/runtime/workers.js
  out/serve.json
  $ rays web ../../../sketches/flow_vectors.rays -o again
  $ diff -r out again && echo identical
  identical
  $ head -1 out/program.js
  // Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.
  $ grep -c "export function frame" out/program.js
  1
  $ cmp out/runtime/prims.js ../../../lib/web_export/runtime/prims.js && echo "runtime is the checked-in one"
  runtime is the checked-in one

A packed-array program keeps its constants in data.bin, and the region table names them.

  $ rays web ../../../sketches/particles.rays -o particles
  $ test -s particles/data.bin && grep -c "rt.data" particles/program.js
  1

A draw graph that reaches a sop graph is refused, with the span of the reference.

  $ cat > sop.rays <<'X'
  > (workspace w
  >   (graph geometry :context sop (sop/box))
  >   (graph picture :context draw
  >     (let* [g (ref geometry)]
  >       (draw/merge (draw/background "#080a10")
  >                   (draw/circle [10 10 0] 5)))))
  > X
  $ rays web sop.rays -o refused
  File "sop.rays", line 4, characters 13-27:
  Error [E_WEB_GRAPH]: The web export draws 2D canvas graphs only; this reaches the sop geometry.
  [1]
  $ test -e refused && echo written || echo nothing written
  nothing written

C1 topology and mesh operators retain the production 2D export boundary.

  $ cat > mesh.rays <<'X'
  > (workspace mesh
  >   (graph geometry :context sop
  >     (let* [source (sop/mesh :points (array/vec3 0)
  >                            :vertex_points (array/int 0)
  >                            :primitive_offsets (array/int 1) :kinds (array/int 0))
  >            topology (sop/topology source)]
  >       (sop/mesh :points (sop/attr source :P)
  >                 :vertex_points topology.vertex_points
  >                 :primitive_offsets topology.primitive_offsets :kinds topology.kinds)))
  >   (graph picture :context draw
  >     (let* [g (ref geometry)] (draw/background "#080a10"))))
  > X
  $ rays web mesh.rays -o mesh_out 2>&1 | sed -n '/^Error/p'
  Error [E_WEB_GRAPH]: The web export draws 2D canvas graphs only; this reaches the sop geometry.
  $ test -e mesh_out && echo written || echo nothing written
  nothing written

The built-in noise image exports with native seeded scalar arguments.

  $ rays web ../../../sketches/flow_image.rays -o image
  $ grep -c "kind: 'noise'" image/program.js
  1

Generated images export deterministically, with kernels and compiled live arguments, without UV data.

  $ rays web ../../../sketches/flow_contours.rays -o contours
  $ rays web ../../../sketches/flow_contours.rays -o contours_again
  $ diff -r contours contours_again && echo identical
  identical
  $ grep -c '"id":"image[0-9]*"' contours/kernels.json
  6
  $ test ! -s contours/data.bin && echo 'UVs are computed on the GPU'
  UVs are computed on the GPU
  $ rays web ../../../sketches/flow_image_kernel.rays -o image_kernel
  $ rays web ../../../sketches/flow_shader.rays -o shader

File images are decoded into the standalone bundle; nested render targets export deterministically.

  $ printf 'P3\n2 1\n255\n255 0 0 0 255 0\n' > pixels.ppm
  $ cat > loaded.rays <<'X'
  > (workspace w (graph picture :context draw
  >   (draw/image (image/render (draw/image (image/load "pixels.ppm")) :width 3 :height 5))))
  > X
  $ rays web loaded.rays -o loaded
  $ rays web loaded.rays -o loaded_again
  $ diff -r loaded loaded_again && echo identical
  identical
  $ test -s loaded/data.bin && grep -c "kind: 'load'" loaded/program.js
  1
  $ grep -c "kind: 'render'" loaded/program.js
  1

Missing or invalid assets fail before writing any export.

  $ printf 'bad image\n' > pixels.ppm
  $ rays web loaded.rays -o bad 2>&1 | sed -n 's/^Error \[\([^]]*\)\].*/\1/p'
  E_WEB_IMAGE
  $ test -e bad && echo written || echo nothing written
  nothing written
  $ rm pixels.ppm
  $ rays web loaded.rays -o missing 2>&1 | sed -n 's/^Error \[\([^]]*\)\].*/\1/p'
  E_WEB_IMAGE
  $ test -e missing && echo written || echo nothing written
  nothing written

A standalone application's file assets must be known at export time.

  $ cat > dynamic_path.rays <<'X'
  > (workspace w (graph picture :context draw
  >   (draw/image (image/load (str "image-" (frame/index) ".png")))))
  > X
  $ rays web dynamic_path.rays -o bad 2>&1 | sed -n 2p
  Error [E_WEB_IMAGE]: A standalone export needs image/load paths known at export time.

A mark set whose attributes are GPU regions never invokes them on the CPU: the frame names a device array, and two
attributes of equal count and rate share one kernel with an output each.

  $ rays web ../../../sketches/flow_particles_gpu.rays -o gpu
  $ grep -c "rt.gpuArray(0)" gpu/program.js
  1
  $ grep -c "rt.invoke" gpu/program.js
  0
  [1]
  $ grep -c '^export const gpu = ' gpu/program.js
  1
  $ grep -o '"id":"k[0-9]*"' gpu/kernels.json
  "id":"k0"
  $ cat > fused.rays <<'X'
  > (workspace w
  >   (graph g :context draw
  >     (let* [p (map (fn [i] [(+ i t) i 0]) (array/range 2048))
  >            r (map (fn [i] (+ 1 (* 0.001 (+ i t)))) (array/range 2048))]
  >       (draw/circles p :radius r))))
  > X
  $ rays web fused.rays -o fused
  $ grep -o '"gpu": {[^}]*}' fused/program.js
  "gpu": {"output": 0}
  "gpu": {"output": 1}
  $ grep -c '"id":' fused/kernels.json
  1
  $ grep -o '"output_widths":\[[0-9,]*\]' fused/kernels.json
  "output_widths":[3,1]

The frame reading a region's value (here one element of it) keeps that region on the CPU: a read on the GPU would be
a readback in the frame, which the web cannot do.

  $ cat > read.rays <<'X'
  > (workspace w
  >   (graph g :context draw
  >     (let* [p (map (fn [i] [(+ i t) i 0]) (array/range 2048))]
  >       (draw/merge (draw/circles p :radius 2) (draw/circle (array/nth p 0) 3)))))
  > X
  $ rays web read.rays -o read
  $ grep -c "rt.gpuArray" read/program.js
  0
  [1]
  $ grep -c "rt.invoke(0" read/program.js
  1

Computed local scalar, exact integer, array and nested-record captures export as structured Core.
The production exporter never probes deferred argument sources or leaves unresolved region arguments.

  $ cat > captures.rays <<'X'
  > (workspace w (graph picture :context draw
  >   (let* [minimum (array/min (array/float 3 t))
  >          exact (int/add 9007199254740993 (frame/index))
  >          column (array/concat (array/float 2 t) (array/float 2 (+ t 3.)))
  >          sample {:nested {:bias minimum :exact exact :column column}}
  >          shift (fn [x] (+ x sample.nested.bias))]
  >     (draw/text [0 0 0] (str (array/sum (map shift sample.nested.column)) ":" sample.nested.exact)))))
  > X
  $ rays web captures.rays -o captures
  $ printf '{"type":"module"}\n' > captures/package.json
  $ grep -c '\$core/argument' captures/program.js
  0
  [1]
  $ cat > captures.mjs <<'X'
  > import {readFileSync} from 'node:fs';
  > import * as program from './captures/program.js';
  > import * as p from './captures/runtime/prims.js';
  > import {createRuntime} from './captures/runtime/runtime.js';
  > const file = readFileSync('captures/data.bin'), data = file.buffer.slice(file.byteOffset, file.byteOffset + file.byteLength);
  > const rt = createRuntime(program, {data});
  > await rt.stepFrame({t: 0.25, dt: 0, frame: 2, tick: 2, size: [640,360], pointer: [0,0], keys: [], buttons: [], events: []});
  > const text = rt.marks.find(mark => mark.type === 'text')?.text;
  > if (text !== '8:9007199254740995') throw new Error(`computed captures: ${text}`);
  > console.log(text);
  > const zero = new p.PArray(1, Float64Array.of(-0, 0));
  > if (!Object.is(p.array_min(zero), -0) || !Object.is(p.array_max(zero), 0)) throw new Error('extremum signed zero');
  > const offsets = new p.PArray(1, BigInt64Array.of(0n, 2n));
  > const integer = new p.PArray(1, BigInt64Array.of(4611686018427387903n, 4611686018427387903n));
  > if (p.array_segment_sum(integer, offsets).data[0] !== -2n) throw new Error('exact wrapping segment sum');
  > const cancellation = new p.PArray(1, Float64Array.of(1e16, 1, -1e16));
  > if (!Object.is(p.array_segment_sum(cancellation, new p.PArray(1, BigInt64Array.of(0n, 3n))).data[0], 0)) throw new Error('ordered additions');
  > if (!Object.is(zero.data[0], -0) || integer.data[0] !== 4611686018427387903n) throw new Error('borrowed input mutation');
  > console.log('Reduction signed zero, ordered cancellation and signed63 wrapping are exact');
  > X
  $ node captures.mjs
  8:9007199254740995
  Reduction signed zero, ordered cancellation and signed63 wrapping are exact
