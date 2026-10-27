Invariant lookup tables are independent of the mapped source count. The generated export
transports numeric captures through data.bin and executes the actual runtime and worker pool.

  $ cat > gather.rays <<'X'
  > (workspace gather (graph picture :context draw
  >   (let* [table (map (fn [i] [(+ i 0.25) (- i 1.0) -0.0]) (array/range 7))
  >          positions (map (fn [(i : int)] (+ (array/nth table (int/mod i 7)) [t 0.0 0.0])) (array/range 32769 :type int))]
  >     (draw/points positions :color "#ffffff"))))
  > X
  $ rays web gather.rays -o gather
  $ printf '{"type":"module"}\n' > gather/package.json
  $ cat > gather.mjs <<'X'
  > import {readFileSync} from 'node:fs';
  > import * as program from './gather/program.js';
  > import {createRuntime} from './gather/runtime/runtime.js';
  > import {createPool} from './gather/runtime/workers.js';
  > const file=readFileSync('gather/data.bin'),data=file.buffer.slice(file.byteOffset,file.byteOffset+file.byteLength);
  > const region=program.regions.findIndex(r=>r.source.includes('const table ='));
  > if(region<0||program.regions[region].count!==32769||!program.regions[region].uniforms.some(u=>u.table))throw Error('lookup capture transport');
  > for(const size of [0,1,8]){const pool=size?await createPool({size}):null;
  >   try{const rt=createRuntime(program,{data,pool});rt.current={t:.25,dt:0,frame:0,size:[1,1]};
  >     const out=(await rt.invoke(region,[])).data;
  >     for(let i=0;i<32769;i++){const row=i%7;if(out[i*3]!==row+.5||out[i*3+1]!==row-1||!Object.is(out[i*3+2],0))throw Error('lookup export bytes '+i)}
  >     await rt.stepFrame({t:.25,frame:1});if(rt.counts.gpu!==0||rt.marks.length===0)throw Error('lookup actual frame');
  >   }finally{pool?.close()}}
  > console.log('lookup export: data.bin, actual frames, one/eight workers and CPU placement pass');
  > X
  $ node gather.mjs
  lookup export: data.bin, actual frames, one/eight workers and CPU placement pass

Integer lookup captures keep their low bits through the actual exported data writer, in exact
precision (fast precision carries region integers as numbers, exact to 2^53).

  $ cat > integer.rays <<'X'
  > (workspace gather (graph picture :context draw
  >   (let* [table (array/int 3 9007199254740993)
  >          samples (map (fn [(i : int)] (int/add (array/nth table (int/mod i 3)) (frame/index))) (array/range 32769 :type int))]
  >     (draw/text [0 0 0] (str (array/nth samples 0)))))
  >   (graph settings :context settings (settings/config :precision "exact")))
  > X
  $ rays web integer.rays -o integer
  $ printf '{"type":"module"}\n' > integer/package.json
  $ cat > integer.mjs <<'X'
  > import {readFileSync} from 'node:fs';import * as program from './integer/program.js';
  > import {createRuntime} from './integer/runtime/runtime.js';import {createPool} from './integer/runtime/workers.js';
  > const file=readFileSync('integer/data.bin'),data=file.buffer.slice(file.byteOffset,file.byteOffset+file.byteLength);
  > const region=program.regions.findIndex(r=>r.source.includes('const table ='));
  > if(region<0||!program.regions[region].integer||!program.regions[region].uniforms.some(u=>u.table?.[2]))throw Error('integer table transport');
  > for(const size of [0,1,8]){const pool=size?await createPool({size}):null;try{
  >   const rt=createRuntime(program,{data,pool});rt.current={t:.25,dt:0,frame:2,size:[1,1]};
  >   const values=(await rt.invoke(region,[])).data;if(!(values instanceof BigInt64Array)||values.some(n=>n!==9007199254740995n))throw Error('integer table bits');
  >   await rt.stepFrame({t:.25,frame:2});if(rt.marks.find(m=>m.type==='text')?.text!=='9007199254740995')throw Error('integer table frame');
  > }finally{pool?.close()}}
  > console.log('integer lookup export: exact signed63 data and actual frame pass');
  > X
  $ node integer.mjs
  integer lookup export: exact signed63 data and actual frame pass

Text leaves keep their native bytes through exported captures and worker results.

  $ cat > text.rays <<'X'
  > (workspace lookup (graph picture :context draw
  >   (let* [names (array/text 7 "é日本😀")
  >          selected (map (fn [(i : int)] (if (> t 0.) (array/nth names (int/mod i 7)) "safe")) (array/range 32769 :type int))]
  >     (draw/text [0 0 0] (array/nth selected 0)))))
  > X
  $ rays web text.rays -o text
  $ printf '{"type":"module"}\n' > text/package.json
  $ cat > text.mjs <<'X'
  > import {readFileSync} from 'node:fs';import * as program from './text/program.js';
  > import {createRuntime} from './text/runtime/runtime.js';import {createPool} from './text/runtime/workers.js';
  > import {textBytes,textOfBytes} from './text/runtime/prims.js';
  > const file=readFileSync('text/data.bin'),data=file.buffer.slice(file.byteOffset,file.byteOffset+file.byteLength);
  > const region=program.regions.findIndex(r=>r.text&&r.source.includes('const table ='));
  > if(region<0||!program.regions[region].uniforms.some(u=>u.texts))throw Error('text capture transport');
  > for(const size of [0,1,8]){const pool=size?await createPool({size}):null;try{
  >   const rt=createRuntime(program,{data,pool});rt.current={t:.25,dt:0,frame:2,size:[1,1]};
  >   const values=(await rt.invoke(region,[])).data;if(values.length!==32769||values.some(s=>s!=='é日本😀'))throw Error('text table values');
  >   await rt.stepFrame({t:.25,frame:2});if(rt.counts.gpu!==0||rt.marks.find(m=>m.type==='text')?.text!=='é日本😀')throw Error('text table frame');
  >   const raw=textOfBytes(Uint8Array.of(0xff,0,0x80));
  >   const r=program.regions[region],uniforms=r.uniforms.map(u=>u.texts?{texts:u.texts.map(()=>raw)}:u);
  >   rt.load({...program,regions:program.regions.map((r,i)=>i===region?{...r,uniforms}:r)});
  >   rt.current={t:.25,dt:0,frame:3,size:[1,1]};
  >   if((await rt.invoke(region,[])).data.some(s=>!Buffer.from(textBytes(s)).equals(Buffer.from([255,0,128]))))throw Error('text worker bytes');
  > }finally{pool?.close()}}
  > console.log('text lookup export: immutable captures, exact bytes, actual frame and one/eight workers pass');
  > X
  $ node text.mjs
  text lookup export: immutable captures, exact bytes, actual frame and one/eight workers pass

Cartesian Text leaves transport a Text source separately from the lookup table;
skips compact worker outputs without changing either source count.

  $ cat > product.rays <<'X'
  > (workspace lookup (graph picture :context draw
  >   (let* [names (array/text 7 "picked")
  >          selected (for [i (array/range 2 :type int) old (array/text 16385 "old")]
  >            :skip [0 16384 32769]
  >            (if (> t 0.) (if (int/eq i 0) old (array/nth names i)) "safe"))]
  >     (draw/text [0 0 0] (array/nth selected 0)))))
  > X
  $ rays web product.rays -o product
  $ printf '{"type":"module"}\n' > product/package.json
  $ cat > product.mjs <<'X'
  > import {readFileSync} from 'node:fs';import * as program from './product/program.js';
  > import {createRuntime} from './product/runtime/runtime.js';import {createPool} from './product/runtime/workers.js';
  > const file=readFileSync('product/data.bin'),data=file.buffer.slice(file.byteOffset,file.byteOffset+file.byteLength);
  > const region=program.regions.findIndex(r=>r.text&&r.count===32770);
  > if(region<0||program.regions[region].kept!==32767||!program.regions[region].inputs.some(i=>i.texts))throw Error('Text Product count');
  > for(const size of [0,1,8]){const pool=size?await createPool({size}):null;try{
  >   const rt=createRuntime(program,{data,pool});rt.current={t:.25,dt:0,frame:0,size:[1,1]};
  >   const output=(await rt.invoke(region,[])).data;
  >   if(output.length!==32767||output.some((s,i)=>s!==(i<16383?'old':'picked')))throw Error('Text Product values');
  > }finally{pool?.close()}}
  > console.log('Text Product export: independent Text source, counts, skipped chunks and one/eight workers pass');
  > X
  $ node product.mjs
  Text Product export: independent Text source, counts, skipped chunks and one/eight workers pass
