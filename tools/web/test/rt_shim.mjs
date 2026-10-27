// A minimal `rt` for running emitted frame programs (Js.frame) under node: the primitives of
// prims.js, a mark table, the Lisp frame record, and stand-ins for the runtime services the
// frame program calls. Shape geometry uses the same runtime tessellator that the browser exports.
import * as prims from '../../../lib/web_export/runtime/prims.js';
import * as shape2 from '../../../lib/web_export/runtime/shape2.js';
import { frameValue } from '../../../lib/web_export/runtime/runtime.js';

export function makeRt() {
  return {
    ...prims,
    marks: [],
    frameValue,
    image: () => 1,
    invoke: () => { throw new Error('the shim has no regions'); },
    polygon: shape2.polygon,
    polyline: shape2.polyline,
    rounded_rect: shape2.rounded_rect,
  };
}
