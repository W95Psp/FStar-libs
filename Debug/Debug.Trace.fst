module Debug.Trace

module I = Debug.Trace.Internal

[@plugin]
val trace_raw (s: string) (v: 'a): 'a
let trace_raw s v = I.trace s v

[@plugin (strict_on_arguments [0])]
val trace (s: string) (v: 'a): 'a
let trace s v = trace_raw (s ^ "\n") v

