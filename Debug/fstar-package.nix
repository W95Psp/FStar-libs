  { name = "Debug.Trace";
    sources-directory = ./.;
    sources = [
      "Debug.Trace"
      "Debug.Trace.Internal.fsti"
    ];
    ocaml-sources = [
      "Debug_Trace_Internal.ml"
    ];
    tactic-module = "Debug.Trace";
    dependencies = [];
    compile = [];
  }
