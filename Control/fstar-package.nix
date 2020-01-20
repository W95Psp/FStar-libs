  { name = "Control";
    sources-directory = ./.;
    sources = [
      "Control.Applicative"
      "Control.Functor"
      "Control.Monoid"
      "Control.Semigroup"
    ];
    ocaml-sources = [];
    dependencies =
      with (import ./..);
      [ Data.Function
      ];
    tactic-module = null;
    compile = [];
  }
