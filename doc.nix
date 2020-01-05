let p = import ./.;
    packages = (
      let
        resolve = p:
          if isDerivation p
          then [p]
          else (
            builtins.concatLists
              (
                map (attr: resolve p."${attr}")
                  (builtins.filter (x: x != "doc") (attrNames p))
              )
          )
        ;
      in resolve p
    );
    doc-db-src = p.MkDoc.doc-builder packages;
    doc-db = builtins.fromJSON
      (builtins.readFile ''${doc-db-src}/db.json'');
    pkgs = import <nixpkgs> {};
    isDerivation = pkgs.lib.isDerivation;
    concatStringsSep = pkgs.lib.concatStringsSep;
    hasPrefix = pkgs.lib.hasPrefix;
    trace = builtins.trace;
    attrNames = builtins.attrNames;
    nchar = n: s: if n == 0 then "" else s + nchar (n - 1) s;
    h = n: t: ''${nchar n "#"} ${t}'' + "\n"; 
    make = prefix: p:
      let
        len = builtins.length prefix;
        content = p.doc or "";
        c_name = concatStringsSep "." prefix;
      in
      ( if len == 0
        then ""
        else h len c_name
      )
      +
      (if content == "" then "" else content + "\n")
      +
      ( if isDerivation p
        then (
          let
            entries = builtins.filter (
              x: hasPrefix c_name x 
              && !(pkgs.lib.hasInfix "__proj__" x)
              && !(pkgs.lib.hasInfix "uu___" x)
              && !(pkgs.lib.hasInfix ".Mk" x) 
              && (builtins.match ".*\\.[A-Z][^.]*" x == null)
            ) (attrNames doc-db);
            names = map (x:
''
<details><summary><code>${x}</code></summary>
<p>
```fstar
${
  let l = pkgs.lib.splitString "\\n" (doc-db."${x}".type or ""); in
  # let l = builtins.filter (x: x != "") l; in
  concatStringsSep "\n" l # + (if builtins.length l == 0 then "" else "\n") 
 }
```
</p>
</details>

'' 

# - `${x}`
# ```${
#   let l = pkgs.lib.splitString "\\n" (doc-db."${x}".type or ""); in
#   "\n" + concatStringsSep "\n" l + "\n" # + (if builtins.length l == 0 then "" else "\n") 
# }```


#               ''
            ) entries;
          in
            (concatStringsSep "\n" names) + "\n"
        )
        else concatStringsSep ""
          (map
            (attr:
              make
                (prefix ++ [attr])
                p."${attr}"
            )
            (builtins.filter (x: x != "doc") (attrNames p))
          )
      );
    in
      let
        f = pkgs.writeText "doc.md" (make [] p);
      in (pkgs.runCommand "doc-markdown" {} ''
        mkdir $out
        cp ${f} $out/doc.md
      '')
      #   pkgs.stdenv.mkDerivation {
      #   name = "doc-markdown";
        
      #   phases = ["install"];
      #   installPhase = ''
      #   mkdir $out
      #   cp ${f} $out/doc.md
      #   '';
      # }
   
