let
  path = "/home/lucas/Bureau/veffect/";
  p = pm (import ../veffect/fstar-package.nix);
    pm = pkgs.fstar-package-manager;

    pkgs = import <nixpkgs> {};

    isDerivation = pkgs.lib.isDerivation;
    concatStringsSep = pkgs.lib.concatStringsSep;
    hasPrefix = pkgs.lib.hasPrefix;
    trace = builtins.trace;
    attrNames = builtins.attrNames;
    head = builtins.head;
    tail = builtins.tail;
    filter = builtins.filter;
    nchar = n: s: if n == 0 then "" else s + nchar (n - 1) s;
    removeOdd = l: if l == [] then l else (
      if tail l == []
      then []
      else tail (tail l)
    );
    re = "\n(open[[:space:]]+([^\n]+)|module[[:space:]]+[^\n=]+=[[:space:]]+([^\n]+))";

    h = n: t: ''${nchar n "#"} ${t}'' + "\n"; 
    get-packages = dir: pB:
      let p = pB.module; in
      [{ key = p.name;
         val = (
           map (
             file:
             let
               matches = r: filter (line: builtins.match r == null);
               linesRaw = filter (x: ! (builtins.isList x) && x != "") (
                   builtins.split "\n"
                     (builtins.readFile "${dir}/${file}.fst")
                 );
               lines =
                 map (x: head (filter (x: x != null) (tail x)))
                   (filter builtins.isList (
                   builtins.split re
                     (builtins.readFile "${dir}/${file}.fst")
                 ))
               ;
               opens = matches "open.*" lines;
             in
               { module = file; refs = lines; nlines = builtins.length linesRaw; }
           ) p.sources
         );
      }]
         ++ builtins.concatLists (map (p: get-packages p.module.sources-directory p) p.dependencies)
    ;
    toObject = o: l:
      if l == []
      then o
      else (
        let
          hd = head l;
          tl = tail l;
        in
          toObject (o // {"${hd.key}" = hd.val;}) tl
      )
    ;
    packages = toObject {} (get-packages path p.build);
    pn = builtins.replaceStrings ["."] ["_"];
in
"digraph G {" +
      concatStringsSep "\n\n"
        (map
          (packageName:
            let
              modules = packages."${packageName}";
            in
              ''
              subgraph cluster_${pn packageName} {
                 # style=filled;
                 fontsize=22;
                 border=lightgrey;
                 label="${pn packageName} (${toString (builtins.foldl' (x: y: x+y) 0 (map (m: m.nlines) modules))}L)";
                 node[shape=none];
                 ${concatStringsSep "->" (map (m:
                   ''"${pn m.module} (${toString m.nlines}L)"''
                 ) modules)} -> " " [style=invis]

              }
''
          )
          (attrNames packages)
        ) +
      ''
      ''
      + "}"
      # builtins.toJSON packages
      # (trace "${import ../veffect}")
      #   (pkgs.writeText "out.json" "x" #(builtins.toJSON packages)
      #   )


        
                 # ${concatStringsSep "\n" (map (m:
                 #   concatStringsSep "\n"
                 #     (map
                 #       (x: "   node [shape=none, margin=0, fontsize=8] " + pn m.module
                 #           # + " -> " + pn x + "[color=\"#00000011\",penwidth=0.9]"
                 #           + ";"
                 #       )
                 #       m.refs)
                 # ) modules)}
