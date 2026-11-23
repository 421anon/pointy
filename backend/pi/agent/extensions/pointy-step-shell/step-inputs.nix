step:
let
  argValues = builtins.concatLists (
    map (value: if builtins.isList value then value else [ value ]) (builtins.attrValues step.meta.pointy.args)
  );
  dependencies = builtins.filter (value: builtins.isAttrs value && (value.meta.pointy.id or null) != null) argValues;
in
builtins.concatStringsSep "" (
  map (dependency: "dependency ${toString dependency.meta.pointy.id} ${dependency.outPath}\n") dependencies
  ++ map (package: "package - ${package.outPath}\n") (step.devPackages or [ ])
)
