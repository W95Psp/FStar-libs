{
  doc = ''
Here is a centralized repo for some F* "libraries" I use. 

These are packed into sort of packages using https://github.com/W95Psp/fstar-nix-packer
  '';
  ToString = import ./ToString;
  DefaultValue = import ./DefaultValue;
  PartialOrder = import ./PartialOrder;
  Data = import ./Data;
  MkDoc = import ./MkDoc;
  NormTools = import ./NormTools;
  MetaTools = import ./MetaTools;
  Control = import ./Control;
  FStar-Tactics-JS = import ./FStar-Tactics-JS;
}
