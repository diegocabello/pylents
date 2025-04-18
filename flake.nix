{
  description = "pylents - Python and ENTS processing tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Build Haskell parser with proper package management
        haskellParser = pkgs.stdenv.mkDerivation {
          pname = "ents-parser";
          version = "0.1.0";
          src = ./haskell-parser;
          
          buildInputs = [
            (pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
              aeson
              parsec
              containers
              text
            ]))
          ];
          
          buildPhase = ''
            ghc -o parser parser.hs
          '';
          
          installPhase = ''
            mkdir -p $out/bin
            cp parser $out/bin/ents-parser
          '';
        };
        
        pythonEnv = pkgs.python3.withPackages (ps: with ps; [
          xattr
        ]);
        
        # Create wrapper script
        pylentsScript = pkgs.writeShellScriptBin "pylents" ''
          #!/usr/bin/env bash
          
          if [ "$1" == "parse" ] || [ "$1" == "process" ]; then
            ${haskellParser}/bin/ents-parser "$2" temp_tags.json && 
            ${pythonEnv}/bin/python ${./python-implementation}/merge_tags.py temp_tags.json tags.json &&
            rm temp_tags.json &&
            ${pythonEnv}/bin/python ${./python-implementation}/prettyprint.py tags.json 
          else
            ${pythonEnv}/bin/python ${./python-implementation}/pylents.py "$@"
          fi
        '';
      in
      {
        packages = {
          parser = haskellParser;
          python = pythonEnv;
          default = pylentsScript;
        };
        
        apps.default = {
          type = "app";
          program = "${pylentsScript}/bin/pylents";
        };
        
        devShell = pkgs.mkShell {
          buildInputs = [
            haskellParser
            pythonEnv
            pylentsScript
          ];
        };
      });
}
