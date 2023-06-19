{
  description = ''
    A Nix flake for working with Tidal Cycles. https://tidalcycles.org/
  '';

  inputs = {
    utils = {
      url = "github:numtide/flake-utils";
    };
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    tidal-src = {
      url = "github:tidalcycles/tidal/main";
      flake = false;
    };
  };

  outputs = inputs: let
    utils.supportedSystems = [
      "aarch64-linux"
      "i686-linux"
      "x86_64-linux"
      # "aarch64-darwin"
      "x86_64-darwin"
    ];
    utils.eachSupportedSystem =
      inputs.utils.lib.eachSystem utils.supportedSystems;

    mkPackages = pkgs: let
      quarklib = pkgs.callPackage ./quark/lib.nix {};
    in rec {
      ghcWithTidal = pkgs.haskellPackages.ghcWithPackages (p: [p.tidal]);
    };

    overlays = rec {
    };

    mkDevShells = pkgs: tidalpkgs: rec {
      tidal = pkgs.mkShell {
        name = "tidal";
        buildInputs = [
          tidalpkgs.ghcWithTidal
          pkgs.ghcid
        ];
      };
      default = tidal;
    };

    mkOutput = system: let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
    in rec {
      packages = mkPackages pkgs;
      devShells = mkDevShells pkgs packages;
      formatter = pkgs.alejandra;
    };

    # The output for each system.
    systemOutputs = utils.eachSupportedSystem mkOutput;
  in
    # Merge the outputs and overlays.
    systemOutputs // {inherit overlays utils;};
}
