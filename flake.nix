{
  description = "Hopper Nix Flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/23.05";

  outputs = { self, nixpkgs }:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = self: super: {
        hsPkgs = super.haskell.packages.ghc944.override {
          overrides = hself: hsuper: {
            ghcid = super.haskell.lib.dontCheck hsuper.ghcid;
            pinch = hsuper.callCabal2nix "pinch" (builtins.fetchGit {
              url = "https://github.com/abhinav/pinch.git";
              rev = "c392bbce4534e98df07a4e80102efb59b12d8529";
            }) {};
            hopper = hsuper.callCabal2nix "hopper" ./hopper {};
            hopper-thrift = hsuper.callCabal2nix "hopper-thrift" ./hopper-thrift {};
          };
        };
        hopper-distributed = self.hsPkgs.callCabal2nix "hopper-distributed" ./hopper-distributed {};
      };
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in
          {
            default = pkgs.hopper-distributed;
          }
      );
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          libs = with pkgs; [
            zlib
          ];
        in
        {
          default = pkgs.hsPkgs.shellFor {
            packages = hsPkgs: [ ];
            buildInputs = with pkgs; [
              hsPkgs.cabal-install
              hsPkgs.cabal-fmt
              hsPkgs.ghcid
              hsPkgs.ghc
              hopper-distributed
            ] ++ libs;
            shellHook = "export PS1='[$PWD]\n❄ '";
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
          };
        });
    };
}
