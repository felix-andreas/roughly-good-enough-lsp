{
  inputs = {
    systems.url = "github:nix-systems/default";
    nixpkgs.url = "unstable"; # local registry
    devshell.url = "github:numtide/devshell";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      systems,
      nixpkgs,
      devshell,
      rust-overlay,
    }:
    {
      lib = {
        eachSystem = nixpkgs.lib.genAttrs (import systems);
        makePkgs =
          system: nixpkgs':
          import nixpkgs' {
            inherit system;
            overlays = [
              devshell.overlays.default
              (import rust-overlay)
            ];
          };
        rpkgs =
          pkgs: with pkgs.rPackages; [
            renv
            devtools
          ];
      };
      devShells = self.lib.eachSystem (system: {
        default =
          let
            pkgs = self.lib.makePkgs system nixpkgs;
          in
          pkgs.mkShell {
            # motd = "";
            packages = with pkgs; [
              just
              (radianWrapper.override {
                packages = (self.lib.rpkgs pkgs);
                wrapR = true;
              })
              gnumake
              evcxr
              (rust-bin.selectLatestNightlyWith (
                toolchain:
                toolchain.default.override {
                  extensions = [
                    "rust-src"
                    "rust-analyzer"
                  ];
                }
              ))
              # libs
              # pkg-config
            ];
            # env = [
            #   {
            #     name = "LD_LIBRARY_PATH";
            #     prefix = "$DEVSHELL_DIR/lib";
            #   }
            #   {
            #     name = "PKG_CONFIG_PATH";
            #     prefix = "$DEVSHELL_DIR/lib/pkgconfig";
            #   }
            # ];
          };
      });
    };
}
