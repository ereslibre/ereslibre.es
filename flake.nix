{
  inputs = {
    devenv.url = "github:cachix/devenv";
    nixities.url = "github:ereslibre/nixities";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, devenv, nixities, systems, ... }@inputs:
    let forEachSystem = nixities.nixpkgs.lib.genAttrs (import systems);
    in {
      devShells = forEachSystem (system:
        let pkgs = nixities.legacyPackages.${system};
        in {
          default = devenv.lib.mkShell {
            inherit pkgs;
            inputs.nixpkgs = nixities.nixpkgs;
            modules = [{
              packages = with pkgs; [ emacs-nox git just python3 ];

              enterShell = ''
                echo 'Run ~make~ or ~make publish~'
              '';
            }];
          };
        });
    };
}
