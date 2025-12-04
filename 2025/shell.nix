{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  packages = with pkgs; [
    # Python
    python3
    # Haskell
    ghc
    haskell-language-server
    # Zig
    zig
    zls
    # Julia
    (julia-bin.withPackages ["LanguageServer" "SymbolServer" "StaticLint" "DSP"])
  ];
}
