{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "mmark";

  src = ./mmark.el;

  phases = [ "buildPhase" "installPhase" ];

  buildInputs = [ pkgs.emacs ];

  buildPhase = ''
    cp $src mmark.el
    emacs --batch -f batch-byte-compile mmark.el
  '';

  installPhase = ''
    install -d $out/share/emacs/site-lisp
    install mmark.el* $out/share/emacs/site-lisp
  '';
}
