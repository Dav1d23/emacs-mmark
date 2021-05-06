{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "mmark";

  src = [
    ./mmark.el
    ./mmark-test.el
    ./run_test.sh
  ];

  phases = [
    "unpackPhase"
    "testPhase"
    "buildPhase"
    "installPhase"
  ];

  buildInputs = [ pkgs.emacs ];

  unpackPhase = ''
    for srcFile in $src; do
      cp $srcFile $(stripHash $srcFile)
    done
  '';

  testPhase = ''
    bash run_test.sh
  '';

  buildPhase = ''
    emacs --batch -f batch-byte-compile mmark.el
  '';

  installPhase = ''
    install -d $out/share/emacs/site-lisp
    install mmark.el* $out/share/emacs/site-lisp
  '';
}
