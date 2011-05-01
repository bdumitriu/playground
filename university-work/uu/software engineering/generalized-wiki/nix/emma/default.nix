{stdenv, fetchurl}:

stdenv.mkDerivation {
  name = "emma-unknown-version";
  builder = ./builder.sh;
  src = ./emma-unknown-version.tar.gz;
}
