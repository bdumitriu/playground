{stdenv, fetchurl}:

stdenv.mkDerivation {
  name = "testviz-0.1.2";
  builder = ./builder.sh;
  src = fetchurl {
    url = http://losser.st-lab.cs.uu.nl/~pmuilwij/testviz-0.1.2.tar.gz;
    md5 = "f9bb9576daa4c89d5c7fee9827640c73";
  };
}
