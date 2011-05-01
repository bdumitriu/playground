source $stdenv/setup

tar zxvf $src || fail
ensureDir $out/jars || fail
mv emma-*/lib/*.jar $out/jars || fail
