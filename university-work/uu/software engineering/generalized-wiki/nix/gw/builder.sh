set -e

. $stdenv/setup

# Hack: this is required for the buildfarm
ensureDir $out/nix-support
echo "$system" > $out/nix-support/system

unpackPhase=gwUnpackPhase
gwUnpackPhase() {
  echo "nothing to do here."
  sourceRoot=.
}

installPhase=gwInstallPhase
gwInstallPhase() {
  mkdir lib
  mkdir build

  builddir=`pwd`/build

  # Libraries that required at compile and runtime
  #ln -s $jdomjar ./lib/
  ln -s $lucenejar ./lib/ 
  ln -s $fileuploadjar ./lib/
  ln -s $jingjar ./lib/
  #ln -s $svnjar ./lib/

  # Libraries that are only required for testing
  mkdir test-lib
  ln -s $junit/junit.jar ./test-lib/
  ln -s $httpunit/lib/httpunit.jar ./test-lib/
  ln -s $httpunit/jars/js.jar ./test-lib/
  ln -s $httpunit/jars/nekohtml.jar ./test-lib/
  ln -s $httpunit/jars/Tidy.jar ./test-lib/
  ln -s $mockobjects/mockobjects-core-* ./test-lib/
  ln -s $mockobjects/mockobjects-jdk1.4-j2ee1.3-*.jar ./test-lib/
  # ln -s $emma/jars/emma* ./test-lib

  cp $jdom/lib/jaxen-core.jar ./lib/
  cp $jdom/lib/jaxen-jdom.jar ./lib/
  cp $jdom/lib/saxpath.jar ./lib/
  cp $jdomjar ./lib/
  cp $svnjar ./lib/
  cp $javasvn ./lib/

  cp -R $src/* .

  export NIX_ANT_OPTS="-Xmx256m"
  
  # hack, hack, hack
  $ant/bin/ant \
    -lib $testviz/testviz-analysis.jar \
    -lib $testviz/testviz-retrieval.jar \
    -lib $testviz/lib/dom4j-1.5.2.jar \
    -lib $testviz/lib/hsqldb/hsqldb.jar \
    -lib $testviz/lib/xerces.jar \
    -lib $testviz/lib/xalan.jar \
    -lib $junit/junit.jar \
    -Djunit=$junit \
    -Djunitjar=$junit/junit.jar \
    -Dservletsjar=$servletsjar \
    -Dbuilddir=$builddir \
    -Dlibdir=$(pwd)/lib \
    -Dtestlibdir=$(pwd)/test-lib \
    -Dversion=$version \
    -Dtestviz.home=$testviz/ \
    -Dpath.prefix=file:// \
    -Ddistdir=$out \
    -Dsubversion=$subversion \
    -Dcoreutils=$coreutils \
    $target || fail

  # If we have generated testviz reports ...
  if test -e "$out/docs/testviz/testresults.xml"; then
    prevdir=$(pwd)
    cd $out/docs || fail
    # hack to make the link at the release page work.
    mv testviz coverage || fail
    tar jcf $out/coverage.tar.bz2 coverage  || fail
    cd $prevdir  || fail
  fi
}

phases="installPhase"
genericBuild
