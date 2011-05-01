customBuildOptions : pkgs :

rec {
  war =
    build "install";

  dist =
    build "dist";

  test =
    build "all-report";

  testviz =
    build "testviz";

  build = target :
    pkgs.stdenv.mkDerivation ({
      name = "gw";
      version = "0.4";

      builder = ./builder.sh;
      inherit target;

      src = ../../src;
      ant = pkgs.apacheAnt;

      # build-time dependency
      servletsjar = pkgs.jetty ~ "lib/javax.servlet.jar";

      javasvn = pkgs.javasvn ~ "javasvn.jar";
      buildInputs = [pkgs.graphviz];

      inherit (pkgs) junit mockobjects httpunit commonsFileUpload jdom subversion coreutils;

      # run-time dependencies
      svnjar = pkgs.subversionWithJava ~ "lib/svn-javahl/svn-javahl.jar";

      fileuploadjar =
        pkgs.commonsFileUpload ~ "commons-fileupload-1.0.jar";

      jingjar =
        pkgs.jing ~ "bin/jing.jar";

      lucenejar =
        pkgs.lucene ~ "lucene-1.4.1.jar";

      jdomjar =
        pkgs.jdom ~ "build/jdom.jar";

      testviz =
        import ../testviz {
          inherit (pkgs) fetchurl stdenv;
        };
  }
  // customBuildOptions);
}
