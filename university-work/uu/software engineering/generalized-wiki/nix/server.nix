customBuildOptions : pkgs :

rec {
  body =
    (import ./jetty) customBuildOptions {
      name = "gw-server";

      webapps = [
          { path = "/gw"; war=gw ~ "/gw.war" ; }
        ];

       httpapps = [
	   { path = "/reports"; resoucebase=gw ~ "/docs/junitreport"; }
         ];

      sslSupport = false;
      inherit (pkgs) stdenv jetty jdk;
      libraryPath = [(pkgs.subversionWithJava ~ "lib")];

      port = 8080;

      runtimeInputs =
        [pkgs.strategoxt pkgs.bibtextools pkgs.libxslt];
    };

  gw =
    ((import ./gw) customBuildOptions pkgs).war;

  coverage =
    ((import ./gw) customBuildOptions pkgs).testviz;
}
