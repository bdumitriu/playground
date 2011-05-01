customBuildOptions :

  { stdenv, jdk, jetty
  , name
  , webapps, httpapps ? []
  , port ? 8080, sslKey ? null, sslSupport ? true
  , minThreads ? 5, maxThreads ? 500, libraryPath ? []
  , runtimeInputs ? []
  } :

stdenv.mkDerivation ({
  inherit name;
  builder = ./builder.sh;

  paths = map (webapp : webapp.path) webapps;
  wars  = map (webapp : webapp.war)  webapps;
  httppaths = map (httpapp : httpapp.path) httpapps;
  httpbases = map (httpapp : httpapp.resoucebase) httpapps;
  
  inherit jdk jetty libraryPath runtimeInputs;
  inherit sslSupport port minThreads maxThreads;

} // (if sslSupport then
       {inherit (sslKey) store storepass keypass;}
      else {})
 // customBuildOptions)
