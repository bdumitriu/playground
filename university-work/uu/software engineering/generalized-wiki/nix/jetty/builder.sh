set -e

. $stdenv/setup

# Hack: this is required for the buildfarm
ensureDir $out/nix-support
echo "$system" > $out/nix-support/system

unpackPhase=jettyUnpackPhase
jettyUnpackPhase() {
  echo "nothing to do here."
  sourceRoot=.
}

installPhase=jettyInstallPhase
jettyInstallPhase() {
  ensureDir $out
  ensureDir $out/bin
  ensureDir $out/etc

  libPath=
  for path in $libraryPath
  do
    libPath="$libPath:$path"
  done

  runtimePath=$(x= ; for i in $runtimeInputs; do x="$x${x:+:}$i/bin"; done; echo $x)

  cat >> $out/bin/start-jetty <<EOF
#! /bin/sh

export CLASSPATH=
export JETTY_HOME=$jetty
export JAVA_HOME=$jdk
export LANG="en_US"
export PATH=$runtimePath
#export LD_ASSUME_KERNEL=2.2.5

\$JAVA_HOME/bin/java -Djava.library.path=$libPath -Djetty.home=\$JETTY_HOME -jar \$JETTY_HOME/start.jar $out/etc/server.xml
EOF

  cat >> $out/etc/server.xml <<EOF
<?xml version="1.0"?>
<!DOCTYPE Configure PUBLIC "-//Mort Bay Consulting//DTD Configure 1.2//EN" "http://jetty.mortbay.org/configure_1_2.dtd">

<Configure class="org.mortbay.jetty.Server">
  <Call name="addListener">
    <Arg>
EOF

  if test "$sslSupport"; then
    cat >> $out/etc/server.xml <<EOF
      <New class="org.mortbay.http.SunJsseListener">
        <Set name="Port">$port</Set>
        <Set name="Keystore">$store</Set>
        <Set name="Password">$storepass</Set>
        <Set name="KeyPassword">$keypass</Set>
      </New>
EOF
  else
    cat >> $out/etc/server.xml <<EOF
      <New class="org.mortbay.http.SocketListener">
        <Set name="Port">$port</Set>
      </New>
EOF
  fi

  cat >> $out/etc/server.xml <<EOF
    </Arg>
  </Call>
EOF

  i=0
  for p in $paths
  do
    path[i]=$p
    i=`expr $i + 1`
  done

  i=0
  for w in $wars
  do
    war[i]=$w
    i=`expr $i + 1`
  done

  i=0
  for hp in $httppaths
  do
    hpath[i]=$hp
    i=`expr $i + 1`
  done

  i=0
  for hrb in $httpbases
  do
    hbase[i]=$hrb
    i=`expr $i + 1`
  done

  echo "number of paths: ${#path[*]}"
  echo "number of wars: ${#war[*]}"
  echo "number of httppaths: ${#hpath[*]}"
  echo "number of httpresoucebases: ${#hbase[*]}"

  i=0
  while [ "$i" -lt "${#war[*]}" ]
  do
    cat >> $out/etc/server.xml <<EOF
  <Call name="addWebApplication">
    <Arg>${path[i]}</Arg>
    <Arg>${war[i]}</Arg>

    <Set name="extractWAR">true</Set>
    <Set name="defaultsDescriptor">org/mortbay/jetty/servlet/webdefault.xml</Set>
  </Call>
EOF
    i=`expr $i + 1`
  done

  i=0
  while [ "$i" -lt "${#hpath[*]}" ]
  do
    cat >> $out/etc/server.xml <<EOF
  <Call name="addContext">
    <Arg>
      <New class="org.mortbay.http.HttpContext">
        <Set name="ContextPath">${hpath[i]}</Set>
        <Set name="ResourceBase">${hbase[i]}</Set>
        <Call name="addHandler">
	  <Arg>
	    <New class="org.mortbay.http.handler.ResourceHandler" />
	  </Arg>
	</Call>
      </New>
    </Arg>
  </Call>
EOF
    i=`expr $i + 1`
  done

  cat >> $out/etc/server.xml <<EOF
</Configure>
EOF

  chmod a+x $out/bin/start-jetty
}

phases="installPhase"
genericBuild
