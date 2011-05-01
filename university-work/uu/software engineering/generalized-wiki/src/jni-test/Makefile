J2SDK=/home/eelco/.nix-profile
SVN=/home/eelco/.nix-profile

Main.class: *.java
	javac *.java

libsubversion.so: Subversion.c Subversion.h
	gcc -Wall -shared -fpic -o libsubversion.so Subversion.c \
	  -I$(J2SDK)/include -I$(J2SDK)/include/linux \
	  -I$(SVN)/include/subversion-1 \
	  -I$(SVN)/include/apr-0 \
	  -L$(SVN)/lib -lsvn_client-1

Subversion.h: Subversion.java
	javah -jni -classpath . Subversion

run: Main.class libsubversion.so
	LD_LIBRARY_PATH=. java Main

clean:
	rm -f *.class *.so Subversion.h
