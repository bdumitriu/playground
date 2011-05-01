J2SDK=$(HOME)/.nix-profile
SVN=$(HOME)/.nix-profile
JAVAC=javac
JFLAGS= -classpath . -d .
JPREFIX=gw/storage/bindings

all: $(JPREFIX)/TestJNI.class libsvnra.so

$(JPREFIX)/RAException.class: RAException.java
	$(JAVAC) $(JFLAGS) RAException.java 

$(JPREFIX)/SVNRAStorageLogMessage.class: SVNRAStorageLogMessage.java
	$(JAVAC) $(JFLAGS) ../StorageLogMessage.java ../AbstractStorageLogMessage.java SVNRAStorageLogMessage.java

$(JPREFIX)/SVNJNIAdapter.class: $(JPREFIX)/SVNRAStorageLogMessage.class $(JPREFIX)/RAException.class SVNJNIAdapter.java
	$(JAVAC) $(JFLAGS) ../StorageBlameLine.java SVNJNIAdapter.java

$(JPREFIX)/TestJNI.class: $(JPREFIX)/SVNJNIAdapter.class TestJNI.java
	$(JAVAC) $(JFLAGS) TestJNI.java

libsvnra.so: SVNJNIAdapter.c SVNJNIAdapter.h stack.h stack.c java_classes.h blame.h blame.c
	gcc -Wall -shared -fpic -o libsvnra.so SVNJNIAdapter.c stack.c blame.c \
	  -I$(J2SDK)/include -I$(J2SDK)/include/linux \
	  -I$(SVN)/include/subversion-1 \
	  -I$(SVN)/include/apr-0 \
	  -L$(SVN)/lib -lsvn_client-1

SVNJNIAdapter.h: $(JPREFIX)/SVNJNIAdapter.class
	javah -jni -o $@ gw.storage.bindings.SVNJNIAdapter

run: all
	LD_LIBRARY_PATH=. java gw.storage.bindings.TestJNI 

clean:
	rm -rf ./gw *.so SVNJNIAdapter.h
