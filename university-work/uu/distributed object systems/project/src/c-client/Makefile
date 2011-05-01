PREFIX=/usr
CC=gcc
ORBIT_IDL=$(PREFIX)/bin/orbit-idl-2
CFLAGS=$(shell pkg-config ORBit-2.0 ORBit-CosNaming-2.0 --cflags)
LDFLAGS=$(shell pkg-config ORBit-2.0 ORBit-CosNaming-2.0 --libs)

TARGETS=calendar-client
IDLOUT=Calendar-common.c Calendar-stubs.c Calendar.h
CALENDARIDLPREFIX=..

all: $(IDLOUT) calendar-client

calendar-client : calendar-client.o Calendar-common.o Calendar-stubs.o

$(IDLOUT): $(CALENDARIDLPREFIX)/Calendar.idl
	$(ORBIT_IDL) --noskels $(CALENDARIDLPREFIX)/Calendar.idl

clean:
	rm -rf *.o *~ $(IDLOUT)

distclean: clean
	rm -rf calendar-client
