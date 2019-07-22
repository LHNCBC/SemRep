#! /usr/bin/make -f
#
include ../Makefile.include
QPC=qpc
QLD=qld
QLDFLAGS=-S -v -g

LEXICON_BASE=$(SKR_SRC_HOME)/lexicon
LEXICON=$(LEXICON_BASE)/lexicon
MORPH=$(LEXICON_BASE)/morph

METAMAP=$(SKR_SRC_HOME)/metamap
MMI=$(SKR_SRC_HOME)/mmi
SKR=$(SKR_SRC_HOME)/skr
SKR_DB=$(SKR_SRC_HOME)/db
SKR_LIB=$(SKR_SRC_HOME)/lib
SKR_TEXT=$(SKR_SRC_HOME)/text
TAGGER=$(SKR_SRC_HOME)/tagger
WSD=$(SKR_SRC_HOME)/WSD/WSD

QPATH= \
	-f lexicon:$(LEXICON) \
	-f mm_tools_lib:$(MM_TOOLS_LIB) \
	-f metamap:$(METAMAP) \
	-f morph:$(MORPH)     \
	-f mmi:$(MMI)         \
	-f skr:$(SKR)         \
	-f skr_db:$(SKR_DB)   \
	-f skr_lib:$(SKR_LIB) \
	-f tagger:$(TAGGER)   \
	-f text:$(SKR_TEXT)   \
	-f wsd:$(WSD)

O_SYSTEM = $(shell uname)
ifeq '$(O_SYSTEM)' "SunOS"
  QLDENDFLAGS=
  QLDLIBS=-lm -lsocket -lnsl -lrt -d
else
  ifeq '$(O_SYSTEM)' "Linux"
    QLDENDFLAGS=-LD -m32
    QLDLIBS=-lm -lpthread -d
  endif
endif

COPT=-c
QPOPT=-v
OUTPUT_FILE=a.out
QOF_FILE=loader.qof
PROLOG_FILE=loader.pl
QPCFLAGS=$(QPOPT) $(COPT) $(QPATH)

# $SKR_DEBUG ane $NLS_CLNT_LIB are set in SKRenv.08
DEBUG_LIB=$(SKR_DEBUG)/debug.a
LINK_LIBS=$(BERKELEY_LIB) $(DEBUG_LIB)

%.qof : %.pl
	$(QPC) $(QPCFLAGS) -o $@ $<

%.out : %.qof
	$(QLD) $(QLDFLAGS) -o $@ $(QLDLIBS) $(QPATH) $< $(LINK_LIBS) $(QLDENDFLAGS)

all : a.out

$(OUTPUT_FILE) : $(QOF_FILE)
	$(QLD) $(QLDFLAGS) -o $@ $(QLDLIBS) $(QPATH) $< $(LINK_LIBS) $(QLDENDFLAGS) 

clean:
	rm -f *.qof *.out *.o
