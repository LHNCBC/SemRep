#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
# CVS: $Id$
#
# NOTE: This Makefile is *not* used anymore. It probably does not work.
# NOTE: This Makefile requires GNU make.
#

SRCDIR=.

# Include common Makefile parts.
include $(SRCDIR)/../../Common

# LIB_PATH=`pwd`/..
LIBDIR=$(LIB_PATH)

.SUFFIXES:

CFLAGS += -I$(SRCDIR)/../../Emulator			\
	-I$(SRCDIR)/../../Emulator/$(PLATFORM)		\
	-I$(SRCDIR)/../../Utils/$(PLATFORM)/include	\
	$(JAVAINC)

JNI_CLASSES= SICStus SPTerm SPPredicate SPQuery SPException

spntative.$(OBJEXT) : $(HDR)

OBJ= spnative.$(OBJEXT) jasper.$(OBJEXT) flinkage.$(OBJEXT)

HDR= $(patsubst %,se_sics_jasper_%.h,$(JNI_CLASSES))

LOCAL_CLASSPATH= $(SRCDIR)/..
ifneq (.$(JAVA_CLASSPATH).,..)
LOCAL_CLASSPATH= $(SRCDIR)/..$(PATHSEP)$(JAVA_CLASSPATH)
endif

all: $(HDR)

all-recursive: $(OBJ)


# $(HDR): se_sics_jasper_%.h: $(SRCDIR)/../../se/sics/jasper/%.class
# 	$(JAVAH) -classpath $(SRCDIR)/../.. -jni se.sics.jasper.$*
# 	touch $@

# [PM] 4.1.2 not tested. The headers are currently built explicitly, instead of being generated for spnative.c by demand.
$(HDR) : se_sics_jasper_%.h : %.java
	$(JAVAH) -classpath $(SRCDIR)/../.. -jni -o '$(call NATIVEPATH, $@)' se.sics.jasper.$*


#$(CLASSES): %.class: %.java
#	$(JAVAC) -classpath '$(LOCAL_CLASSPATH)' $^

#install:
#	-(cd examples;  $(MAKE) $(MFLAGS) LIBDIR=$(LIBDIR) SP_INC=$(SP_INC) install )
#	$(MKDIR) $(LIBDIR)/jasper
#	-$(CP) *.class README $(LIBDIR)/jasper

$(OBJ): %.$(OBJEXT): %.c
	$(CC) $(CFLAGS) -DSPDLL $(INCR_CFLAGS) $(NOLINK_OPT) $(NOLINK_OUTPUT_OPT)$@ $<

clean:
	(cd examples; $(MAKE) clean)
	rm -f *~ *.$(OBJEXT) jasper_*.h



