# -*- Mode:makefile -*-
#
# Copyright (C) 2015 SICS Swedish ICT
#

SRCDIR=.
SRCROOT = $(SRCDIR)/../..

.PHONY: default
default: all

# Include common Makefile parts.
include $(SRCROOT)/Common

ifeq ($(ENABLE_DOTNET),yes)

.PHONY: all clean maintainerclean
all clean maintainerclean: 
ifeq ($(ENABLE_DOTNETCSHARP),yes)
	$(MAKE) -f Makefile_csharp $@
endif # ifeq ($(ENABLE_DOTNETCSHARP),yes)

ifeq ($(ENABLE_DOTNETJSHARP),yes)
	$(MAKE) -f Makefile_jsharp $@
endif # ifeq ($(ENABLE_DOTNETJSHARP),yes)

endif # ifeq ($(ENABLE_DOTNET),yes)

