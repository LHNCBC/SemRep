
#
# Copyright (C) 1988-2010 Swedish Institute of Computer Science.
#
#
# NOTE: This Makefile requires GNU make.
#
# NOTE2: If you get this make file as part of a binary installation
# then it may serve as an example but it is unlikely to work "as is". 
#

SRCDIR=.

LIBRARY_OUTPUT_PREFIX = $(SRCDIR_PREFIX)

ifneq ($(SRCDIR),.)
SRCDIR_PREFIX = $(SRCDIR)/

# VPATH = $(SRCDIR)
vpath %.pl $(SRCDIR)
vpath %.c $(SRCDIR)
vpath %.po $(LIBRARY_OUTPUT_PREFIX)
else				# SRCDIR = .
# No explicit vpath
# empty SRCDIR_PREFIX
SRCDIR_PREFIX =
endif				# SRCDIR .



# [PM] 4.1.3+ We use second expansion with order-only prerequisites
# (using $(dir $@), suitably quoted) to automatically create necessary
# target directories. All cases are (redundantly) marked with explicit
# mention of .SECONDEXPANSION:
.SECONDEXPANSION:

.PHONY: default
default: all

# Include common Makefile parts.
include $(SRCDIR_PREFIX)../Common

ifneq ($(FORCE_PARALLEL),yes)
.NOTPARALLEL: # [PM] 4.2.1+ make -j does not work here (conflicts in splfr glue code handling)
endif

.PHONY: library_examples
library_examples: jasper_examples prologbeans_examples


# [PM] WinCE compensate for invalid clock on WinCE device.
ifeq ($(WINCE),yes)

# [PM] 3.11.2 sometimes on WinCE the file does not get created (or
# gets created empty?) but there is no error code.
#  -s FILE     FILE exists and has a size greater than zero
TOUCH_SP_TARGET = { [ -s '$@' ] && touch '$@'; } || { echo '$@ was not created' 1>&2; exit 1; }
endif				# WINCE

VERIFY_TARGET_CREATED = @ [ -f $@ ] || { echo "ERROR: Target $@ was not created" 1>&2; exit 1; }

.SUFFIXES:
.SUFFIXES: .pl .po .c .$(OBJEXT) $(EXEEXT) .$(FLI_SHSFX) .$(STSFX)


# [PM] stuff that is not used from prolog but is still part of
## library, e.g prologbeansdotnet (but prologbeansdotnet is Win32 only
## so added later)
OTHER=

# [MC] removed math.po
ALL_MODULES= \
	aggregate.po \
        alignments.po \
	arrays3.po \
	assoc.po \
	assoc3.po \
	atts.po \
	avl.po \
	bags.po \
	between.po \
	clpb.po \
	codesio.po \
	csv.po \
	detcheck.po \
	determinacy.po \
	det.po \
	disassembler.po \
	fastrw.po \
	file_systems.po \
        fligen.po \
	heaps.po \
	lists.po \
	lists3.po \
	logarr.po \
	mkindex.po \
	mzn_sicstus.po \
	nondetdecl.po \
	obj_decl.po \
	objects.po \
	ordsets.po \
	plunit.po \
	process.po \
	queues.po \
	queues3.po \
	random.po \
	random3.po \
	rem.po \
	resgen.po \
	samsort.po \
	sets.po \
	sockets.po \
	statistics.po \
	str_decl.po \
	structs.po \
	SU_messages.po \
	system.po \
	system3.po \
	terms.po \
	trees.po \
	types.po \
	ugraphs.po \
	varnumbers.po \
	wgraphs.po \
	xml.po \
	xref.po \
	$(OPROFILE_MODULE) \
	$(PERF_MODULE) \
	$(BDB_MODULE) \
	$(TIMEOUT_MODULE) \
        $(COMCLIENT_MODULE) \
        $(TCLTK_MODULE)


ALL_MODULES += \
	linda/client.po \
	linda/server.po \
	pillow.po \
	prologbeans.po \
	prologbeansserver.po \

# .NET modules (including Mono, on non-Win32)

ifeq ($(ENABLE_DOTNET),yes)
OTHER += prologbeansdotnet
endif

# Modules only supported under Win32


ifeq ($(WIN32),yes)
COMCLIENT_MODULE = comclient.po
endif				# WIN32

# [PM] 3.9 it is now possible to turn off clpfd
ifeq ($(ENABLE_CLPFD),yes)
	CLPFD_MODULE=clpfd.po
	ALL_MODULES += zinc/zinc_utils.po zinc/flatzinc_parser.po
	ALL_MODULES += zinc/flatzinc.po zinc/minizinc.po zinc.po
	ALL_MODULES += zinc/zinc_tests.po
	ALL_MODULES += fz.po
	ALL_MODULES += fdbg.po
endif

ALL_MODULES += $(ODBC_MODULE)

ifneq ($(TCLTK_MODULE),)
ALL_MODULES += gauge.po
endif

ALL_MODULES += clpq.po
ALL_MODULES += clpr.po
ALL_MODULES += $(CLPFD_MODULE)

ALL_MODULES += $(CHR_MODULE)


# Modules requiring -m (i.e, those that load Java/jasper)
JAVA_MODULES += $(JASPER_MODULE)
ALL_MODULES += $(JAVA_MODULES)


# Dependencies between .po files
$(LIBRARY_OUTPUT_PREFIX)aggregate.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)arrays3.po:		logarr.po
$(LIBRARY_OUTPUT_PREFIX)assoc.po:		
$(LIBRARY_OUTPUT_PREFIX)assoc3.po:		avl.po
$(LIBRARY_OUTPUT_PREFIX)atts.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)avl.po:			
$(LIBRARY_OUTPUT_PREFIX)bags.po:		
$(LIBRARY_OUTPUT_PREFIX)bdb.po:			types.po fastrw.po
modules_that_need[fastrw.po] += bdb.po
$(LIBRARY_OUTPUT_PREFIX)between.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)chr.po:			types.po lists.po atts.po avl.po ordsets.po terms.po structs.po
$(LIBRARY_OUTPUT_PREFIX)clpb.po:		atts.po lists.po
$(LIBRARY_OUTPUT_PREFIX)clpfd.po:		types.po lists.po atts.po avl.po ordsets.po trees.po samsort.po timeout.po
modules_that_need[timeout.po] += clpfd.po
$(LIBRARY_OUTPUT_PREFIX)clpq.po $(LIBRARY_OUTPUT_PREFIX)clpr.po: 	types.po ordsets.po atts.po terms.po assoc3.po ugraphs.po
modules_that_need[ugraphs.po] += $(LIBRARY_OUTPUT_PREFIX)clpq.po clpr.po
$(LIBRARY_OUTPUT_PREFIX)codesio.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)csv.po:			lists.po
$(LIBRARY_OUTPUT_PREFIX)comclient.po:		lists.po
$(LIBRARY_OUTPUT_PREFIX)detcheck.po:		determinacy.po
$(LIBRARY_OUTPUT_PREFIX)determinacy.po:		ordsets.po
$(LIBRARY_OUTPUT_PREFIX)det.po:			determinacy.po
$(LIBRARY_OUTPUT_PREFIX)disassembler.po:	lists.po types.po
$(LIBRARY_OUTPUT_PREFIX)fastrw.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)fdbg.po: 		lists.po sockets.po codesio.po avl.po atts.po clpfd.po
$(LIBRARY_OUTPUT_PREFIX)file_systems.po:	types.po system.po
$(LIBRARY_OUTPUT_PREFIX)fligen.po:		types.po lists.po
$(LIBRARY_OUTPUT_PREFIX)fz.po:			random.po zinc.po
$(LIBRARY_OUTPUT_PREFIX)gauge.po: 		lists.po tcltk.po context.pl 
$(LIBRARY_OUTPUT_PREFIX)heaps.po:		
$(LIBRARY_OUTPUT_PREFIX)jasper.po:		types.po lists.po 
$(LIBRARY_OUTPUT_PREFIX)jserver.po:		types.po sockets.po codesio.po system.po avl.po
$(LIBRARY_OUTPUT_PREFIX)lists.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)lists3.po:		lists.po
$(LIBRARY_OUTPUT_PREFIX)linda/client.po:	fastrw.po sockets.po types.po
modules_that_need[fastrw.po] += linda/client.po
$(LIBRARY_OUTPUT_PREFIX)linda/server.po:	fastrw.po sockets.po lists.po
modules_that_need[fastrw.po] += linda/server.po
$(LIBRARY_OUTPUT_PREFIX)logarr.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)math.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)mkindex.po:		lists.po file_systems.po system.po
$(LIBRARY_OUTPUT_PREFIX)nondetdecl.po:		
$(LIBRARY_OUTPUT_PREFIX)obj_decl.po:		lists.po alignments.po
$(LIBRARY_OUTPUT_PREFIX)objects.po:		types.po structs.po
ifneq ($(ODBC_MODULE),)
$(LIBRARY_OUTPUT_PREFIX)$(ODBC_MODULE):		codesio.po lists.po types.po
endif				# ODBC
$(LIBRARY_OUTPUT_PREFIX)ordsets.po:		
$(LIBRARY_OUTPUT_PREFIX)pillow.po:		lists.po sockets.po
$(LIBRARY_OUTPUT_PREFIX)plunit.po:		terms.po types.po
$(LIBRARY_OUTPUT_PREFIX)prologbeans.po:		lists.po terms.po fastrw.po codesio.po system.po prologbeansserver.po
modules_that_need[fastrw.po] += prologbeans.po
$(LIBRARY_OUTPUT_PREFIX)prologbeansserver.po:	sockets.po
$(LIBRARY_OUTPUT_PREFIX)queues.po:		
$(LIBRARY_OUTPUT_PREFIX)queues3.po:		queues.po
$(LIBRARY_OUTPUT_PREFIX)random.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)random3.po:		random.po assoc3.po
modules_that_need[random.po] += random3.po
$(LIBRARY_OUTPUT_PREFIX)rem.po:			between.po
$(LIBRARY_OUTPUT_PREFIX)resgen.po:		types.po lists.po file_systems.po
$(LIBRARY_OUTPUT_PREFIX)samsort.po:		
$(LIBRARY_OUTPUT_PREFIX)sets.po:		lists.po
$(LIBRARY_OUTPUT_PREFIX)sockets.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)str_decl.po:		alignments.po
$(LIBRARY_OUTPUT_PREFIX)statistics.po:		lists.po
$(LIBRARY_OUTPUT_PREFIX)structs.po:		types.po alignments.po
$(LIBRARY_OUTPUT_PREFIX)SU_messages.po:		
$(LIBRARY_OUTPUT_PREFIX)system.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)system3.po:		system.po file_systems.po sockets.po lists.po
$(LIBRARY_OUTPUT_PREFIX)terms.po:		types.po avl.po
$(LIBRARY_OUTPUT_PREFIX)timeout.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)trees.po:		
$(LIBRARY_OUTPUT_PREFIX)types.po:		
$(LIBRARY_OUTPUT_PREFIX)ugraphs.po:		types.po lists.po ordsets.po avl.po random.po
modules_that_need[random.po] += ugraphs.po $(modules_that_need[ugraphs.po])
$(LIBRARY_OUTPUT_PREFIX)varnumbers.po:		types.po
$(LIBRARY_OUTPUT_PREFIX)wgraphs.po:		types.po lists.po ordsets.po avl.po heaps.po random.po
modules_that_need[random.po] += wgraphs.po
$(LIBRARY_OUTPUT_PREFIX)xml.po:			types.po lists.po
$(LIBRARY_OUTPUT_PREFIX)xref.po:		avl.po
$(LIBRARY_OUTPUT_PREFIX)zinc/zinc_utils.po:		lists.po types.po clpfd.po
$(LIBRARY_OUTPUT_PREFIX)zinc/flatzinc_parser.po:	lists.po zinc/zinc_utils.po
$(LIBRARY_OUTPUT_PREFIX)zinc/flatzinc.po:		avl.po clpfd.po codesio.po lists.po random.po structs.po terms.po timeout.po types.po zinc/zinc_utils.po zinc/flatzinc_parser.po str_decl.po
modules_that_need[random.po] += zinc/flatzinc.po $(modules_that_need[zinc/flatzinc.po])
modules_that_need[timeout.po] += zinc/flatzinc.po $(modules_that_need[zinc/flatzinc.po])
$(LIBRARY_OUTPUT_PREFIX)zinc/minizinc.po:		file_systems.po lists.po process.po types.po zinc/zinc_utils.po zinc/flatzinc.po
modules_that_need[zinc/flatzinc.po] += zinc/minizinc.po $(modules_that_need[zinc/minizinc.po])
$(LIBRARY_OUTPUT_PREFIX)zinc.po:			zinc/flatzinc.po zinc/minizinc.po
modules_that_need[zinc/flatzinc.po] += zinc.po $(modules_that_need[zinc.po])
modules_that_need[zinc/minizinc.po] += zinc.po $(modules_that_need[zinc.po])
$(LIBRARY_OUTPUT_PREFIX)zinc/zinc_tests.po:		file_systems.po lists.po zinc.po
modules_that_need[zinc.po] += zinc/zinc_tests.po

# ------------------------------------------------------------------------------
# Stuff to do before building...
# ------------------------------------------------------------------------------

# [PM] 4.1.3+ Modules that have foreign code in subdirs (e.g. bdb.po
# should go here but _not_ chr.po)
FOREIGN_SUBDIR_MODULES= $(CLPFD_MODULE) $(JASPER_MODULE) $(TCLTK_MODULE) $(BDB_MODULE) $(COMCLIENT_MODULE) $(PERF_MODULE)

# [MC] 4.0beta6 additional subdirs that should be installed
EXTRA_ORIG_FILES_DIRS = chr/orig_files chr/orig_files/examples chr/orig_files/errors chr/orig_files/warnings clpqr/examples clpqr/examples/monash

# ------------------------------------------------------------------------------
# Some library modules want soft-links here and there...
# ------------------------------------------------------------------------------
CLPQR_COMMONS=compenv.pl arith.pl bb.pl bv.pl dump.pl fourmotz.pl \
	   ineq.pl itf3.pl nf.pl ordering.pl project.pl redund.pl store.pl
CLPQ_LINKS= $(patsubst %.pl,clpq/%.pl,$(CLPQR_COMMONS))
CLPR_LINKS= $(patsubst %.pl,clpr/%.pl,$(CLPQR_COMMONS))

SOFTLINKS=$(CLPQ_LINKS) $(CLPR_LINKS)

ifeq ($(WIN32),yes)
$(CLPQ_LINKS): clpq/%.pl: clpqr/%.pl
$(CLPR_LINKS): clpr/%.pl: clpqr/%.pl
$(CLPR_LINKS) $(CLPQ_LINKS):
	($(RM) -f $@; $(CP) -f $< $@)
else
$(addprefix $(SRCDIR_PREFIX),$(CLPQ_LINKS)): $(SRCDIR_PREFIX)clpq/%.pl: $(SRCDIR_PREFIX)clpqr/%.pl
	pwd; $(RM) -f $@ && $(LN) ../clpqr/$(<F) $@
$(addprefix $(SRCDIR_PREFIX),$(CLPR_LINKS)): $(SRCDIR_PREFIX)clpr/%.pl: $(SRCDIR_PREFIX)clpqr/%.pl
	pwd; $(RM) -f $@ && $(LN) ../clpqr/$(<F) $@
endif

$(LIBRARY_OUTPUT_PREFIX)clpq.po: $(CLPQ_LINKS)
$(LIBRARY_OUTPUT_PREFIX)clpr.po: $(CLPR_LINKS$) # (addprefix $(SRCDIR_PREFIX),$(CLPR_LINKS))

# [PM] 3.12.2 more silent by default
SICSTUS_SILENT_FLAGS := --nologo --noinfo
SICSTUS_FLAGS = -f $(SICSTUS_SILENT_FLAGS)

# [PM] make it possible to override for cross compiling
SICSTUS_FOR_LIBRARY = $(SICSTUS)
SICSTUS_FOR_LIBRARY_FLAGS = $(SICSTUS_FLAGS)

SICSTUS_FOR_LIBRARY_CHR ?= $(SICSTUS_FOR_LIBRARY)
SICSTUS_FOR_LIBRARY_CHR_FLAGS ?= $(SICSTUS_FOR_LIBRARY_FLAGS)

# [PM] 4.0 When we go final we should not allow any known broken library modules
ifeq ($(SICSTUS_RELEASE_BUILD), no)
ALLOW_KNOWN_BAD_LIBRARIES := yes
endif				# !SICSTUS_RELEASE_BUILD
# [PM] 4.2 Allow bad libs if --enable-force-build
ifeq ($(SICSTUS_FORCE_BUILD), yes)
ALLOW_KNOWN_BAD_LIBRARIES := yes
endif				# 


ifeq ($(ALLOW_KNOWN_BAD_LIBRARIES), yes)
QA_BARF = :
else
QA_BARF = printf "Module \"%s\" was disapproved%s for QA reasons, cannot continue (see disapproved.txt)\n" "$(@F)" "$${d_by:+ by $${d_by}}" 1>&2; exit 1
endif

define BUILD_LEGEND
	@printf "\nBuilding %s " "$@"; \
	 if [ -f approved.txt ]; then \
	   a_by=`sed -n -e 's/#.*^//' -e 's/^$(basename $(@F))[ \t][ \t]*\([a-zA-Z0-9][a-zA-Z0-9]*\).*$$/\1/p' < approved.txt`; \
	   d_by=`sed -n -e 's/#.*^//' -e 's/^$(basename $(@F))[ \t][ \t]*\([a-zA-Z0-9][a-zA-Z0-9]*\).*$$/\1/p' < disapproved.txt`; \
	   d_cmnt=`sed -n -e 's/^$(basename $(@F))[ \t][ \t]*\([a-zA-Z0-9][a-zA-Z0-9]*\)[ \t]*#[ \t]*\(..*\)$$/\2/p' < disapproved.txt`; \
	 else \
	   a_by=default; \
	   d_by=''; \
	   d_cmnt=''; \
	 fi; \
	 if [ -n "$${d_by}" ]; then \
	   if [ -n "$${a_by}" ]; then \
	     printf "!!! QA REJECTED by %s but approved by %s, fight it out will you!\n" "$${d_by}" "$${a_by}"; \
	   else \
	     printf "!!! QA REJECTED BY %s%s !!\n" "$${d_by}" "$${d_cmnt:+ (}$${d_cmnt}$${d_cmnt:+)}"; \
	   fi; \
	   $(QA_BARF) ; \
	 elif [ -n "$${a_by}" ]; then \
	   if [ "default" != "$${a_by}"  ]; then printf "(approved by %s) ...\n" "$${a_by}"; else printf "...\n"; fi \
	 else \
	   printf " Not QA-approved! (see approved.txt)\n"; \
	 fi; \
	 printf "=========================================\n\n"
endef


clpfd_extra_files = clpfd/fdsets clpfd/ixq clpfd/enum clpfd/compiler \
                   clpfd/lib clpfd/automaton clpfd/geost clpfd/mddi
clpfd.po: $(addsuffix .pl, $(clpfd_extra_files))

clpq_extra_files = clpq/arith clpq/arith_q clpq/itf3 clpq/store \
                   clpq/geler clpq/nf clpq/nfq clpq/ordering \
                   clpq/class clpq/project clpq/bv clpq/ineq \
                   clpq/redund clpq/fourmotz clpq/bb clpq/dump 
clpq_extra_options = ignore_warning(import(_,nfq,clpq,private)) ignore_warning(import(_,classq,clpq,private))
clpq.po: $(addsuffix .pl, $(clpq_extra_files))

clpr_extra_files = clpr/arith clpr/arith_r clpr/itf3 \
                   clpr/store clpr/geler clpr/nf clpr/nfr \
                   clpr/ordering clpr/class clpr/project clpr/bv \
                   clpr/ineq clpr/redund clpr/fourmotz clpr/bb \
                   clpr/dump
clpr_extra_options = ignore_warning(import(_,nfr,clpr,private)) ignore_warning(import(_,classr,clpr,private))
clpr.po: $(addsuffix .pl, $(clpr_extra_files))

chr_extra_files = chr/b_globval chr/hprolog chr/hpattvars \
                  chr/chr_runtime chr/pairlist chr/chr_hashtable_store \
                  chr/binomialheap chr/find chr/a_star chr/listmap \
                  chr/clean_code chr/builtins chr/guard_entailment \
                  chr/chr_compiler_errors chr/chr_compiler_options \
                  chr/chr_compiler_utility chr/chr_translate

# [PM] 4.1.3+ chr.po should not depend on chr_extra_files since we do
# not have proper make rules for these. Instead rely on the default
# dependency on chr.pl which does depend on the proper files.
#
# chr.po: $(addsuffix .pl, $(chr_extra_files))

# [PM] 4.1.3+ Used to save context as part of gauge.po. Why was this
# done? # Did, in effect:
# gauge_extra_file = context


$(JAVA_MODULES): MKLIBS_ENV = $(JASPER_ENV)
$(PERF_MODULE):  MKLIBS_ENV = env SP_PERF_DISABLE=yes

$(addprefix $(LIBRARY_OUTPUT_PREFIX),$(ALL_MODULES)): $(LIBRARY_OUTPUT_PREFIX)%.po: %.pl
	$(BUILD_LEGEND)
	echo "make('$<', \
                   $(call MAKE_PROLOG_LIST_QUOTED_ELEMENTS, $(addsuffix .pl, $(addprefix $(SRCDIR_PREFIX),$($(*)_extra_files)))), \
                   $(call MAKE_PROLOG_LIST, output('$@') $($(*)_extra_options))), \
              halt;halt(42)." \
        | $(MKLIBS_ENV) $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) $($(*)_extra_sicstus_options) -l $(SRCDIR_PREFIX)mklibs $(SICSTUS_MKLIBS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

$(SRCDIR_PREFIX)chr.pl: $(sort $(SRCDIR_PREFIX)chr/orig_files/chr_swi.pl $(wildcard $(SRCDIR_PREFIX)chr/orig_files/*.pl))
	$(MAKE) -C $(SRCDIR_PREFIX)chr all 'SICSTUS=$(SICSTUS_FOR_LIBRARY_CHR)' 'SICSTUS_FLAGS=$(SICSTUS_FOR_LIBRARY_CHR_FLAGS)'

# C flags for foreign resources. Make sure the latest sicstus.h is
# found first by placing prefix/include first.
CFLAGS:= -I'$(call NATIVEPATH, $(prefix)/include)' $(CFLAGS)


# [PD] 3.11.3
.PHONY: prologbeansdotnet
prologbeansdotnet:
	$(MAKE) -C prologbeans.NET

# .PHONY: prologbeanssharp
# prologbeanssharp:
# 	$(MAKE) -C prologbeanssharp



# ------------------------------------------------------------------------------
# Definition of foreign resources.
# ------------------------------------------------------------------------------

# TODO: fix dependencies from C source to header files.

# These definitions are used when expanding the makefile template
# resname_makefile_TEMPLATE, below. Their name is very significant:
# <module>_src etc. is used to construct dependencies for
# <module>.<shsfx>.
# The .c suffix should not be present

math_src=math
codesio_src=codesio

ifneq ($(TIMEOUT_MODULE_FOREIGN),no)
timeout_src=timeout
endif				# TIMEOUT_MODULE_FOREIGN != no

random_src=random
fastrw_src=fastrw
oprofile_src=oprofile
perf_src = perf/perf

# [PM] 4.3 We trigger a (soft) assert if oprofile is already active
# when building the oprofile library. To avoid this we forcibly hide
# any SP_SPTI_PATH value from the environment.
oprofile_extra_sicstus_options=-DSP_SPTI_PATH=none

# [PM] 4.3.2+ Probably a good idea to turn of perf profiler when building perf module.
perf_extra_sicstus_options=-DSP_SPTI_PATH=none

spti_oprofile_src = spti_oprofile
ifneq ($(OPROFILE_MODULE),)
# Only build if OProfile is available
SPTI_MODULES += spti_oprofile
endif				# OPROFILE_MODULE

spti_perf_src = perf/spti_perf
ifneq ($(PERF_MODULE),)
# Only build if PERF is available
SPTI_MODULES += spti_perf
endif				# PERF_MODULE


spti_verbose_src = spti_verbose
# Always build this
SPTI_MODULES += spti_verbose

# structs_src=structs
$(patsubst %,$(PLATFORM)/%_d.$(OBJEXT),$(fastrw_src)) : fastrw.h
$(patsubst %,$(PLATFORM)/%_s.$(OBJEXT),$(fastrw_src)) : fastrw.h

tcltk_src=tcltk/tcl tcltk/tk tcltk/tkappini tcltk/tkterm tcltk/util
comclient_src=comclient/comclient

clpfd_src= \
clpfd/ac3intervals \
clpfd/alldifferent \
clpfd/alldistinct \
clpfd/arith \
clpfd/bc_alldiff \
clpfd/ct \
clpfd/bin_packing \
clpfd/disjoint2 \
clpfd/discrete \
clpfd/dvars \
clpfd/fdsets \
clpfd/gcc \
clpfd/geost \
clpfd/indexical \
clpfd/keysorting \
clpfd/lcc \
clpfd/lex \
clpfd/lh \
clpfd/lin \
clpfd/linfast \
clpfd/linpbool \
clpfd/linubool \
clpfd/main \
clpfd/mddi \
clpfd/ttef_cumulative \
clpfd/multi_cumulative \
clpfd/nvalue \
clpfd/profile \
clpfd/relation \
clpfd/setsingleton \
clpfd/sorting \
clpfd/statistics \
clpfd/support \
clpfd/symmcum \


# [PM] 3.9b4 (almost) all CLPFD files depend on fd.h, dvars.h, qsort.ic
$(patsubst %,$(PLATFORM)/%_d.$(OBJEXT),$(clpfd_src)) : clpfd/fd.h clpfd/dvars.h clpfd/qsort.ic
$(patsubst %,$(PLATFORM)/%_s.$(OBJEXT),$(clpfd_src)) : clpfd/fd.h clpfd/dvars.h clpfd/qsort.ic

# [PM] 4.0 everything in tcltk depends on tcl.h
$(patsubst %,$(PLATFORM)/%_d.$(OBJEXT),$(tcltk_src)) : tcltk/tcl.h
$(patsubst %,$(PLATFORM)/%_s.$(OBJEXT),$(tcltk_src)) : tcltk/tcl.h

jasper_src=jasper/jasper
bdb_src = bdb/hash bdb/bdb bdb/ixkeys bdb/bdb_sync
# [PM] 4.3 no longer use dbaux.c (only for profiling, on Windows. Needs rewrite.)
# bdb_src += bdb/dbaux 
ifneq ($(ODBC_MODULE),)
odbc_src=odbc
endif				# ODBC

tcltk_lib=$(TCLLIB)
bdb_lib=$(BDBLIB)
ifneq ($(ODBC_MODULE),)
odbc_lib=$(ODBCLIB)
endif				# ODBC
jasper_lib=$(JAVALIB)
comclient_lib=$(COMCLIENTLIB)

#################################################################################
######## Tcl/Tk

## [PM] 3.9 Added -I. to CFLAGS for all resources that reside in
##          subdirs in order to find <RESOURCE>_glue.h
tcltk_copt=$(TCLINC) -I.

# [PM] 3.9b4 Tcl/Tk headers are broken. they should say that their API uses __cdecl
ifeq ($(WIN32),yes)
   tcltk_copt += -Gd
   tcltk_d_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(tcltk_copt)))
   tcltk_s_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(tcltk_copt)))
endif				# WIN32


# [PM] 4.4.0 with llvm project clang the warnings about non-prototypes
# in X11 headers is back. The real question is why they do not give a
# warning with the Apple clang (Update, they _do_ give a warning with
# Apple clang in Xcode 9 (Apple LLVM version 9.0.0
# (clang-900.0.37)). Suppressing the warning for tcltk module should
# not hurt as long as the compiler supports the option.
#
ifeq ($(CC_IS_GCC),yes)		# GCC or CLANG
# [PM] 4.1.3+ Still needed on Mac OS X Tk uses broken X11 headers from 1993.
# [PM] 3.9b4 Older X11 headers are not fully prototyped, even
# if __STDC__ In particular this happens on scheutz (SunOS
# 5.7, X11 headers from 1991) We could check the headers, X11
# on Linux does the right thing.

tcltk_copt += -Wno-strict-prototypes

endif				# GCC or CLANG



#################################################################################
######## Berkeley DB


# [PM] 4.1 Berkeley DB 4.8.24 is built with VS 8 so uses 64-bit time_t.
#      FIXME: Should have a generic test for whether BDB uses 32 or 64 bit time_t (or BDB could get its headers in order...)
# ifeq ($(WIN32),yes)
#
#   # VS 8 uses 64bit time_t by default but the BDB installer is built
#   # with VS 7.1 which used 32bit time_t. Try to work around it by
#   # forcing VS 8 to go back the old behavior
#   bdb_defines += -D_USE_32BIT_TIME_T
# endif

bdb_copt=-DSICSTUS $(bdb_defines) $(BDBINC) -I.

# [PM] 3.9b4 bdb headers are broken. they should say that their API uses__cdecl
ifeq ($(WIN32),yes)
   bdb_copt += -Gd

   bdb_d_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(bdb_copt)))
   bdb_s_splfr_copt = $(patsubst %,--cflag=%$(space),$(filter-out -Gd, $(bdb_copt)))

endif				# WIN32

#################################################################################
######## Jasper


jasper_copt = 
jasper_copt += $(NO_UNKNOWN_PRAGMAS) $(JAVAINC) -I.

ifneq ($(findstring darwin-10.8, $(PLATFORM)), )
# [PM] 4.2.2 The Mac OS X 10.6 jni.h header has deprecation attributes on some functions. (this attribute is not present in 10.5 or 10.7).
jasper_copt += -Wno-deprecated-declarations
endif # Mac OS X 10.6
jasper_copt += $(NO_UNKNOWN_PRAGMAS)
jasper_copt += $(JAVAINC)
jasper_copt += -I.

# NOTE: (lib)jasper_splfrflags should match what is used by
#        InstallSICStus.in ([PM] 3.8.6)



jasper_splfrflags=$(JASPER_SPLFR_FLAGS)

# NOTE: (lib)jasper_splfrflags should match what is used by
#        InstallSICStus.in ([PM] 3.8.6)

jasper_d_copt = $(jasper_copt)
jasper_d_copt += -DMULTI_SP_AWARE
jasper_d_splfrflags = $(jasper_splfrflags)
jasper_d_splfrflags += --multi-sp-aware

#################################################################################
######## codesio
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
codesio_d_copt = $(codesio_copt)
codesio_d_copt += -DMULTI_SP_AWARE
codesio_d_splfrflags = $(codesio_splfrflags)
codesio_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS


#################################################################################
######## CLPFD

SP_INSIDER_COPTS = -DSP_INSIDER -DLOCAL_INCLUDES -I$(SRCDIR_PREFIX)../Emulator -I$(SRCDIR_PREFIX)../Emulator/$(PLATFORM) -I.

clpfd_copt = $(SP_INSIDER_COPTS)

# [PM] 4.0.5 Always true: ENABLE_CLPFD_MULTI_SP != no
# ifneq ($(ENABLE_CLPFD_MULTI_SP),no)
clpfd_d_copt = $(clpfd_copt)
clpfd_d_copt += -DMULTI_SP_AWARE
clpfd_d_splfrflags = $(clpfd_splfrflags)
clpfd_d_splfrflags += --multi-sp-aware
# endif				# ENABLE_CLPFD_MULTI_SP != no

ifeq ($(ENABLE_CLPFD_MULTI_SP),always)
# [PM] 4.0.5 Use MULTI_SP API for clpfd even if static foreign resource
clpfd_s_copt = $(clpfd_copt)
clpfd_s_copt += -DMULTI_SP_AWARE
clpfd_s_splfrflags = $(clpfd_splfrflags)
clpfd_s_splfrflags += --multi-sp-aware
endif				# ENABLE_CLPFD_MULTI_SP == always



#################################################################################
######## fastrw
fastrw_copt = -DSICSTUS
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
fastrw_d_copt = $(fastrw_copt)
fastrw_d_copt += -DMULTI_SP_AWARE
fastrw_d_splfrflags = $(fastrw_splfrflags)
fastrw_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## math

ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
math_d_copt = $(math_copt)
math_d_copt += -DMULTI_SP_AWARE
math_d_splfrflags = $(math_splfrflags)
math_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## random
ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
random_d_copt = $(random_copt)
random_d_copt += -DMULTI_SP_AWARE
random_d_splfrflags = $(random_splfrflags)
random_d_splfrflags += --multi-sp-aware
endif				# ENABLE_MULTITHREADED_LIBS

#################################################################################
######## odbc
# [PM] 4.3 ODBCINC is C compiler flags, not just include directory paths
# odbc_copts += $(addprefix -I, $(odbc_includes))
odbc_copts += $(ODBCINC)
odbc_d_copt = $(odbc_copts)
odbc_s_copt = $(odbc_copts)

# library(odbc) is not multi-sp aware
# ifeq ($(ENABLE_MULTITHREADED_LIBS),yes)
# odbc_d_copt += -DMULTI_SP_AWARE
# odbc_d_splfrflags = $(odbc_splfrflags)
# odbc_d_splfrflags += --multi-sp-aware
# endif				# ENABLE_MULTITHREADED_LIBS


#################################################################################
######## oprofile

# Not multi-threaded aware yet.

# Make the SPTI_OnLoad/SPTI_OnUnLoad visible
oprofile_d_splfrflags += --no-hide-symbols
oprofile_d_splfrflags += $(OPAGENT_LIB)


# Make the SPTI_OnLoad/SPTI_OnUnLoad visible
spti_oprofile_d_splfrflags += --no-hide-symbols
spti_oprofile_d_splfrflags += $(OPAGENT_LIB)

# Make the SPTI_OnLoad/SPTI_OnUnLoad visible
spti_verbose_d_splfrflags += --no-hide-symbols

#################################################################################
######## perf

# Not multi-threaded aware yet.

# Make the SPTI_OnLoad/SPTI_OnUnLoad visible
perf_d_splfrflags += --no-hide-symbols

# Make the SPTI_OnLoad/SPTI_OnUnLoad visible
spti_perf_d_splfrflags += --no-hide-symbols
# We fake mult-sp-aware since we need to access the API via event.spenv
spti_perf_d_copt += -DMULTI_SP_AWARE
spti_perf_d_splfrflags += --multi-sp-aware
# No extra libs needed?
# spti_perf_d_splfrflags += $(???_LIB)

#################################################################################
######## timeout
ifeq ($(ENABLE_PTHREAD_TIMEOUT),yes)
# [PM] 3.9b4 the new pthread based timeout. Very experimental
timeout_copt += -DLEGACY_TIMOUT=0
endif
# [PM] 3.11.2 needs Emulator/win32getimes.ic
timeout_copt += -I$(SRCDIR_PREFIX)../Emulator

#################################################################################
######## structs


############################################################################################

# ------------------------------------------------------------------------------
# Build rules for foreign resources
# ------------------------------------------------------------------------------

# FOREIGN_MODULES define which modules get a shared/static resource
# built with them. 
FOREIGN_MODULES= \
	math.po random.po codesio.po \
	fastrw.po \
	$(CLPFD_MODULE) $(TCLTK_MODULE) \
	$(BDB_MODULE) $(COMCLIENT_MODULE) \
	$(JASPER_MODULE) $(ODBC_MODULE) $(OPROFILE_MODULE) $(PERF_MODULE)

ifneq ($(TIMEOUT_MODULE_FOREIGN),no)
$(error [PM] 4.4.0 the timeout foreign resource is no longer available)
endif				# TIMEOUT_MODULE_FOREIGN != no

ifneq ($(TIMEOUT_MODULE_FOREIGN),no)
FOREIGN_MODULES += timeout.po
endif


# This rule says: every foreign module has an associated shared resource
# and an optional static resource.
$(addprefix $(LIBRARY_OUTPUT_PREFIX),$(FOREIGN_MODULES)): $(LIBRARY_OUTPUT_PREFIX)%.po: $(LIBRARY_OUTPUT_PREFIX)$(PLATFORM)/%.$(FLI_SHSFX)
ifeq ($(ENABLE_STATLIB),yes)
$(addprefix $(LIBRARY_OUTPUT_PREFIX),$(FOREIGN_MODULES)): $(LIBRARY_OUTPUT_PREFIX)%.po: $(LIBRARY_OUTPUT_PREFIX)$(PLATFORM)/%.$(STSFX)
endif

# [PM] 4.1.3+ construct the real MODULES list.
ifeq ($(DISABLE_FOREIGN_RESOURCES),yes)
# [PM] 4.1.3+ modules_that_need[XXX.po] is a hack for modules
# that depend on a module XXX.po that needs foreign code. We could/should
# generalize this
BUILT_MODULES_ = $(filter-out $(FOREIGN_MODULES) $(modules_that_need[random.po]) $(if $(filter no, $(TIMEOUT_MODULE_FOREIGN)),,$(modules_that_need[timeout.po])) $(modules_that_need[fastrw.po]), $(ALL_MODULES))
else				# !DISABLE_FOREIGN_RESOURCES
BUILT_MODULES_ = $(ALL_MODULES)
endif				# !DISABLE_FOREIGN_RESOURCES

BUILT_MODULES = $(addprefix $(LIBRARY_OUTPUT_PREFIX),$(BUILT_MODULES_))

BUILT_FOREIGN_SUBDIR_MODULES_ = $(filter $(FOREIGN_SUBDIR_MODULES),$(BUILT_MODULES_))
BUILT_FOREIGN_SUBDIR_MODULES = $(addprefix $(LIBRARY_OUTPUT_PREFIX),$(BUILT_FOREIGN_SUBDIR_MODULES_))

BUILT_FOREIGN_PLATFORM_DIRS_ = $(patsubst %.po,$(PLATFORM)/%,$(BUILT_FOREIGN_SUBDIR_MODULES_))
BUILT_FOREIGN_PLATFORM_DIRS = $(addprefix $(LIBRARY_OUTPUT_PREFIX),$(BUILT_FOREIGN_PLATFORM_DIRS_))


# [PM] 4.1.3+ target dirs now created on demand
# .PHONY: dirs
# dirs: $(BUILT_FOREIGN_PLATFORM_DIRS)


.SECONDEXPANSION:
# [PM] 4.2 Ensure library/$(PLATFORM) is not indexed by SPIDER. This
# is mainly so that we can put sys_mod.pl, and other meta-info files,
# there without SPIDER picking it up accidentally)
$(LIBRARY_OUTPUT_PREFIX)$(PLATFORM)/.spider_data: | $$(dir $$@)
	rm -f $@
	printf '<?xml version="1.0" encoding="UTF-8"?>\n' >> $@
	printf '<se.sics.sicstus.spider.data>\n' >> $@
	printf '  <!-- Do not index anything in this folder -->\n' >> $@
	printf '  <fileProperties name=".">\n' >> $@
	printf '    <property key="indexed" value="false"/>\n' >> $@
	printf '  </fileProperties>\n' >> $@
	printf '</se.sics.sicstus.spider.data>\n' >> $@

.SECONDEXPANSION:
# [PM] 4.2 Place definition of prolog module (and other Bips, eventually) in
# devsys_info. We should add more info to it, eventually.
# We need to use write_canonical to avoid the problem with operator as
# argument of operator which SPIDER/ISO forbids ([PM] 4.3 not a
# problem in SPIDER anymore, but we do not want to depend on the
# current operators).
$(LIBRARY_OUTPUT_PREFIX)devsys_info: $(SRCDIR_PREFIX)../Bips/sys_mod.pl $(SRCDIR_PREFIX)../Bips/sys_mod_det.pl $(SRCDIR_PREFIX)../Bips/spider_c_initial_bips.pl | $(if $(LIBRARY_OUTPUT_PREFIX), $$(dir $$@) )
$(LIBRARY_OUTPUT_PREFIX)devsys_info: $(SRCDIR_PREFIX)../Bips/sys_mod.pl $(SRCDIR_PREFIX)../Bips/common_meta_predicates.pl $(SRCDIR_PREFIX)../Bips/sys_mod_det.pl $(SRCDIR_PREFIX)../Bips/spider_c_initial_bips.pl | $(if $(LIBRARY_OUTPUT_PREFIX), $$(dir $$@) )
	printf '/* -*- Mode:Prolog; buffer-read-only:t -*-\n  DO NOT EDIT! THIS CODE IS GENERATED FROM\n  $^\n*/\n' > $@
	sed -e 's/\(:-.*include(.*\)/% PREPROCESSED AWAY: \1/' $< >> $@
	cat $(filter %/common_meta_predicates.pl,$^) >> $@
	cat $(filter %/sys_mod_det.pl,$^) >> $@
	echo '% Meta predicate bips.' >> $@
	$(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) --goal "((predicate_property(prolog:Head, meta_predicate(Meta)), functor(Head, F, _), F \== call, \+ predicate_property(user:Head, _), writeq((:- meta_predicate(prolog:Meta))), write(' .'), nl, fail);halt)." >> $@
	$(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) --goal "M=prolog, (findall(F/A, (prolog:inline_codable_bip(M:P), functor(P,F,A)), Inlined), ( Inlined = [_|_] -> write('\n%% These are inlined and should not be counted when computing call-number from source code.\n'), write_canonical((:- inlined_bip(Inlined))), write('.'), nl; true)), halt." >> $@
	echo '% Other pre-defined predicates. Not auto-imported.' >> $@
	$(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) --goal "(open('$(filter %/spider_c_initial_bips.pl,$^)', read, S), /*retractall(head(_,_)),*/ repeat, read(S,T), (T = end_of_file -> close(S); T=M:P, M\==prolog, assert(head(M,P)), fail)), findall(M,head(M,_),Ms1), sort(Ms1, Modules), (member(M, Modules), findall(N/A, (head(M,P), functor(P,N,A)), Exports), write_canonical((:- module(M,Exports))), write('.'), nl, ((Exports=[_|_], write('%% Assume all pre-defined predicates are semi-determinate\n'), member(DetFA,Exports), write_canonical((:- is(DetFA,semidet))), write(' .\n'), fail);true), (findall(F/A, (prolog:inline_codable_bip(M:P), functor(P,F,A)), Inlined), ( Inlined = [_|_] -> write('\n%% These are inlined and should not be counted when computing call-number from source code.\n'), write_canonical((:- inlined_bip(Inlined))), write('.'), nl; true)), fail; halt)." >> $@


##### CLPFD GENERATED HEADERS BEGIN
## To add an additional include file (bar.ic, say), duplicate the
## foo.ic BEGIN...END block and replace foo.ic with bar.ic (and change
## its recipe and prerequisites). The things outside foo.ic
## BEGIN...END should not need to change.

CLPFD_GENERATED_HEADERS_DIR := $(LIBRARY_OUTPUT_PREFIX)$(PLATFORM)/clpfd

### foo.ic BEGIN
# [PM] 4.3 Dummy prerequisite for clpfd C codes, for Mats to adapt. The dependency on lists.pl is just an example. The generation of foo.ic is just an example.
# C code should use #include "foo.ic" (without directory)
# creates output directory on demand
# .SECONDEXPANSION:
# $(CLPFD_GENERATED_HEADERS_DIR)/foo.ic: arith_codegen.pl | $$(dir $$@)
# 	echo "$(BARF_ON_ERROR) ensure_loaded('$<'),length([a,b],X),open('$@',write,S),write(S, '/* -*- Mode:C; buffer-read-only:t -*-'),nl(S),write(S,'This file is automatically generated, do not edit */'),nl(S), write(S,'#define MY_DEFINE '),write(S,X),nl(S),close(S),halt." | $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS)
# 	@ echo 'Resulting $@:'; cat $@ # echo the resulting file, for debugging only.
# clpfd_generated_includes += $(CLPFD_GENERATED_HEADERS_DIR)/foo.ic
### foo.ic END


.SECONDEXPANSION:
$(CLPFD_GENERATED_HEADERS_DIR)/mul-iteration.ic: arith_codegen.pl | $$(dir $$@)
	echo 'codegen(mul).' | $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) -l $< > $@
clpfd_generated_includes += $(CLPFD_GENERATED_HEADERS_DIR)/mul-iteration.ic

.SECONDEXPANSION:
$(CLPFD_GENERATED_HEADERS_DIR)/quo-iteration.ic: arith_codegen.pl | $$(dir $$@)
	echo 'codegen(quo).' | $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) -l $< > $@
clpfd_generated_includes += $(CLPFD_GENERATED_HEADERS_DIR)/quo-iteration.ic

.SECONDEXPANSION:
$(CLPFD_GENERATED_HEADERS_DIR)/div-iteration.ic: arith_codegen.pl | $$(dir $$@)
	echo 'codegen(div).' | $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) -l $< > $@
clpfd_generated_includes += $(CLPFD_GENERATED_HEADERS_DIR)/div-iteration.ic

.SECONDEXPANSION:
$(CLPFD_GENERATED_HEADERS_DIR)/rem-iteration.ic: arith_codegen.pl | $$(dir $$@)
	echo 'codegen(rem).' | $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) -l $< > $@
clpfd_generated_includes += $(CLPFD_GENERATED_HEADERS_DIR)/rem-iteration.ic

.SECONDEXPANSION:
$(CLPFD_GENERATED_HEADERS_DIR)/mod-iteration.ic: arith_codegen.pl | $$(dir $$@)
	echo 'codegen(mod).' | $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) -l $< > $@
clpfd_generated_includes += $(CLPFD_GENERATED_HEADERS_DIR)/mod-iteration.ic

# ... generated_includes_dir is passed with -I to C compiler
clpfd_generated_includes_dir = $(CLPFD_GENERATED_HEADERS_DIR)
GENERATED_INCLUDES_DIRS += $(CLPFD_GENERATED_HEADERS_DIR)

##### CLPFD GENERATED HEADERS END

# ------------------------------------------------------------------------------


DUMMY_RESOURCE_NAMES += timeout
timeout_pl := $(LIBRARY_OUTPUT_PREFIX)$(PLATFORM)/dummy_timeout.pl
GENERATED_FILES += $(timeout_pl)
timeout_src := timeout_placeholder

$(timeout_pl) :
	printf '/* -*- Mode:Prolog; buffer-read-only:t -*-\n  DO NOT EDIT! THIS CODE IS GENERATED\n*/\n' > $@
	printf '%s\n' ":- module(timeout, [])." >> $@
	printf '%s\n' "foreign(to_start_timer_a, '$start_timer_a'(+term, +term, [-integer]))." >> $@
	printf '%s\n' "foreign(to_stop_timer_a, '$stop_timer_a'([-term]))." >> $@
	printf '%s\n' "foreign(to_timer_now, '$timer_now'([-term]))." >> $@
	printf '%s\n' "foreign(to_clocks_per_second, '$clocks_per_second'([-integer]))." >> $@
	printf '%s\n' "foreign_resource(timeout, [" >> $@
	printf '%s\n' "        init(to_init)," >> $@
	printf '%s\n' "        deinit(to_deinit)," >> $@
	printf '%s\n' "        to_start_timer_a, to_stop_timer_a, to_timer_now," >> $@
	printf '%s\n' "        to_clocks_per_second" >> $@
	printf '%s\n' "                          ])." >> $@


DUMMY_RESOURCES += $(patsubst %, $(LIBRARY_OUTPUT_PREFIX)$(PLATFORM)/%.$(FLI_SHSFX), $(DUMMY_RESOURCE_NAMES))
ifeq ($(ENABLE_STATLIB),yes)
DUMMY_RESOURCES += $(patsubst %, $(LIBRARY_OUTPUT_PREFIX)$(PLATFORM)/%.$(STSFX), $(DUMMY_RESOURCE_NAMES))
endif

# ------------------------------------------------------------------------------


# [PM] 4.1.3+ Note: In make 3.82 the define syntax allows an optional
# equal sign after the variable name but this does not work in 3.81.

# [PM] 4.1.3+ This is similar to what the genmakefile script used to
# output but as an eval-uable function
define resname_makefile_TEMPLATE # args: (resname, shared-suffix)
ifeq ($$($(1)_d_copt),)
$(1)_d_copt := $$($(1)_copt)
endif
ifeq ($$($(1)_s_copt),)
$(1)_s_copt := $$($(1)_copt)
endif
ifeq ($$($(1)_d_splfr_copt),)
$(1)_d_splfr_copt := $$(patsubst %,--cflag=%$$(space),$$($(1)_d_copt))
endif
ifeq ($$($(1)_s_splfr_copt),)
$(1)_s_splfr_copt := $$(patsubst %,--cflag=%$$(space),$$($(1)_s_copt))
endif
ifeq ($$($(1)_d_splfrflags),)
$(1)_d_splfrflags := $$($(1)_splfrflags)
endif
ifeq ($$($(1)_s_splfrflags),)
$(1)_s_splfrflags := $$($(1)_splfrflags)
endif
# Special hack to ensure that a -Gr (__fastcall) on CFLAGS can be overridden by $(1)_cflags_XXX
$(1)_cflags_static := $$(CFLAGS)
$(1)_cflags_dynamic := $$(CFLAGS)
$(1)_incr_cflags := $$(INCR_CFLAGS)

ifeq ($$(WIN32),yes)

ifneq ($$(strip $$(filter -Gd /Gd, $$($(1)_d_copt))),) # /Gd or -Gd (use __cdecl calling convention)
$(1)_cflags_dynamic := $$(filter-out -Gr /Gr, $$(CFLAGS)) # remove any "use __fastcall" option
$(1)_incr_cflags := $$(filter-out -Gr /Gr, $$(INCR_CFLAGS)) # remove any "use __fastcall" option
endif				# -Gd specified

ifneq ($$(strip $$(filter -Gd /Gd, $$($(1)_s_copt))),) # /Gd or -Gd (use __cdecl calling convention)
$(1)_cflags_static := $$(filter-out -Gr /Gr, $$(CFLAGS)) # remove any "use __fastcall" option
endif				# -Gd specified
endif				# WIN32


# Dynamic object file
$(1)_objs_d := $$(addprefix $$(LIBRARY_OUTPUT_PREFIX)$$(PLATFORM)/, $$(addsuffix _d.$$(OBJEXT), $$($(1)_src)))
# Static object file
$(1)_objs_s := $$(addprefix $$(LIBRARY_OUTPUT_PREFIX)$$(PLATFORM)/, $$(addsuffix _s.$$(OBJEXT), $$($(1)_src)))

.SECONDEXPANSION:
# Build dynamic object files
# creates $(PLATFORM)/subdir/ on demand
$$($(1)_objs_d): $$(LIBRARY_OUTPUT_PREFIX)$$(PLATFORM)/%_d.$$(OBJEXT): %.c $$($(1)_generated_includes) $$(LIBRARY_OUTPUT_PREFIX)$$(PLATFORM)/$(1)_glue.h | $$$$(dir $$$$@)
	$$(CC) $$(addprefix -I,$$(dir $$(filter %_glue.h,$$^))) $$(addprefix -I,$$($(1)_generated_includes_dir)) -DMakefile_after_glue_I1 $$($(1)_cflags_dynamic) $$($(1)_d_copt) -DSPDLL $$($(1)_incr_cflags) $$< $$(NOLINK_OPT) $$(NOLINK_OUTPUT_OPT)$$@

.SECONDEXPANSION:
# Build static object files
# creates $(PLATFORM)/subdir/ on demand
$$($(1)_objs_s): $$(LIBRARY_OUTPUT_PREFIX)$$(PLATFORM)/%_s.$$(OBJEXT): %.c $$($(1)_generated_includes) $$(LIBRARY_OUTPUT_PREFIX)$$(PLATFORM)/$(1)_glue.h | $$$$(dir $$$$@)
	echo '$$$$<=$$<, $$$$^=$$^, $$$$@=$$@'
	$$(CC) $$(addprefix -I,$$(dir $$(filter %_glue.h,$$^))) $$(addprefix -I,$$($(1)_generated_includes_dir)) -DMakefile_after_glue_I2 $$($(1)_cflags_static) $$($(1)_s_copt) $$< $$(NOLINK_OPT) $$(NOLINK_OUTPUT_OPT)$$@

.SECONDEXPANSION:
# Build extern declarations for foreign/2,3 facts
$$(LIBRARY_OUTPUT_PREFIX)$$(PLATFORM)/$(1)_glue.h: $(if $($(1)_pl),$($(1)_pl),$(1).pl) | $$$$(dir $$$$@)
	@echo
	@echo "Building $$@..."
	@echo =========================================
	@echo
	$$(MACOSX_MALLOC_SCRIBBLE_KLUDGE) $$(SPLFR) $$(SPLFRFLAGS) --header='$$@' --resource=$(1) --namebase=$$(@D)/$$(basename $$(@F)) $$($(1)_d_splfrflags) $$(filter %.$$(OBJEXT) %.pl,$$^) --nocompile 


# Builds a shared resource
$$(LIBRARY_OUTPUT_PREFIX)$$(PLATFORM)/$(1).$(2): $(if $($(1)_pl),$($(1)_pl),$(1).pl) $$($(1)_objs_d)
	@echo
	@echo "Building $$@..."
	@echo =========================================
	@echo
	rm -f so_locations
	$$(MACOSX_MALLOC_SCRIBBLE_KLUDGE) $$(SPLFR) $$(SPLFRFLAGS) --header=$(1)_tmp_header.h --resource=$(1) --namebase=$$(@D)/$$(basename $$(@F)) $$($(1)_d_splfrflags) $$(filter %.$$(OBJEXT) %.pl,$$^) $$($(1)_lib) $$(SPLFR_CFLAGS) $$($(1)_d_splfr_copt) -o $$@
	rm -f $(1)_tmp_header.h

# Builds a static resource
$$(LIBRARY_OUTPUT_PREFIX)$$(PLATFORM)/$(1).$$(STSFX): $(if $($(1)_pl),$($(1)_pl),$(1).pl) $$($(1)_objs_s)
	@echo
	@echo "Building $$@..."
	@echo =========================================
	@echo
	$$(MACOSX_MALLOC_SCRIBBLE_KLUDGE) $$(SPLFR) $$(SPLFRFLAGS) --header=$(1)_tmp_header.h --resource=$(1) --namebase=$$(@D)/$$(basename $$(@F)) --static $$($(1)_s_splfrflags) $$(filter %.$$(OBJEXT) %.pl,$$^) $$($(1)_lib) $$(SPLFR_CFLAGS) $$($(1)_s_splfr_copt) -o $$@
	rm -f $(1)_tmp_header.h
endef # resname_makefile_TEMPLATE

# [PM] 4.2.1 Note: vpath directories must exist when the make file is
# processed, so we can not do "vpath spti_% $(PLATFORM)", see
# <http://www.mail-archive.com/help-make@gnu.org/msg06252.html>. Also
# adding "vpath whatever $(PLATFORM)" breaks the make file for
# unrelated targets, like fastrw_glue.h.

define spti_makefile_TEMPLATE # args: (resname, shared-suffix)
$(1)_pl := $(1).pl
GENERATED_FILES += $$($(1)_pl)
$$($(1)_pl):
	printf '/* -*- Mode:Prolog; buffer-read-only:t -*-\n  DO NOT EDIT! THIS CODE IS GENERATED\n*/\n' > $$@
	printf ':- module('%s',[]).\n%% Dummy, not really a foreign resource\nforeign_resource('%s', []).\n' '$(1)' '$(1)' >> $$@
$(eval $(call resname_makefile_TEMPLATE,$(1),$(SHSFX)))
endef # spti_makefile_TEMPLATE

$(foreach F,$(patsubst %.po,%,$(FOREIGN_MODULES)),$(eval $(call resname_makefile_TEMPLATE,$(F),$(FLI_SHSFX))))
$(foreach F,$(SPTI_MODULES),$(eval $(call spti_makefile_TEMPLATE,$(F))))

$(foreach F,$(DUMMY_RESOURCE_NAMES),$(eval $(call resname_makefile_TEMPLATE,$(F),$(FLI_SHSFX))))


ifneq ($(JASPER_MODULE),)

# ------------------------------------------------------------------------------
# Build JNI header files
# ------------------------------------------------------------------------------

#[PD] 3.9 added Jasper (*** FIX? Since no native methods are left in SPTerm,
#                       SPPredicate, SPQuery, SPException and SPCanonicalAtom
#                       (they have been moved to SICStus), should we remove all
#                       but Jasper and SICStus?)
JNI_CLASSES= Jasper SICStus SPTerm SPPredicate SPQuery SPException SPCanonicalAtom
JNI_HDR= $(patsubst %,jasper/se_sics_jasper_%.h,$(JNI_CLASSES))

.PHONY: jni_headers
jni_headers: $(JNI_HDR)

# [PM] 3.10.2 ../ is ugly
$(JNI_HDR): jasper/se_sics_jasper_%.h: $(SRCDIR_PREFIX)../se/sics/jasper/%.class
	(cd jasper ; $(JAVAH) -verbose -classpath $(SRCDIR_PREFIX)../.. -jni se.sics.jasper.$* )
	touch $@

# this is cheating, it is really compiling spnative.c that needs JNI_HDR 
jasper.po: $(JNI_HDR)

# ------------------------------------------------------------------------------
# Build examples in library('jasper/examples')
# ------------------------------------------------------------------------------

JASPER_XMPLDIR=$(SRCDIR_PREFIX)jasper/examples

#[PD] 3.8.7 no foreign resources in example
#JASPER_EXAMPLES=$(patsubst %, $(JASPER_XMPLDIR)/%, simple.$(SHSFX) Queens.class Simple.class simple.po jqueens.po)

JASPER_EXAMPLE_FILES = Simple.class simple.po

ifeq ($(ENABLE_CLPFD),yes)	# [PM] 3.10.1 conditionalize on clpfd
JASPER_EXAMPLE_FILES += Queens.class jqueens.po
endif				# ENABLE_CLPFD

JASPER_EXAMPLES := $(addprefix $(JASPER_XMPLDIR)/, $(JASPER_EXAMPLE_FILES))

.PHONY: jasper_examples
jasper_examples: $(JASPER_EXAMPLES)

# [PM] 3.10.2 ensure errors aborts compilation
$(JASPER_XMPLDIR)/simple.po $(JASPER_XMPLDIR)/jqueens.po: %.po: %.pl $(SRCDIR_PREFIX)../$(PCACHE)
	echo "$(BARF_ON_ERROR) ensure_loaded('$<'),save_files('$<','$@'),halt." | $(JASPER_ENV) $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

# [PM] 4.3.3 Adding toplevel directory to classpath is problematic if
# there are junk Java files there (e.g. on a development
# machine). Instead rely on jasper.jar being built.
#
# # We don't use jasper.jar here in case it hasn't been built. ([PM] 3.10.2 FIXME: it should have been built!)
# JASPER_XMPL_CLASSPATH='$(SRCDIR)$(PATHSEP)..$(PATHSEP)$(call NATIVEPATH, $(JASPER_XMPLDIR))'
#
# [PM] 4.3.3 (this way we do not get confused if there are random .java files in the toplevel directory (at ./../).
JASPER_XMPL_CLASSPATH='$(call NATIVEPATH, $(CURDIR)/../se/sics/jasper/jasper.jar)$(PATHSEP)$(call NATIVEPATH, $(JASPER_XMPLDIR))'

$(JASPER_XMPLDIR)/Queens.class $(JASPER_XMPLDIR)/Simple.class: %.class: %.java
	$(JAVAC) $(JAVACFLAGS) $(JAVACDBGFLAGS) -verbose  $(JAVACLINTFLAGS) -deprecation -classpath $(JASPER_XMPL_CLASSPATH) $^

else				# !JASPER_MODULE
.PHONY: jasper_examples jni_headers
jasper_examples jni_headers:
	@echo "Jasper is not supported on this platform. Target \"$@\" will not be built."
endif				# !JASPER_MODULE

# [PM] 3.10.2 PrologBeans examples

# [PM] 3.11.2 Assume ENABLE_JAVA=yes implies PROLOGBEANS_MODULE non-empty
ifeq ($(ENABLE_JAVA),yes)

# [PM] 3.10.2 ../ is ugly
PROLOGBEANS_JAR := $(SRCDIR_PREFIX)../se/sics/prologbeans/prologbeans.jar

PROLOGBEANS_EXAMPLES_DIR := $(SRCDIR_PREFIX)prologbeans/examples

# [PM] 3.10.2 there is not much point in compiling the example prolog files.
PROLOGBEANS_EXAMPLE_FILES := $(filter-out %.po, \
	pbtest/PBTest.class pbtest/pbtest.po \
        evaluate/EvaluateGUI.class evaluate/evaluate.po \
	sessionsum/sessionsum.po )

PROLOGBEANS_EXAMPLES := $(addprefix $(PROLOGBEANS_EXAMPLES_DIR)/, $(PROLOGBEANS_EXAMPLE_FILES))


PROLOGBEANS_EXAMPLE_CLASSPATH := '$(call NATIVEPATH, $(PROLOGBEANS_JAR))$(PATHSEP)$(call NATIVEPATH, $(PROLOGBEANS_EXAMPLES_DIR))'

$(filter %.po, $(PROLOGBEANS_EXAMPLES)): %.po: %.pl $(SRCDIR_PREFIX)../$(PCACHE)
	echo "$(BARF_ON_ERROR) ensure_loaded('$<'),save_files('$<','$@'),halt." | $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

$(filter %.class, $(PROLOGBEANS_EXAMPLES)): %.class: %.java
	$(JAVAC) $(JAVACFLAGS) $(JAVACDBGFLAGS) -verbose $(JAVACLINTFLAGS) -deprecation -classpath $(PROLOGBEANS_EXAMPLE_CLASSPATH) $^


.PHONY: prologbeans_examples
prologbeans_examples: $(PROLOGBEANS_EXAMPLES)

else				# !ENABLE_JAVA
.PHONY: prologbeans_examples
prologbeans_examples:
	@echo "PrologBeans is not supported on this platform. Target \"$@\" will not be built."
endif				# !ENABLE_JAVA


# ------------------------------------------------------------------------------
# Support for require/1
# ------------------------------------------------------------------------------

$(LIBRARY_OUTPUT_PREFIX)INDEX.pl: $(LIBRARY_OUTPUT_PREFIX)mkindex.po $(filter-out $(UNINDEXED_MODULES:.po=.pl),$(BUILT_MODULES:.po=.pl))
	@echo
	@echo "Building $@..."
	@echo =========================================
	@echo 
	echo '$$<==$<, $$(@D)=$(@D)'
	echo "make_index('$(@D)'),halt." | $(SICSTUS_FOR_LIBRARY) $(SICSTUS_FOR_LIBRARY_FLAGS) -l $< $(SICSTUS_MKLIBS_FLAGS)
	$(VERIFY_TARGET_CREATED)
	$(TOUCH_SP_TARGET)

#
#
#

# [PM] 4.0 order-only prerequisite telling to make SU_messages.po before everything else
$(filter-out SU_messages.po, $(ALL_MODULES)) : | SU_messages.po

# .po files are not really portable, so make sure that they are
# rebuilt when changing platform.
$(ALL_MODULES): $(SRCDIR_PREFIX)../$(PCACHE)


# [PM]
.PHONY: other
other: $(OTHER)

.PHONY: test_modules
# [PM] 4.1.3 bitrot. Not used.
test_modules:
	set -e; for module in $(BUILT_MODULES:.po=); do \
	  echo ;\
	  echo "*****************************************" ;\
	  echo "*** Attemting to load module $$module ***" ;\
	  echo "*****************************************" ;\
	  echo "use_module(library('$$module')),halt." | $(SICSTUS) -f ;\
	done

# ------------------------------------------------------------------------------

SPTI_LIBS = $(patsubst %, $(LIBRARY_OUTPUT_PREFIX)$(PLATFORM)/%.$(SHSFX), $(SPTI_MODULES))

# ------------------------------------------------------------------------------
# Main rule. Build invididual modules by:
# % make all BUILT_MODULES='tcltk.po system.po'".
# ------------------------------------------------------------------------------

.PHONY: all
all: INDEX.pl $(BUILT_MODULES) other $(PLATFORM)/.spider_data devsys_info $(SPTI_LIBS) $(DUMMY_RESOURCES)

# ------------------------------------------------------------------------------
# Directory creation
# (we want these late to allow things above to add directories)
# ------------------------------------------------------------------------------

# [PM] 4.2 make 3.82 seems to be picky with (i.e. require) / at end of directory name.
#          make 3.81 seems to treat either the same (and therefore
#          complain about duplicates targets) so we remove the non-/
#          terminated variant.
$(sort $(addsuffix /,$(GENERATED_INCLUDES_DIRS)) $(addsuffix /,$(LIBRARY_OUTPUT_PREFIX)$(PLATFORM)) $(addsuffix /,$(BUILT_FOREIGN_PLATFORM_DIRS))):
	$(MKDIR) $@


# ------------------------------------------------------------------------------
# Installation
# ------------------------------------------------------------------------------

INSTALLFILES = Makefile README mklibs.pl

ifeq ($(WIN32),no)
# [PM] 3.9b5 license.pl is only used on non-Windows.
INSTALLFILES += license.pl
endif				# !WIN32

# [PM] 4.0 *~ is (emacs) backup file, #*# is (emacs) autosave file, .#* is CVS "previous version"-file
# Had trouble quoting # so it would not be treated as a comment in either make or shell. Therefore use define instead of ordinary SKIPTEST = ...
# "$${file##*/}" is a (faster?) way of doing $$(basename "$$file")
define SKIPTEST
case "$${file##*/}" in (*~|\#*\#|.\#*) echo "skipping $$file"; continue;; esac;
endef
# [PM] 4.0 This was used before but did the wrong thing for non-basename $file
# SKIPTEST = if expr "$$file" : '.*~.*' > /dev/null || expr "$$file" : '\.#.*' > /dev/null; then echo "Skipping $$file..." ; continue; fi;

# [PM] 4.0 the original install target does not install everything it should so we might as well trust the new copy-everything approach
INSTALL_WITH_TARCOPY=yes

ifeq ($(WIN32),yes)
# [PM] 4.0 copying with tar instead of (INSTALL) is a gazillion times faster, at least on cygwin
INSTALL_WITH_TARCOPY=yes
endif

# xref doc/Makefile keep in sync
ifeq ($(INSTALL_WITH_TARCOPY), yes)

#          We would use pax -r -w but there is no pax for cygwin

# We copy everything not explicitly removed. We may need to do some manual deletes or add a --exclude-from=FILE 
# This is fast but also easier to maintain. The original install target turned out to be quit badly out of date.

TARCOPY_EXCLUDES = --exclude CVS      # CVS meta info directory
TARCOPY_EXCLUDES += --exclude "\#*\#" # emacs autosave file
TARCOPY_EXCLUDES += --exclude='.\#*'  # CVS previous version
TARCOPY_EXCLUDES += --exclude '*~'    # (emacs) backup files
TARCOPY_EXCLUDES += --exclude '*_glue.*' # splfr generated glue
TARCOPY_EXCLUDES += --exclude '*.stackdump' # cygwin core dumps
# not on Win32: TARCOPY_EXCLUDES += --exclude 'core*' # core dumps

TARCOPY_EXCLUDES += --exclude '*_d.$(OBJEXT)' # dynamic foreign resource objects
TARCOPY_EXCLUDES += --exclude '*_s.$(OBJEXT)' # static foreign resource objects

TARCOPY_EXCLUDES += --exclude license.pl

TARCOPY_EXCLUDES += --exclude approved.txt --exclude disapproved.txt

TARCOPY_EXCLUDES += --exclude 'bdb/iqsoft' # internal documentation etc
# TARCOPY_EXCLUDES += --exclude 'Makefile_*' # generated makefiles

# [PM] 4.3.2 The C# files are back again
# TARCOPY_EXCLUDES += --exclude '*/prologbeans.NET/*.cs' # The C# code is not longer used

TARCOPY_EXCLUDES += --exclude='chr/examples*'

# not (currently) distributed libraries
# TARCOPY_EXCLUDES += --exclude jasper


.PHONY: install
install:
	@echo "Creating directory $(SP_LIBDIR)/library..."
	$(MKDIR) "$(SP_LIBDIR)/library"
	$(INSTALL_DATA) $(SRCDIR_PREFIX)../Common $(SP_LIBDIR)
	$(GNUTAR) --create --mode=a+rwX $(TARCOPY_EXCLUDES) . | $(GNUTAR) --file=- --extract -C "$(SP_LIBDIR)/library" --verbose
else

$(error [PM] 4.0 The original install target has not managed to keep itself updated.)

endif				# !INSTALL_WITH_TARCOPY

# ------------------------------------------------------------------------------
# Extra Dependencies
# ------------------------------------------------------------------------------
$(oprofile_objs_d) : oprofile_common.c
$(spti_oprofile_objs_d) : oprofile_common.c

$(perf_objs_d) : perf/perf_common.c
$(spti_perf_objs_d) : perf/perf_common.c



# ------------------------------------------------------------------------------
# Administration
# ------------------------------------------------------------------------------
.PHONY: clean distclean
# [PM] WinCE FIXME: check if the *.o and *.obj should be here, was not in 3.11.1
clean distclean:
	$(RM) -rf $(patsubst %,'%',$(wildcard $(PLATFORM) */$(PLATFORM) *.pdb *.lib *.exp *.mapfile *.o *.obj *.sbr)) $(wildcard $(GENERATED_FILES)) $(wildcard *.plist) $(RMDUMMY)
	$(RM) -f $(patsubst %,'%',$(wildcard $(SOFTLINKS))) $(RMDUMMY)
	$(MAKE) -C chr $@
	$(MAKE) -C jasper/examples $@
	(cd jasper; $(RM) -f se_sics_jasper*.h $(RMDUMMY))
	$(RM) -f $(ALL_MODULES) INDEX.pl index.pl devsys_info so_locations $(RMDUMMY)
	$(RM) -f $(patsubst %,'%',$(wildcard *_glue.h *_glue_*.c *_glue*.c)) $(RMDUMMY)
	$(RM) -f $(patsubst %,'%', $(wildcard prologbeans/examples/*/*.class)) $(RMDUMMY)
	$(MAKE) -C prologbeans.NET $@

.PHONY: maintainerclean mostlyclean
maintainerclean mostlyclean: distclean
	$(MAKE) -C chr $@
	$(RM) -f $(FR_MAKEFILES) $(RMDUMMY)
