
#############################################################################
#
#                          PUBLIC DOMAIN NOTICE                         
#         Lister Hill National Center for Biomedical Communications
#                      National Library of Medicine
#                      National Institues of Health
#           United States Department of Health and Human Services
#                                                                         
#  This software is a United States Government Work under the terms of the
#  United States Copyright Act. It was written as part of the authors'
#  official duties as United States Government employees and contractors
#  and thus cannot be copyrighted. This software is freely available
#  to the public for use. The National Library of Medicine and the
#  United States Government have not placed any restriction on its
#  use or reproduction.
#                                                                        
#  Although all reasonable efforts have been taken to ensure the accuracy 
#  and reliability of the software and data, the National Library of Medicine
#  and the United States Government do not and cannot warrant the performance
#  or results that may be obtained by using this software or data.
#  The National Library of Medicine and the U.S. Government disclaim all
#  warranties, expressed or implied, including warranties of performance,
#  merchantability or fitness for any particular purpose.
#                                                                         
#  For full details, please see the MetaMap Terms & Conditions, available at
#  http://metamap.nlm.nih.gov/MMTnCs.shtml.
#
############################################################################

# WSD MBL classifier makefile, currently supports Solaris and Linux
#
HOST = $(shell hostname)

ifeq '$(HOST)' "nls10"
  TIMBL=/home/wrogers/pub/machinelearning/mbl/Timbl5
  TIMBLLIB=/net/indfiler/vol/vol0/nls10/wsd/Disambiguator/WSD_Server/lib
else
  ifeq '$(HOST)' "cgsblx2.nlm.nih.gov"
    TIMBL=/usr/local/pub/src/machinelearning/mbl/Timbl5
    TIMBLLIB=/home/wrogers/devel/exper/wsd/Disambiguator/WSD_Server/lib
  endif
endif

O_SYSTEM = $(shell uname)

ifeq '$(O_SYSTEM)' "SunOS"
  OSTYPE = solaris
  LIBS = -R$(TIMBLLIB) -L$(TIMBLLIB) -lTimbl
else
  ifeq '$(O_SYSTEM)' "Linux"
	OSTYPE = linux
	LIBS = -W,-R$(TIMBLLIB) -L$(TIMBLLIB) -lTimbl
  endif
endif

export OSTYPE
export O_SYSTEM

# CFLAGS=-g -D_REENTRANT -D_GNU_SOURCE
INCLUDES=-I$(TIMBL) -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/$(OSTYPE)
CXXFLAGS=$(INCLUDES) -g -Wall
LDFLAGS=
LEX=flex

WSDMBLOBJS=WsdMbl.o Tokenize.o Scanner.o Token.o ListUtils.o

all : libWsdMbl.so

libWsdMbl.so: $(WSDMBLOBJS) 
	$(CXX) -shared -o $@ $(WSDMBLOBJS) $(LIBS)

WsdMbl.o : WsdMbl.cc
Tokenize.o : Tokenize.cc
Token.o : Token.cc Token.h
Scanner.o : Scanner.cc Scanner.h Token.h
ListUtils.o : ListUtils.cc ListUtils.h

install : libWsdMbl.so
	cp libWsdMbl.so ../lib

clean :
	rm -f *.o *.so 
