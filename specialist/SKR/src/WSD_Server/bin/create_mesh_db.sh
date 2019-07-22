
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

#!/bin/sh

UMLSKS_HOST=umlsks
UMLS_RELEASE=2002AD
LIB=$WSD/WSD_Server/lib
JAR_FILE=$LIB/wsd.jar:$LIB/kss-api.jar:$LIB/thirdparty.jar:$LIB/db.jar

java -classpath $JAR_FILE wsd.util.MeSHFrequencyCalculator $UMLSKS_HOST $UMLS_RELEASE $1 $1.txt 
#cat $1.txt | cut -d'|' -f2,3 | sed 's/\\/\\\\/g' | db_load -c duplicates=0 -T -t btree $1.db

