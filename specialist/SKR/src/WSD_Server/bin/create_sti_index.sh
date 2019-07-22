
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
#
# Build index for Semantic Type Indexing Method (wsd.methods.SemTypeIndexingMethod)
# example usage:
#    % bin/create_sti_index.sh index wsdvsc01-12-dc.txt wstv-dc
#

if [ $# = 0 ]; then
    echo "usage: $0 [index <sourcefile> <indexdir>] [get <indexdir> <term>]"
    exit 0
fi 

LIB=$WSD/WSD_Server/lib
JAR_FILE=$LIB/wsd.jar

$JAVA_HOME/bin/java -mx2g -classpath $JAR_FILE wsd.util.IntArrayIndex $*

exit 1
