
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

# --------------------------
# Single word lexicon
# --------------------------

if [ $# = 0 ]; then
    echo "usage: $0 <four-digit-year>"
    exit 0
fi

#TOP=/net/nls2/export/home/divita/nls/specialist/lexicon1999
#TOP=/nfsvol/nls/specialist/lexicon1999_Current
TOP=$SKR_SRC_HOME/lexicon

# path of nlp java classes
# CURRENT=/net/lexlx2/media/usbdisk/testbed/v2.4.B/nls/nlp
#CURRENT=/net/indlx1/export/home/wrogers/Projects/mmtxdev/rel-2-4-C-git/nls/nlp
CURRENT=/nfsvol/nlsaux15/mmtxdev/nls_projects/nls/nlp

# Berkeley DB library path for java
#DB_TOP=/nfsvol/nls/tools/berkeley_db/db_3
DB_TOP=/nfsvol/nls/tools/berkeley_db/Linux-i686/db-4.1.25
# should be: DB_TOP=$BERKELEY
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$DB_TOP/lib
DB_CLASSES=$DB_TOP/classes
export LD_LIBRARY_PATH

# ---------------------------
# index the files 
# ---------------------------
LEXICON_VERSION="$1"
DEFAULT_LEXICON_FILE=$DATABASE_HOME/lexiconStatic${LEXICON_VERSION}
export DEFAULT_LEXICON_FILE
DEFAULT_LEXICON_INDEX_FILE=${DEFAULT_LEXICON_FILE}Ind
export DEFAULT_LEXICON_INDEX_FILE

../linsert/linsert -v ${LEXICON_VERSION} -l ../data/lexicon${LEXICON_VERSION} -i ../data/singleWordLexiconStatic${LEXICON_VERSION}Ind -r ../data/LEXICON${LEXICON_VERSION} >./buildLexicon.msg 2>&1

echo "done" >>./buildLexicon${LEXICON_VERSION}.msg
# ---------------------------
# read in the indexed files, 
# find out the orphan terms
# and tag them
# ---------------------------

echo "About to alter the infl Table and create a new infl table"
java -mx900m -ms300m -cp "$DB_CLASSES:$CURRENT/lib/nlpProject.jar:$TOP/classes:$TOP/config:" gov.nih.nlm.nls.singleWordLexicon.SingleWordLexicon $* >orphanWords 2>./inflMsgs.txt 
echo "done" >>./inflMsgs.txt

echo "About to alter the eui Table and create a new eui table"
java -mx900m -ms300m -cp "$DB_CLASSES:$CURRENT/lib/nlpProject.jar:$TOP/classes:$TOP/config:" gov.nih.nlm.nls.singleWordLexicon.SingleWordLexicon --euiTable $* >orphanWords2 2>./euiMsgs.txt 
echo "done" >>./euiMsgs.txt 

# ----------------------------------------------
# The programs require that the endings of the data files
# end with IndByEui.dbx and IndByInfl.dbx.
#
# Rename the files to make them so:
# ----------------------------------------------
# mv ../data/singleWordLexiconStatic${LEXICON_VERSION} ../data/lexiconStatic${LEXICON_VERSION}
mv ../data/singleWordLexiconStatic${LEXICON_VERSION}IndByEui.dbx ../data/lexiconStatic${LEXICON_VERSION}IndByEui.dbx
mv ../data/singleWordLexiconStatic${LEXICON_VERSION}IndByInfl.dbx ../data/lexiconStatic${LEXICON_VERSION}IndByInfl.dbx
