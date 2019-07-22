#!/bin/sh

# --------------------------
# Single word lexicon
# --------------------------

if [ $# = 0 ]; then
    echo "usage: $0 <four-digit-year>"
    exit 0
fi

# SKR_SRC_HOME=$HOME/specialist/SKR/src
# TOP=/net/nls2/export/home/divita/nls/specialist/lexicon1999
# TOP=/nfsvol/nls/specialist/lexicon1999_Current
# TOP=$SKR_SRC_HOME/lexicon
# export TOP

# path of nlp java classes
# CURRENT=/net/lexlx2/media/usbdisk/testbed/v2.4.B/nls/nlp
# CURRENT=/net/indlx1/export/home/wrogers/Projects/mmtxdev/rel-2-4-C-git/nls/nlp
# CURRENT=/nfsvol/nlsaux15/mmtxdev/rel-2-4-C-git/nls/nlp
# NLS=/rhome/wjrogers/Projects/nls_projects/nls
# NLP_CLASSES=$NLS/nlp/lib/nlpProject.jar
# SWL_CLASSES=$NLS/singleWordLexicon/lib/singleWordLexicon.jar

# Berkeley DB library path for java
# DB_TOP=/nfsvol/nls/tools/berkeley_db/db_3
# DB_TOP=/nfsvol/nls/tools/berkeley_db/Linux-i686/db-4.1.25
# DB_TOP=/usr/local/BerkeleyDB.4.5-m32
# DB_TOP=/usr/local/BerkeleyDB.4.5
# should be: DB_TOP=$BERKELEY
# LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$DB_TOP/lib
# DB_CLASSES=$DB_TOP/lib/db.jar
# export LD_LIBRARY_PATH

# DATABASE_HOME=${LEXICON}/data
# export DATABASE_HOME

# DEFAULT_LEXICON_FILE=$DATABASE_HOME/singleWordLexiconStatic${LEXICON_VERSION}
# export DEFAULT_LEXICON_FILE
# DEFAULT_LEXICON_INDEX_FILE=${DEFAULT_LEXICON_FILE}Ind
# export DEFAULT_LEXICON_INDEX_FILE

# LCAT_HELP_FILE=${LEXICON}/lcat/lcat.help
# export LCAT_HELP_FILE

# ---------------------------
# index the files 
# ---------------------------

LEXICON_VERSION="$1"
LEXICON_DIR=..

# LM_TRANSLATED_RULES_FILE=${LEXICON_DIR}/morph/lm_translated_rules
# export LM_TRANSLATED_RULES_FILE

# IM_TRANSLATED_RULES_FILE=${LEXICON_DIR}/morph/im_translated_rules
# export IM_TRANSLATED_RULES_FILE

# DM_TRANSLATED_RULES_FILE=${LEXICON_DIR}/morph/dm_translated_rules
# export DM_TRANSLATED_RULES_FILE


 echo ../linsert/linsert -v ${LEXICON_VERSION}              \
   -l ../data/singleWordLexiconStatic${LEXICON_VERSION}     \
   -i ../data/singleWordLexiconStatic${LEXICON_VERSION}Ind  \
   -r ../data/LEXICON${LEXICON_VERSION}

time ../linsert/linsert -v ${LEXICON_VERSION}              \
  -l ../data/singleWordLexiconStatic${LEXICON_VERSION}     \
  -i ../data/singleWordLexiconStatic${LEXICON_VERSION}Ind  \
  -r ../data/LEXICON${LEXICON_VERSION} > ./buildLexicon${LEXICON_VERSION}.msg 2>&1

echo "done" >> ./buildLexicon${LEXICON_VERSION}.msg

file ../data/singleWordLexiconStatic${LEXICON_VERSION}IndByInfl.dbx
file ../data/singleWordLexiconStatic${LEXICON_VERSION}IndByEui.dbx

wc ./buildLexicon${LEXICON_VERSION}.msg

grep Cannot ./buildLexicon${LEXICON_VERSION}.msg && echo "Error building lexicon files, check buildLexicon${LEXICON_VERSION}.msg"

# ---------------------------
# read in the indexed files, 
# find out the orphan terms
# and tag them
# ---------------------------

# echo "About to alter the infl Table and create a new infl table"
# echo java -mx900m -ms300m -cp "$DB_CLASSES:$NLP_CLASSES:$SWL_CLASSES:$TOP/classes:$TOP/config:" gov.nih.nlm.nls.singleWordLexicon.SingleWordLexicon $*
# java -mx900m -ms300m -cp "$DB_CLASSES:$NLP_CLASSES:$SWL_CLASSES:$TOP/classes:$TOP/config:" gov.nih.nlm.nls.singleWordLexicon.SingleWordLexicon $* >orphanWords 2>./inflMsgs.txt 
# echo "done" >>./inflMsgs.txt


# echo "About to alter the eui Table and create a new eui table"
# echo java -mx900m -ms300m -cp "$DB_CLASSES:$NLP_CLASSES:$SWL_CLASSES:$TOP/classes:$TOP/config:" gov.nih.nlm.nls.singleWordLexicon.SingleWordLexicon --euiTable $*
# java -mx900m -ms300m -cp "$DB_CLASSES:$NLP_CLASSES:$SWL_CLASSES:$TOP/classes:$TOP/config:" gov.nih.nlm.nls.singleWordLexicon.SingleWordLexicon --euiTable $* >orphanWords2 2>./euiMsgs.txt 
# echo "done" >>./euiMsgs.txt 


# ----------------------------------------------
# The programs require that the endings of the data files
# end with IndByEui.dbx and IndByInfl.dbx.
#
# Rename the files to make them so:
# ----------------------------------------------

echo Renaming files
 
mv ../data/singleWordLexiconStatic${LEXICON_VERSION}              \
   ../data/lexiconStatic${LEXICON_VERSION}

mv ../data/singleWordLexiconStatic${LEXICON_VERSION}IndByEui.dbx  \
   ../data/lexiconStatic${LEXICON_VERSION}IndByEui.dbx

mv ../data/singleWordLexiconStatic${LEXICON_VERSION}IndByInfl.dbx \
   ../data/lexiconStatic${LEXICON_VERSION}IndByInfl.dbx
