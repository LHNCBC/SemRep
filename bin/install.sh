#! /bin/sh
# install.sh - Public SemRep Install Program
#
# public_semrep/bin/install.sh
# adapted from the MetaMap Install Program
#
# This script reads environment variables BASEDIR and JAVA_HOME from
# parent process.
# 
if [ -x ./bin/date ]; then
  echo "$0 started `./bin/date`"  > ./install.log
else
  echo "$0 started `date`"  > ./install.log
fi
if [ -x ./bin/uname ]; then
  OS=`./bin/uname`
else
  OS=`uname`
fi

if [ $OS = "Linux" ]; then
# GNU libc version test
  REQ_MAJOR_VERSION=2
  REQ_MINOR_VERSION=5
  echo "glib minimun required version: $REQ_MAJOR_VERSION.$REQ_MINOR_VERSION"   > ./install.log
  MAJOR_VERSION=`ldd --version | grep libc | cut -d' ' -f4 | cut -d'.' -f1`
  MINOR_VERSION=`ldd --version | grep libc | cut -d'.' -f2`
  echo "glibc major version: $MAJOR_VERSION" > ./install.log
  echo "glibc minor version: $MINOR_VERSION" > ./install.log
  expr "$MAJOR_VERSION" \>= "$REQ_MAJOR_VERSION" > /dev/null
  if [ $? -eq 1 ]; then
    echo "!!"
    echo "!! Warning: GNU libc version must be $REQ_MAJOR_VERSION.$REQ_MINOR_VERSION or greater."
    echo "!! The current GNU libc version is $MAJOR_VERSION.$MINOR_VERSION."
    echo "!!"
  else
    expr "$MINOR_VERSION" \>= "$REQ_MINOR_VERSION" > /dev/null
    if [ $? -eq 1 ]; then
      echo "!!"
      echo "!! Warning: GNU libc version must be $REQ_MAJOR_VERSION.$REQ_MINOR_VERSION or greater."
      echo "!! The current GNU libc version is $MAJOR_VERSION.$MINOR_VERSION."
      echo "!!"
      echo "!! IMPORTANT NOTE: this script can be incorrect in determining the GLIBC version."
      echo "!! It is suggested that the user verify that they are using a version of"
      echo "!! of GLIBC greater than or equal to ${REQ_MAJOR_VERSION}.${REQ_MINOR_VERSION}"
    fi
  fi
else
  if [ $OS = "MINGW32_NT-5.1" ]; then
    PATH=$PATH:./bin
  fi
fi

# load installrc if present.
if [ -r .installrc ]; then
    source .installrc
fi
# if [ "$BASEDIR" = "" ]; then
#  BASEDIR=$PWD
#fi
echo "Enter basedir of SemRep installation [$PWD] " 
read BASEDIR
if [ "$BASEDIR" = "" ]; then
  BASEDIR=$PWD
fi
BINDIR=$BASEDIR/bin

# assumes that SemRep and MetaMap directories have the same parent
PARENT_DIR=`dirname $BASEDIR`
MM_DEFDIR=$PARENT_DIR/public_mm

echo "Enter basedir of MetaMap installation [$MM_DEFDIR] "
read MM_BASEDIR
if [ "$MM_BASEDIR" = "" ]; then
  MM_BASEDIR=$MM_DEFDIR
fi

echo "Basedir is set to $BASEDIR."
echo "Basedir is set to $BASEDIR." >> ./install.log
echo "MetaMap basedir is set to $MM_BASEDIR."
echo "MetaMap basedir is set to $MM_BASEDIR." >> ./install.log
echo ""

echo "Where does your distribution of Sun\'s JRE reside?"
if [ "$JAVA_HOME" = "" ]; then
  javaprog=`which java`
  if [ $? -eq 0 ]; then
    javabindir=`dirname $javaprog`
    RC_JAVA_HOME=`dirname $javabindir`
  else
    RC_JAVA_HOME=""
  fi
else
  RC_JAVA_HOME=$JAVA_HOME
fi
echo  "Enter home path of JRE (JDK) [$RC_JAVA_HOME]: " 
read JAVA_HOME
if [ "$JAVA_HOME" = "" ]; then
   JAVA_HOME=$RC_JAVA_HOME
fi     

echo Using $JAVA_HOME for JAVA_HOME.
echo Using $JAVA_HOME for JAVA_HOME. >> ./install.log
echo ""

echo  "Delete UMLS Metathesaurus and SPECIALIST Lexicon data files after installation (recommended)? [y/n]:" 
read DELETE_DATA
if [ "$DELETE_DATA" = "y" ]; then
    echo "... will delete the data files after installation." 
    echo "... will delete the data files after installation." >> ./install.log
else 
    echo "... will keep the data files after installation. DATA/DB and DATA/LEXICON directories may be deleted later after successful installation." 
    echo "... will keep the data files after installation. DATA/DB and DATA/LEXICON directories may be deleted later after successful installation." >> ./install.log
fi     

echo Using $JAVA_HOME for JAVA_HOME.
echo Using $JAVA_HOME for JAVA_HOME. >> ./install.log
echo ""

AVAILABLE_DATA="2006 2018"

if [ -d $BASEDIR/DATA/LEXICON ] 
then
    for LEX in $AVAILABLE_DATA 
    do
        echo "Copying $LEX lexicon data files..."
        echo "Copying $LEX lexicon data files..." >> ./install.log
        cp $BASEDIR/DATA/LEXICON/*${LEX}* $MM_BASEDIR/lexicon/data >> ./install.log 2>&1
    done
#    rm -rf $BASEDIR/DATA/LEXICON
else
    echo "Lexicon data directory $BASEDIR/DATA/LEXICON cannot be found."
    echo "Lexicon data directory $BASEDIR/DATA/LEXICON cannot be found." >> ./install.log 2>&1
fi

if [ -d $BASEDIR/DATA/DB ]
then
    for DB in $AVAILABLE_DATA
    do
	echo "Copying $DB MetaMap DB data files..."
	echo "Copying $DB MetaMap DB data files..." >> ./install.log
	cp -r $BASEDIR/DATA/DB/*${DB}* $MM_BASEDIR/DB/ >> ./install.log 2>&1
    done
#    rm -rf $BASEDIR/DATA/DB
else 
    echo "MetaMap DB directory $BASEDIR/DATA/DB cannot be found."
    echo "MetaMap DB directory $BASEDIR/DATA/DB cannot be found." >> ./install.log 2>&1
fi

echo "Setting up bin directory scripts:"
echo "Setting up bin directory scripts:" >> ./install.log
# Setup all scripts generated from templates
for binscriptbase in $BASEDIR/bin/*.in
do
  binscript=`basename $binscriptbase .in`
  binscripttmp=${binscript}.tmp
  binscripttmp1=${binscript}.tmp1
  sed -e "s:@@basedir@@:$BASEDIR:g" $binscriptbase > $BINDIR/$binscripttmp
  sed -e "s:@@mm_basedir@@:$MM_BASEDIR:g" $BINDIR/$binscripttmp > $BINDIR/$binscripttmp1
  sed -e "s:@@java_home@@:$JAVA_HOME:g" $BINDIR/$binscripttmp1 > $BINDIR/$binscript
  chmod +x $BINDIR/$binscript
  if [ -x $BINDIR/$binscript ]; then
    rm $BINDIR/$binscripttmp
    rm $BINDIR/$binscripttmp1
    echo $BINDIR/$binscript generated.
    echo $BINDIR/$binscript generated. >> ./install.log
  fi
done

echo "Setting up test suite:"
echo "Setting up test suite:" >> ./install.log 2>&1

# Setup test script (runTest_<version>.sh runTest_<version>.sh)
for rtscripttmp in $BASEDIR/TestSuite/runTest_*.in
do
  rtscript=`basename $rtscripttmp .in`
  sed -e "s:@@basedir@@:$BASEDIR:g" $rtscripttmp > $BASEDIR/TestSuite/$rtscript
  chmod +x $BASEDIR/TestSuite/$rtscript
  if [ -x $BASEDIR/TestSuite/$rtscript ]; then
    echo $BASEDIR/TestSuite/$rtscript generated.
    echo $BASEDIR/TestSuite/$rtscript generated. >> ./install.log
  fi
done

# check for presence of lexicon, MetaMap databases, tagger, WSD, abgene, entrezgene, transitive closure 
echo "Checking for required datafiles"
echo "Checking for required datafiles" >> ./install.log
MISSINGFILES=0

echo "Checking for required MetaMap datafiles"
echo "Checking for required MetaMap datafiles" >> ./install.log

# Lexicon files
FACTS=`find $MM_BASEDIR/lexicon/morph -name \*facts -print | wc -l`
if [ $FACTS == 0 ]; then
  echo "Warning: lexicon facts files are missing, cannot ensure correct operation of SemRep without them!"
  echo "Warning: lexicon facts files are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
  echo ""
  MISSINGFILES=`expr $MISSINGFILES + 1`
else 
  find $MM_BASEDIR/lexicon/morph -name \*facts -print >> ./install.log 2>&1 
fi

RULES=`find $MM_BASEDIR/lexicon/morph -name \*rules -print | wc -l`
if [ $RULES == 0 ]; then
  echo "Warning: lexicon rules files are missing, cannot ensure correct operation of SemRep without them!"
  echo "Warning: lexicon rules files are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
  echo ""
  MISSINGFILES=`expr $MISSINGFILES + 1`
else 
  find $MM_BASEDIR/lexicon/morph -name \*rules -print >> ./install.log 2>&1
fi

# for LEX in $AVAILABLE_DATA
# do
    LEXICONSTATIC=`find $MM_BASEDIR/lexicon/data -name lexiconStatic2006\* -print | wc -l`
    if [ $LEXICONSTATIC == 0 ]; then
	echo "Warning: lexicon lookup files (lexiconStatic2006*) are missing, cannot ensure correct operation of SemRep without them!"
	echo "Warning: lexicon lookup files (lexiconStatic2006*) are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
	echo ""
	MISSINGFILES=`expr $MISSINGFILES + 1`
    else
	find $MM_BASEDIR/lexicon/data -name lexiconStatic2006\* -print >> ./install.log 2>&1
    fi
# done


# for LEX in $AVAILABLE_DATA
# do
    INDBY=`find $MM_BASEDIR/lexicon/data -name lexiconStatic2006\*IndBy\*.dbx -print | wc -l`
    if [ $INDBY == 0 ]; then
	echo "Warning: lexicon index files (lexiconStatic2006*IndBy*) are missing, cannot ensure correct operation of SemRep without them!"
	echo "Warning: lexicon index files (lexiconStatic2006*IndBy*) are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
	echo ""
	MISSINGFILES=`expr $MISSINGFILES + 1`
    else
	find $MM_BASEDIR/lexicon/data -name lexiconStatic2006\*IndBy\*.dbx -print >> ./install.log 2>&1
	if [ $OS = "MINGW32_NT-5.1" ]; then
	    echo "Trying to fix any lexicon text files that may have been converted from lf to crlf..."
	    find $MM_BASEDIR/lexicon/data -name lexiconStatic2006\?\?\?\? -print -exec dos2unix -U {} \;
	fi
    fi
# done

LEXREC=`find $MM_BASEDIR/lexicon/data -name lex_rec -print | wc -l`
if [ $LEXREC == 0 ]; then
	echo "Warning: lexicon lookup files (lex_rec) are missing, cannot ensure correct operation of SemRep without them!"
	echo "Warning: lexicon lookup files (lex_rec) are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
	echo ""
	MISSINGFILES=`expr $MISSINGFILES + 1`
else
	find $MM_BASEDIR/lexicon/data -name lex_rec -print >> ./install.log 2>&1
fi

# MetaMap DB files
for DB in $AVAILABLE_DATA
do
    DB_BASE=`find $MM_BASEDIR/DB -name DB.\*.${DB}\*.base -print | wc -l`
    if [ $DB_BASE == 0 ]; then
	echo "Warning: UMLS base index files ($DB) are missing, cannot ensure correct operation of SemRep without them!"
	echo "Warning: UMLS base index files ($DB) are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
	echo ""
	MISSINGFILES=`expr $MISSINGFILES + 1`
    else 
	find $MM_BASEDIR/DB -name DB.\*.${DB}\*.base -print >> ./install.log 2>&1
    fi
done

for DB in $AVAILABLE_DATA
do
    DB_SMR=`find $MM_BASEDIR/DB -name DB.\*.${DB}\*.[smr]\* -print | wc -l`
    if [ $DB_SMR == 0 ]; then
	echo "Warning: UMLS index files ($DB) are missing, cannot ensure correct operation of SemRep without them!"
	echo "Warning: UMLS index files ($DB) are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
	echo ""
	MISSINGFILES=`expr $MISSINGFILES + 1`
    else 
	find $MM_BASEDIR/DB -name DB.\*.${DB}\*.[smr]\* -print >> ./install.log 2>&1
    fi
done

# While WSD is optional for MetaMap, it is required for SemRep
# MISSINGOPTIONS=0
# WSD Server files
STI=`find $MM_BASEDIR/WSD_Server -name wstvsc01-12-dc.txt.ser.gz -print | wc -l`
if [ $STI == 0 ]; then
  echo "Warning: WSD Server index files (WSD_Server/*.gz) are missing, cannot ensure correct operation of SemRep without them!"
  echo "Warning: WSD Server index files (WSD_Server/*.gz) are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
  echo ""
#  MISSINGOPTIONS=`expr $MISSINGOPTIONS + 1`
  MISSINGFILES=`expr $MISSINGFILES + 1`
else 
  find $MM_BASEDIR/WSD_Server/wstv-dc -name \*.ser -print >> ./install.log 2>&1
fi

find $MM_BASEDIR -name WSD_Server -print >> ./install.log 2>&1
if [ $? != 0 ]; then
  echo "Warning: WSD Server directory is missing, cannot ensure correct operation of SemRep without it!"
  echo "Warning: WSD Server directory is missing, cannot ensure correct operation of SemRep without it!" >> ./install.log 2>&1
  echo ""
#  MISSINGOPTIONS=`expr $MISSINGOPTIONS + 1`
  MISSINGFILES=`expr $MISSINGFILES + 1`
fi

echo "Checking for required SemRep datafiles"
echo "Checking for required SemRep datafiles" >> ./install.log 2>&1

# SemRep data files
ABGENE=`ls -1 $BASEDIR/DATA/ABGENE | wc -l`
if [ $ABGENE -lt 155 ]; then
  echo "Warning: Some ABGENE data files are missing, cannot ensure correct operation of SemRep without them!"
  echo "Warning: Some ABGENE data files are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
  echo ""
#  MISSINGOPTIONS=`expr $MISSINGOPTIONS + 1`
  MISSINGFILES=`expr $MISSINGFILES + 1`
else 
  ls -1 $BASEDIR/DATA/ABGENE/${OS} >> ./install.log 2>&1
fi

ENTREZGENE=`find $BASEDIR/DATA/ENTREZGENE -name HUMAN\*  -print | wc -l`
if [ $ENTREZGENE -lt 2 ]; then
  echo "Warning: Some ENTREZGENE data files are missing, cannot ensure correct operation of SemRep without them!"
  echo "Warning: Some ENTREZGENE data files are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
  echo ""
#  MISSINGOPTIONS=`expr $MISSINGOPTIONS + 1`
  MISSINGFILES=`expr $MISSINGFILES + 1`
else 
  find $BASEDIR/DATA/ENTREZGENE -name HUMAN\* -print >> ./install.log 2>&1
fi

for DB in $AVAILABLE_DATA
do
    UMLS_HIERARCHY=`find $BASEDIR/DATA/UMLS_HIERARCHY -name \*${DB}\* -print | wc -l`
    if [ $UMLS_HIERARCHY == 0 ]; then
	echo "Warning: No ${DB} UMLS hierarchy data files are found, cannot ensure correct operation of SemRep without them!"
	echo "Warning: No ${DB} UMLS hierarchy  data files are missing, cannot ensure correct operation of SemRep without them!" >> ./install.log 2>&1
	echo ""
#  MISSINGOPTIONS=`expr $MISSINGOPTIONS + 1`
	MISSINGFILES=`expr $MISSINGFILES + 1`
    else 
	find $BASEDIR/DATA/UMLS_HIERARCHY -name \*${DB}\* -print >> ./install.log 2>&1
    fi
done


if [ $MISSINGFILES != 0 ]; then
  echo "!! WARNING: Some necessary datafiles or directories are missing, see install.log for more information. !!"
  echo "!! WARNING: Some necessary datafiles or directories are missing, see install.log for more information. !!" >> ./install.log
else
  if [ "$DELETE_DATA" = "y" ]; then
      echo "Cleaning up..."
      for YEAR in $AVAILABLE_DATA
      do
	  rm $BASEDIR/DATA/LEXICON/*${YEAR}*
	  rm -rf $BASEDIR/DATA/DB/*${YEAR}*
      done
      DBF=`ls -1 $BASEDIR/DATA/DB | wc -l`
      if [ $DBF == 0 ]; then
	  rm -rf $BASEDIR/DATA/DB
      fi
      DBL=`ls -1 $BASEDIR/DATA/LEXICON | wc -l`
      if [ $DBL == 0 ]; then
	  rm -rf $BASEDIR/DATA/LEXICON
      fi
  fi
  echo Public SemRep Install complete.
  echo Public SemRep Install complete. >> ./install.log
fi
echo "$0 ended `date`"  >> ./install.log

export BASEDIR

if [ -f $BASEDIR/bin/install_dfb.sh ]; then 
  echo  "Would like to use a custom data set with SemRep (use data file builder)? [yN]:"
  read RESPONSE
  if [ "$RESPONSE" = "y" ]; then
     echo "running Data File Builder Install..."
     . $BASEDIR/bin/install_dfb.sh
  fi
fi

if [ -f $BASEDIR/bin/install_src.sh ]; then 
    echo "Running SemRep source development environment setup..."
    . $BASEDIR/bin/install_src.sh
fi

if [ -f $BASEDIR/bin/install_javaapi.sh ]; then 
    echo "Running SemRep Java API development environment setup..."
    . $BASEDIR/bin/install_javaapi.sh
fi

if [ -f $BASEDIR/bin/install_uima.sh ]; then 
    echo "Running SemRep UIMA API development environment setup..."
    . $BASEDIR/bin/install_uima.sh
fi

echo ""
echo "Public SemRep Install Settings:"
echo ""
echo "Public MetaMap basedir: $MM_BASEDIR"
echo "Public SemRep basedir: $BASEDIR"
echo "Public SemRep Program Dir: $BINDIR"
echo "Java Home dir: $JAVA_HOME"

echo "Public MetaMap basedir: $MM_BASEDIR" >> ./install.log
echo "Public SemRep basedir: $BASEDIR" >> ./install.log
echo "Public MetaMap Program Dir: $BINDIR" >> ./install.log
echo "Java Home dir: $JAVA_HOME" >> ./install.log
echo ""

exit 0
