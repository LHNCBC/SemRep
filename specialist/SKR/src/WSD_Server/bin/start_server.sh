
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
JAVA_HOME=/nfsvol/nls/tools/Linux-i686/jdk1.6.0_11

if [ ! -n "${WSD}" ]
then
  if [ ! -n "${NLS}" ]
  then
    NLS=/nfsvol/nls
    export NLS
  fi
  WSD=$NLS/tools/Disambiguator
  export WSD
fi

JAVA=${JAVA_HOME}/bin/java
JAR_FILE=$WSD/WSD_Server/lib/wsd.jar:$WSD/WSD_Server/lib/kss-api.jar:$WSD/WSD_Server/lib/thirdparty.jar:$WSD/WSD_Server/lib/db.jar:$WSD/WSD_Server/lib/log4j-1.2.8.jar
JVMOPTIONS="-Xmx1g -Dserver.config.file=$WSD/WSD_Server/config/disambServer.cfg"
LD_LIBRARY_PATH=$WSD/WSD_Server/lib:/net/pax/usr/local/BerkeleyDB.4.1/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

if [ $# = 0 ]
then
	rm -rf $WSD/WSD_Server/log/WSD_Server.log*
	$JAVA $JVMOPTIONS -classpath $JAR_FILE wsd.server.DisambiguatorServer  & echo $! > pid
else
	echo Unexpected number of arguments.
	echo Usage: start_server.sh
fi

