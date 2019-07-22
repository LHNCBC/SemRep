
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

if [ $# = 0 ]; then
    echo "usage: $0 srctable dbfilename"
    exit 0
fi

#DC_ST_TABLE=dc-st.gt10.txt
DC_ST_TABLE=wstvsc01-12-dc.txt

LIB=$WSD/WSD_Server/lib
JAR_FILE=$LIB/wsd.jar:$LIB/kss-api.jar:$LIB/thirdparty.jar:$LIB/db.jar

LD_LIBRARY_PATH=$WSD/WSD_Server/lib:/usr/local/BerkeleyDB.4.1/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

java -classpath $JAR_FILE wsd.util.StiPopulateDb $1 $2


exit 1