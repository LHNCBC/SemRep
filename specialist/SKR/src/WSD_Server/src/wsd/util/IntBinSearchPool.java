
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

package wsd.util;

/**
 * A pool of Integer Binary Search Maps
 *
 * @version $Id: IntBinSearchPool.java,v 1.1 2006/09/20 20:48:35 wrogers Exp $
 */ 

import java.io.*;
import java.util.*;

public class IntBinSearchPool
{
  /** pool of IntBinSearchMaps */
  Map pool = new HashMap(20);

  String indexname;
  String indexDirectoryPath;

  public IntBinSearchPool(String indexDirectoryPath, String indexname) {
    this.indexDirectoryPath = indexDirectoryPath;
    this.indexname = indexname;
  }

  public String getPartitionId(String term)
  {
    String keyLength = Integer.toString(term.length());
    return this.indexname+keyLength;
  }

  public IntBinSearchMap getIntBinSearchMap(String partitionId)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    return IntBinSearchMap.getInstance(this.indexDirectoryPath + File.separator + "partition_" + partitionId);
  }

  public int get(String term)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    IntBinSearchMap ibsm = null;
    String partitionId = getPartitionId(term);
    if (pool.containsKey(partitionId)) {
      ibsm = (IntBinSearchMap)pool.get(partitionId);
    } else {
      ibsm = getIntBinSearchMap(partitionId);
      pool.put(partitionId,ibsm);
    }
    if (ibsm != null)
      return ibsm.get(term);
    else 
      return -1;
  }

  public void close()
    throws IOException
  {
    Iterator poolIter = pool.values().iterator();
    while (poolIter.hasNext()) {
      ((IntBinSearchMap)poolIter.next()).close();
    }
  }
}

