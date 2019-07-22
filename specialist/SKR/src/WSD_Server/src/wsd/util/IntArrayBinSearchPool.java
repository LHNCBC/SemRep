
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
import java.io.*;
import java.util.*;

public class IntArrayBinSearchPool
{
  /** pool of DataBinSearchMaps */
  Map pool = new HashMap(20);

  String indexname;
  String indexDirectoryPath;

  public IntArrayBinSearchPool(String indexDirectoryPath, String indexname) {
    this.indexDirectoryPath = indexDirectoryPath;
    this.indexname = indexname;
  }

  public String getPartitionId(String term)
  {
    String keyLength = Integer.toString(term.length());
    return this.indexname+keyLength;
  }

  public DataBinSearchMap getDataBinSearchMap(String partitionId)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    return DataBinSearchMap.getInstance(this.indexDirectoryPath + File.separator + "partition_" + partitionId);
  }

  public int[] get(String term)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    DataBinSearchMap dbsm = null;
    String partitionId = getPartitionId(term);
    if (pool.containsKey(partitionId)) {
      dbsm = (DataBinSearchMap)pool.get(partitionId);
    } else {
      dbsm = getDataBinSearchMap(partitionId);
      synchronized (pool) {
        pool.put(partitionId,dbsm);
      }
    }
    synchronized (dbsm)
      {
        return dbsm.getIntArray(term);
      }
  }
  
  public int[] get(String term, int[] intArray)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    DataBinSearchMap dbsm = null;
    String partitionId = getPartitionId(term);
    if (pool.containsKey(partitionId)) {
      dbsm = (DataBinSearchMap)pool.get(partitionId);
    } else {
      dbsm = getDataBinSearchMap(partitionId);
      synchronized (pool) {
        pool.put(partitionId,dbsm);
      }
    }
    if (dbsm == null) 
      return null;
    else 
      synchronized (dbsm)
        {
          return dbsm.getIntArrayRef(term, intArray);
        }
  }

  public void close()
    throws IOException
  {
    Iterator poolIter = pool.values().iterator();
    while (poolIter.hasNext()) {
      ((DataBinSearchMap)poolIter.next()).close();
    }
  }
}

