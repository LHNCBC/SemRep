
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
 * Describe class IntIndex here.
 *
 *
 * Created: Fri Aug 25 16:14:13 2006
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */

import java.util.*;
import java.io.*;
import gov.nih.nlm.nls.utils.StringUtils;

public class IntIndex implements Serializable, BinSearchMap {

  /** number of words in index */
  int wordnum;

  /** number of records in each partition */
  Map numrecs;

  /** hashlist of hash or tree maps for generating new indices. */
  transient Map hashlist = new HashMap(350, 0.96f);

  public static final String canonicalSerializedName = "IntIndexInfo.ser";

  /** term counter */
  int i = 0;

  /** name of index */
  String indexname = null;

  /** directory where index resides */
  transient String indexDirectoryPath = null;

  /**
   * Creates a new <code>IntIndex</code> instance.
   *
   */
  public IntIndex() {

  }

  /**
   * Creates a new <code>IntIndex</code> instance.
   *
   */
  public IntIndex(String indexDirectoryPath, String indexname) {
    this.indexDirectoryPath = indexDirectoryPath;
    this.indexname = indexname;

  }

  public int getNumberOfRecords() { return this.wordnum; }
  public int getDataLength() { return 4; /*bytes*/ }
  public void close() throws IOException
  {

  }


  /**
     @param indexDirectoryPath directory where index resides
     @return read only instance of IntArrayIndex.
   */
  public static IntIndex getInstance(String indexDirectoryPath)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    StringBuffer strbuf = new StringBuffer();
    strbuf.append(indexDirectoryPath).append(File.separator).append(canonicalSerializedName);
    String serializedInfo =  strbuf.toString();
    if ( new File(serializedInfo).exists() ) {
      // System.out.println(" loading " + serializedInfo);
      FileInputStream istream = new FileInputStream(serializedInfo);
      ObjectInputStream p = new ObjectInputStream(istream);
      IntIndex index = (IntIndex)p.readObject();
      istream.close();
      index.indexDirectoryPath = indexDirectoryPath;
      return index;
    } else {
      System.err.println("  " + serializedInfo);
    }
    return null;
  }

  /**
   * load table into in-memory term -> value map.
   * Column zero is used as key for index.
   * @exception FileNotFoundException if an error occurs
   * @exception IOException if an error occurs
   */
  public void load_map(String srcfilename, String indexDirectoryPath)
     throws FileNotFoundException, IOException
  {
    // Load records into buckets based on term length.  Each bucket is
    // a TreeMap where record is stored by the ordinal value of the
    // first element (key) of the record.
    String line;
    String key = null;
    String value = null;
    List lineList;
    int i = 0;
    this.indexDirectoryPath = indexDirectoryPath;
    PrintWriter tstatfp = new PrintWriter
      (new BufferedWriter(new FileWriter( indexDirectoryPath + File.separator +
                                          "tpost.stats")));    
    BufferedReader reader = 
      new BufferedReader(new FileReader( srcfilename ));
    System.out.print("*building temporary index and in memory dictionary*");
    while ( (line = reader.readLine()) != null )
      {
	Map bucket;
        if ((i % 1000) == 0) System.out.print(i + "*");
	i++;
	if (line.trim().length() > 0) {
	  lineList = StringUtils.split(line, "|");
	  if (lineList.size() > 0) {
            key = (String)lineList.get(0);
	    value = (String)lineList.get(1);
	  } else {
	    System.err.println("lineList size <= 0, line = " + line);
	  }
	}
	String keyLength = Integer.toString(key.length());
	bucket = (Map)this.hashlist.get(this.indexname+keyLength);
	if (bucket == null ) {
	  bucket = new TreeMap();
	  this.hashlist.put(this.indexname+keyLength, bucket);
	  bucket.put(key, new Integer(value));
	} else {
          bucket.put(key, new Integer(value));
	}
	this.wordnum++;
// 	System.out.println("bucket.put(key: " + key + ", value), " + 
// 	 "keylength: " + keyLength + ", bucket size: " + bucket.size());
//         System.out.println("address:" + bucket.get(key) + ", line: " + line);
      }
    reader.close();
    Iterator iter = hashlist.keySet().iterator();
    while (iter.hasNext()) {
      Object hkey = iter.next();
      Map bucket = (Map)hashlist.get(hkey);
      tstatfp.println("bucket: key: " + hkey + ", size: " + bucket.size());
    }
    tstatfp.close();
    tstatfp.println("# of input lines: " + i );
    tstatfp.println("# of buckets: " + hashlist.size());
    System.out.println();
  }


  public void buildPartition( String indexDirectoryPath, Map aTermMap, String partitionId)
    throws IOException
  {
    int nextpost = 0;
    int numrecs = 0;

    IntBinSearchMap intPartition = 
      new IntBinSearchMap ( indexDirectoryPath + File.separator + "partition_" + partitionId, 
				   BinSearchMap.WRITE );
    
    Iterator keyIter = aTermMap.keySet().iterator();
    while (keyIter.hasNext()) {
      String termKey = (String)keyIter.next();
      int value = ((Integer)aTermMap.get(termKey)).intValue();
      // if (this.debug) {
//       System.out.print("term: " + termKey);
//       System.out.print("value:" + value);
//       System.out.println();
// }
      if ((this.i % 1000) == 0) System.out.print(i + "*");
      this.i++;
      // write dictionary entry
      intPartition.writeEntry(termKey, value);
    }
    this.numrecs.put(partitionId, new Integer(intPartition.getNumberOfRecords()));
    // System.out.println("key: " + key );
    intPartition.close();
    intPartition.serializeMapInfo();
  }


  public void create_index(String indexDirectoryPath)
    throws IOException
  {
    this.numrecs = new HashMap(5);
    PrintWriter statfp = new PrintWriter
      (new BufferedWriter(new FileWriter( indexDirectoryPath + File.separator +
                                          "partition.stats")));
    statfp.println( "# " + indexDirectoryPath + File.separator + "partition.log -- bsp_map.tcl status file" );
    statfp.println( "# total number of terms: " + this.wordnum );
    statfp.println( "#" );
    statfp.println( "# table format: " );
    statfp.println( "#  partition_filename termlen nterms" );

    this.i = 0;
    System.out.print("*creating final index*");
    Iterator iter = this.hashlist.keySet().iterator();
    while (iter.hasNext()) 
      {
	String partitionKey = (String)iter.next();
	Map map = (Map)this.hashlist.get(partitionKey);
	
	buildPartition(indexDirectoryPath, map, partitionKey);
	int keylength = 0;
	Iterator mapIter = map.keySet().iterator();
	if (mapIter.hasNext()) {
	  keylength = ((String)mapIter.next()).length();
	}
	statfp.println( "partition_" + partitionKey + " " + 
			keylength + " " + map.size());
      }    
    statfp.close();
    /* serialize info on object to indexDirectoryPath/<Canonical Serialized Name> */
    FileOutputStream ostream = 
      new FileOutputStream(indexDirectoryPath + File.separator +
                           canonicalSerializedName);
    ObjectOutputStream p = new ObjectOutputStream(ostream);
    p.writeObject(this);
    p.flush();
    p.close();
    ostream.close();
    System.out.println("done*");
  }
  
  public IntBinSearchPool getIntBinSearchPool()
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    return new IntBinSearchPool(this.indexDirectoryPath, this.indexname);
  }


 public static void main(String[] args)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    if (args.length < 3) {
      System.out.println("usage: wsd.util.IntIndex [index <sourcefile> <indexdir>] [get <indexdir> <term>]");
      System.exit(0);
    }
    if (args[0].equals("index")) {
      String srcfilename = args[1];
      String indexdirpath = args[2];
      File indexdir = new File(indexdirpath);
      if (! indexdir.exists()) 
        indexdir.mkdir();
      if (! indexdir.isDirectory()) {
        System.out.println(indexdir + " is not a directory!");
        System.exit(0);
      }
      IntIndex index = new IntIndex(indexdirpath, "intdata");
      index.load_map(srcfilename, indexdirpath);
      index.create_index(indexdirpath);
      // delete temporary files.
    } else if (args[0].equals("get")) {
      String indexdirpath = args[1];
      String term = args[2];
      IntIndex index = IntIndex.getInstance(indexdirpath);
      IntBinSearchPool ibsp = index.getIntBinSearchPool();
      int value = ibsp.get(term);
      if (value == -1) {
        System.out.println("term: " + term + " not found");
      } else {
        System.out.println("term: " + term + ", value: " + value);
      }
    } else {
      System.out.println("usage: wsd.util.IntIndex [index <sourcefile> <indexdir>] [get <indexdir> <term>]");
      System.exit(0);
    }
  }
}
