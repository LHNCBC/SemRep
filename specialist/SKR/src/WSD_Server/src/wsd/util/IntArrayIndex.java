
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
import gov.nih.nlm.nls.utils.StringUtils;

public class IntArrayIndex implements Serializable, BinSearchMap {
  
  /** number of words in index */
  int wordnum;

  /** number of records in each partition */
  Map numrecs;

  /** hashlist of hash or tree maps for generating new indices. */
  transient Map hashlist = new HashMap(350, 0.96f);

  public static final String canonicalSerializedName = "IntArrayIndexInfo.ser";

  /** term counter */
  int i = 0;

  /** name of index */
  String indexname = null;
  /** directory where index resides */
  transient String indexDirectoryPath = null;

  /**
     @param indexDirectoryPath directory where index resides
     @return read only instance of IntArrayIndex.
   */
  public static IntArrayIndex getInstance(String indexDirectoryPath)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    StringBuffer strbuf = new StringBuffer();
    strbuf.append(indexDirectoryPath).append(File.separator).append(canonicalSerializedName);
    String serializedInfo =  strbuf.toString();
    if ( new File(serializedInfo).exists() ) {
      // System.out.println(" loading " + serializedInfo);
      FileInputStream istream = new FileInputStream(serializedInfo);
      ObjectInputStream p = new ObjectInputStream(istream);
      IntArrayIndex index = (IntArrayIndex)p.readObject();
      istream.close();
      index.indexDirectoryPath = indexDirectoryPath;
      return index;
    } else {
      System.err.println("  " + serializedInfo);
    }
    return null;
  }

  public IntArrayIndex()
  {
  }

  public IntArrayIndex(String indexname)
  {
    this.indexname = indexname;
  }

  public int getNumberOfRecords() { return this.wordnum; }
  public int getDataLength() { return -1; }
  public void close() throws IOException
  {

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
    List lineList;
    int i = 0;
    this.indexDirectoryPath = indexDirectoryPath;
    TemporaryIntArrayPostingsPool pool = 
      new TemporaryIntArrayPostingsPool(indexDirectoryPath + File.separator +
                                        "tposts", "rw");
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
	  lineList = StringUtils.split(line, " ");
	  if (lineList.size() > 0) {
            key = (String)lineList.get(0);
	  } else {
	    System.err.println("lineList size <= 0, line = " + line);
	  }
	}
	String keyLength = Integer.toString(key.length());
	bucket = (Map)this.hashlist.get(this.indexname+keyLength);
	if (bucket == null ) {
	  bucket = new TreeMap();
	  this.hashlist.put(this.indexname+keyLength, bucket);
	  bucket.put(key, new Integer(pool.add(line)));
	} else {
          bucket.put(key, new Integer(pool.add(line)));
	}
	this.wordnum++;
// 	System.out.println("bucket.put(key: " + key + ", value), " + 
// 	 "keylength: " + keyLength + ", bucket size: " + bucket.size());
//         System.out.println("address:" + bucket.get(key) + ", line: " + line);
      }
    reader.close();
    tstatfp.println("# of input lines: " + i );
    tstatfp.println("# of buckets: " + hashlist.size());
    Iterator iter = hashlist.keySet().iterator();
    while (iter.hasNext()) {
      Object hkey = iter.next();
      Map bucket = (Map)hashlist.get(hkey);
      tstatfp.println("bucket: key: " + hkey + ", size: " + bucket.size());
    }
    pool.close();
    tstatfp.close();
    System.out.println();
  }

  public void buildInvertedFile( String indexDirectoryPath, Map aTermMap, String partitionId)
    throws IOException
  {
    int nextpost = 0;
    int numrecs = 0;
    TemporaryIntArrayPostingsPool pool = 
      new TemporaryIntArrayPostingsPool(indexDirectoryPath + File.separator + "tposts", "r");

    DataBinSearchMap intPartition = 
      new DataBinSearchMap ( indexDirectoryPath + File.separator + "partition_" + partitionId, 
				   BinSearchMap.WRITE );
    
    Iterator keyIter = aTermMap.keySet().iterator();
    while (keyIter.hasNext()) {
      String termKey = (String)keyIter.next();
      int link = ((Integer)aTermMap.get(termKey)).intValue();
      int[] posting = pool.get(link);
      // if (this.debug) {
//       System.out.print("term: " + termKey);
//       System.out.println(", postings length: " + posting.length);
//       System.out.print("address:" + link + ", posting: ");
//       for (int k = 0; k < posting.length; k++) {
//         System.out.print(posting[k] + " ");
//       }
//       System.out.println();
// }
      if ((this.i % 1000) == 0) System.out.print(i + "*");
      this.i++;
      // write dictionary entry
      intPartition.writeIntArrayEntry(termKey, posting);
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
	
	buildInvertedFile(indexDirectoryPath, map, partitionKey);
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

  public IntArrayBinSearchPool getIntArrayBinSearchPool()
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    return new IntArrayBinSearchPool(this.indexDirectoryPath, this.indexname);
  }

  public static void main(String[] args)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    if (args.length < 3) {
      System.out.println("usage: wsd.util.IntArrayIndex [index <sourcefile> <indexdir>] [get <indexdir> <term>]");
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
      IntArrayIndex iai = new IntArrayIndex("IntArray");
      iai.load_map(srcfilename, indexdirpath);
      iai.create_index(indexdirpath);
      // delete temporary files.
    } else if (args[0].equals("get")) {
      String indexdirpath = args[1];
      String term = args[2];
      int STVECTORSIZE = 129;
      int termVector[]   = new int[STVECTORSIZE];
      IntArrayIndex iai = IntArrayIndex.getInstance(indexdirpath);
      IntArrayBinSearchPool iabsp = iai.getIntArrayBinSearchPool();
      int[] ia = iabsp.get(term, termVector);
      if (ia == null) {
        System.out.println("term: " + term + " not found");
      } else {
        System.out.println("term: " + term);
        for (int i = 0; i<ia.length; i++) {
          System.out.print( ia[i] + " ");
        }
        System.out.println();
      }
    } else {
      System.out.println("usage: wsd.util.IntArrayIndex [index <sourcefile> <indexdir>] [get <indexdir> <term>]");
      System.exit(0);
    }
  }
}
