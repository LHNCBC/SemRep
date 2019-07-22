
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

/**
 * DataBinSearchMap.java
 * <p>
 * organization of one record:
 * <pre>
 *  +------------------------+-------------------+
 *  | term                   |      data         |
 *  +------------------------+-------------------+
 *  |<---- term length ----->|<-- data length -->|
 *  |<------------- record length -------------->|
 * </pre>
 *
 *  Term Length And Data Length Is The Same For All Records In Map.
 *  </p>
 *
 * Created: Wed Jul 25 09:11:23 2001
 *
 * @author <a href="mailto: "Willie Rogers</a>
 * @version $Id: DataBinSearchMap.java,v 1.2 2006/09/25 18:28:00 wrogers Exp $
 */

public class DataBinSearchMap implements BinSearchMap, Serializable 
{

  /** data output stream for writing map. */
  private transient DataOutputStream mapWriter;
  /** random access file for reading map. */
  private transient RandomAccessFile mapRAFile;
  /** number of records in this map. */
  int numberOfRecords = 0;
  /** termLength of all terms in this map. */
  private int termLength = 0;
  /** data length of data associated with terms for each record in map */
  private int dataLength = 0;

  public static final String canonicalSerializedName = "DataBinSearchMap.ser";
  /** filename of map */
  transient String filename;

  /**
   * Instantiate a new or existing binary search map with data of a fixed length
   * @param mapFilename filename of map 
   * @param mode        file mode to use: utils.BinSearchMap.WRITE to open map for writing, and
   *                     utils.BinSearchMap.READ to open map for reading.
   */
  public DataBinSearchMap (String mapFilename, int mode )
    throws FileNotFoundException
  {
    if ( mode == WRITE ) {
      this.mapWriter = 
	new DataOutputStream ( new BufferedOutputStream
			       ( new FileOutputStream ( mapFilename )));
    } else {
      this.mapRAFile = new RandomAccessFile ( mapFilename, "r");
    }
    this.filename = mapFilename;    
  }

  public static DataBinSearchMap getInstance(String filename)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    StringBuffer strbuf = new StringBuffer();
    strbuf.append(filename).append("_").append(canonicalSerializedName);
    String serializedInfo =  strbuf.toString();
    if ( new File(serializedInfo).exists() ) {
      // System.out.println(" loading " + serializedInfo);
      FileInputStream istream = new FileInputStream(serializedInfo);
      ObjectInputStream p = new ObjectInputStream(istream);
      DataBinSearchMap index = (DataBinSearchMap)p.readObject();
      istream.close();
      index.mapRAFile = new RandomAccessFile ( filename, "r");
      index.filename = filename;
      return index;
    } else {
      System.err.println("  " + serializedInfo);
    }
    return null;
  }

  public void serializeMapInfo()
    throws FileNotFoundException, IOException
  {
    /* serialize info on object to indexDirectoryPath/<Canonical Serialized Name> */
    FileOutputStream ostream = 
      new FileOutputStream(this.filename + "_" + canonicalSerializedName);
    ObjectOutputStream p = new ObjectOutputStream(ostream);
    p.writeObject(this);
    p.flush();
    p.close();
    ostream.close();
  }

  /**
   * Write an entry into map.
   * @param term term 
   * @param data data to be assoicated with term
   */
  public void writeEntry(String term, byte[] data)
    throws IOException
  {
    // write dictionary entry
    this.mapWriter.writeBytes(term);
    this.mapWriter.write(data, 0, data.length);
    this.numberOfRecords++;
    this.termLength = term.length();
    this.dataLength = data.length;
  }

  /**
   * Write an entry into map.
   * @param term term 
   * @param data data to be assoicated with term
   */
  public void writeIntArrayEntry(String term, int[] data)
    throws IOException
  {
    // write dictionary entry
    this.mapWriter.writeBytes(term);
    for (int i = 0; i<data.length; i++)
      this.mapWriter.writeInt(data[i]);
    this.numberOfRecords++;
    this.termLength = term.length();
    this.dataLength = data.length;
  }

  /**
   * get data entry for term 
   * @param term term 
   * @return byte array containing value associated with term
   */
  public byte[] get(String term)
    throws IOException
  {
    if (this.mapRAFile == null ) {
       this.mapRAFile = new RandomAccessFile ( this.filename, "r");
    }
    return DiskBinarySearch.binarySearch(this.mapRAFile, term, term.length(), this.numberOfRecords, this.dataLength);
  }

  /**
   * get data entry for term 
   * @param term term 
   * @return byte array containing value associated with term
   */
  public int[] getIntArray(String term)
    throws IOException
  {
    if (this.mapRAFile == null ) {
       this.mapRAFile = new RandomAccessFile ( this.filename, "r");
    }
    return DiskBinarySearch.intArrayBinarySearch(this.mapRAFile, term, term.length(), this.numberOfRecords, this.dataLength);
  }

  /**
   * get data entry for term 
   * @param term term 
   * @return byte array containing value associated with term
   */
  public int[] getIntArrayRef(String term, int[] intArray)
    throws IOException
  {
    if (this.mapRAFile == null ) {
       this.mapRAFile = new RandomAccessFile ( this.filename, "r");
    }
    synchronized(this.mapRAFile) {
      return DiskBinarySearch.intArrayBinarySearch(this.mapRAFile, term, term.length(), this.numberOfRecords, this.dataLength, intArray);
    }
  }

  /**
   * get length of term in each record
   */
  public int getTermLength()
  {
    return this.termLength;
  }

  /**
   * @return get number of records in map
   */
  public int getNumberOfRecords()
  {
    return this.numberOfRecords;
  }

  /**
   * @return get length of data in each record
   */
  public int getDataLength()
  {
    return this.dataLength;
  }

  /** close resources used by this map. */
  public void close()
    throws IOException
  {
    if (this.mapRAFile != null ) {
      this.mapRAFile.close();
    }
    if (this.mapWriter != null ) {
      this.mapWriter.close();
    }
  }

}// DataBinSearchMap
