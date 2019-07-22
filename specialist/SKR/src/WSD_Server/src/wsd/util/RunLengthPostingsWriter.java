
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
 * RunLengthPostings.java
 *
 *
 * Created: Wed Jul 25 09:13:06 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: RunLengthPostingsWriter.java,v 1.1 2006/09/25 18:34:05 wrogers Exp $
 */
// organization:
//  +------------------------+-------------------+------+
//  | byte length of posting |      posting      | .... |
//  +------------------------+-------------------+------+
//  |<------ 4 bytes ------->|<-- byte length -->|

public class RunLengthPostingsWriter implements Serializable
{
  transient DataOutputStream postingsWriter = null;
  String directoryName;
  int nextPosting = 0;

  /**
   * @param aDirectoryName directory in which postings file resides.
   */
  public RunLengthPostingsWriter (String aDirectoryName)
    throws FileNotFoundException
  {
    this.postingsWriter = 
      new DataOutputStream ( new BufferedOutputStream
			     (new FileOutputStream
			      (aDirectoryName + "/postings" )));
    this.directoryName = aDirectoryName;
  }

  /** 
   * if postings object was loaded from a serialized object then 
   * intialize i/o for writing using this method.
   */
  public void initializeIO()
    throws FileNotFoundException
  {
    if (this.postingsWriter == null) {
      this.postingsWriter = 
	new DataOutputStream ( new BufferedOutputStream
			       (new FileOutputStream
				(this.directoryName + "/postings" )));
    }
  }

  /**
   * write a data record into the postings.
   * @param aDataRecord data to be written into postings.
   * @return size of postings in bytes.
   */
  public int writeString(String aDataRecord)
    throws IOException
  {
    int currentPosting = this.nextPosting;
    this.postingsWriter.writeInt(aDataRecord.length());
    this.postingsWriter.writeBytes(aDataRecord);
    this.nextPosting = this.nextPosting + aDataRecord.length() + 4;
    return currentPosting;
  }

  /** close resources used by writer. */
  public void close()
    throws IOException
  {
    if (this.postingsWriter != null) {
      this.postingsWriter.close();
    }
  }

}// RunLengthPostingsWriter
