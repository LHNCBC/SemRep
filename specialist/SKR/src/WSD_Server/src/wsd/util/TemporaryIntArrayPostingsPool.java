
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

class TemporaryIntArrayPostingsPool {
  String postingsFilename = "tpost";
  transient private RandomAccessFile postingsRAF = null;
  static final int BUFFER_SIZE = 1500;
  byte[] buffer = new byte[BUFFER_SIZE];
  int lastIndex = 0;
  /** string containing known punctuation */
  public static final String PUNCTUATION = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
  /** string containing known whitespace */
  public static final String WHITESPACE = " \t\n\r\f";


  public TemporaryIntArrayPostingsPool (String aPostingsFilename, String mode)
  {
    this.postingsFilename = aPostingsFilename;
    try {
      this.postingsRAF =  new RandomAccessFile(this.postingsFilename, mode);
    } catch (FileNotFoundException exception) {
      exception.printStackTrace(System.err);
    }
  }

  /**
   * @param posting integer vector data to post in ASCII String format
   * @return address of posting
   */
  public int add(String posting)
  {
    StringTokenizer st = new StringTokenizer(posting, this.WHITESPACE);
    int[] termVector = new int[st.countTokens() - 1];
    int i = 0;
    st.nextToken(); // skip first token, the key
    while (st.hasMoreTokens()) {
      String coeff = st.nextToken();
      // System.out.println("coeff: " + coeff);
      termVector[i] = Integer.parseInt(coeff);
      i++;
    }
    return this.add(termVector);
  }

  /**
   * @param posting integer vector data to post 
   * @return address of posting
   */
  public int add(int[] posting)
  {
    int address = -1;
    try {
      this.postingsRAF.writeInt(posting.length);
      for (int i = 0; i<posting.length; i++)
        this.postingsRAF.writeInt(posting[i]);
      address = this.lastIndex;
      this.lastIndex = this.lastIndex + (posting.length*4) + 4;
    } catch (Exception exception) {
      System.err.println("add(): exception: " + exception.getMessage());
    }
    return address;
  }

  public int[] get(int address)
  {
    try {
      this.postingsRAF.seek(address);
      int length = this.postingsRAF.readInt();
      int[] posting = new int[length];
      for (int i = 0; i<length; i++)
        posting[i] = this.postingsRAF.readInt();
      return posting;
    } catch (Exception exception) {
      System.err.println("TemporaryIntArrayPostingPool:get(): exception: " + exception.getMessage());
    }
    return null;
  }


  public void close()
  { 
    try {
      this.postingsRAF.close();
    } catch (Exception exception) {
      System.err.println("get(): exception: " + exception.getMessage());
    }
  }
}
