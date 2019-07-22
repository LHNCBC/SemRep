
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
 * Signatures for Binary Search Map classes.
 *
 *
 * Created: Wed Jul 25 09:11:23 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">"Willie Rogers</a>
 * @version $Id: BinSearchMap.java,v 1.1 2006/08/18 19:04:21 wrogers Exp $
 */

public interface BinSearchMap 
{
  /** open map for reading */
  public static final int READ = 0;
  /** open map for writing */
  public static final int WRITE = 1;

  /**
   * @return get number of records in map
   */
  int getNumberOfRecords();

  /**
   * @return get length of data in each record
   */
  int getDataLength();
 
  /** close resources used by this map. */
  void close() throws IOException;

} // BinSearchMap
