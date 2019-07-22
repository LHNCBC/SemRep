
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

import com.sleepycat.db.Dbt;
import com.sleepycat.db.Db;

/**
 * This class is used for convenience in creating key and data elements when
 * the b-tree representations are created or data is retrieved from the
 * b-tree. BerkeleyDB API methods use byte arrays to communicate with database.
 * In order to simplify byte array-string conversions and flag setting procedures,
 * this class was extended from Dbt class.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class StringDbt extends Dbt {

  /**
   * Default constructor. No parameters.
   */
  public StringDbt()
  {
      set_flags(Db.DB_DBT_MALLOC); // tell Db to allocate on retrieval
  }

  /**
   * Constructor that uses a byte-array.
   *
   * @param arr byte-array used to create a StringDbt object.
   */
  public StringDbt(byte[] arr)
  {
      set_flags(Db.DB_DBT_USERMEM);
      set_data(arr);
      set_size(arr.length);
  }

  /**
   * Constructor that uses a String.
   *
   * @param value the string used to create a StringDbt object.
   */
  public StringDbt(String value)
  {
      setString(value);
      set_flags(Db.DB_DBT_MALLOC); // tell Db to allocate on retrieval
  }

  /**
   * Creates a database key or data field from a string.
   *
   * @param value a string that will be used to create a key or data value.
   */
  public void setString(String value)
  {
      set_data(value.getBytes());
      set_size(value.length());
      // must set ulen because sometimes a string is returned
      set_ulen(value.length());
  }
  /**
   * Returns the string representation of a key or data value.
   *
   * @return  the string representation of the data of a Dbt object.
   */
  public String getString()
  {
      return new String(get_data(), 0, get_size());
  }
}