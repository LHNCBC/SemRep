package entrezgene;

import com.sleepycat.db.DatabaseEntry;
import com.sleepycat.db.Database;
import com.sleepycat.db.internal.DbConstants;

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
public class StringDbt extends DatabaseEntry {

  /**
   * Default constructor. No parameters.
   */
  public StringDbt()
  {
      //set_flags(DbConstants.DB_DBT_MALLOC); // tell Db to allocate on retrieval
     // setFlag(DbConstants.DB_DBT_MALLOC);
    setReuseBuffer(false);
  }

  /**
   * Constructor that uses a byte-array.
   *
   * @param arr byte-array used to create a StringDbt object.
   */
  public StringDbt(byte[] arr)
  {
      //set_flags(DbConstants.DB_DBT_USERMEM);
      setReuseBuffer(true);
      setData(arr);
      setSize(arr.length);
  }

  /**
   * Constructor that uses a String.
   *
   * @param value the string used to create a StringDbt object.
   */
  public StringDbt(String value)
  {
      setString(value);
      //set_flags(DbConstants.DB_DBT_MALLOC); // tell Db to allocate on retrieval
     //setFlag(DbConstants.DB_DBT_MALLOC);
     setReuseBuffer(false);
  }

  /**
   * Creates a database key or data field from a string.
   *
   * @param value a string that will be used to create a key or data value.
   */
  public void setString(String value)
  {
      setData(value.getBytes());
      setSize(value.length());
      // must set ulen because sometimes a string is returned
      //set_ulen(value.length());
     setUserBuffer(value.length(),true);
  }
  /**
   * Returns the string representation of a key or data value.
   *
   * @return  the string representation of the data of a Dbt object.
   */
  public String getString()
  {
      return new String(getData(), 0, getSize());
  }

//  private void setFlags(final int flag) {
//	flags &= flag;
 // }
}
