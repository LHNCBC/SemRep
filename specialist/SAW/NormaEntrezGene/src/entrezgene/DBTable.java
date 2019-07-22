package entrezgene;

import java.io.*;
import com.sleepycat.db.*;

public class DBTable {
  private Database mDb;
  private Environment mdbEnv;
  private String fileName;

  public DBTable(Environment env, String fn, boolean clear) {
    mdbEnv = env;
    fileName = fn;

    try {

        DatabaseConfig config = new DatabaseConfig();

        config.setErrorStream(System.err);
	config.setPageSize(1024);           // 1K page sizes.
        config.setBtreeRecordNumbers(true);
        config.setType(DatabaseType.BTREE);
        config.setAllowCreate(true);
	mDb = mdbEnv.openDatabase(null,fileName,null,config);
	//      mDb = new Database(fileName, null, config);
     /* mDb = new Database(mdbEnv,0);
      mDb.set_error_stream(System.err);
      mDb.open(fileName,null,Database.DB_BTREE, Database.DB_CREATE, 0664);*/
      if (clear)
      {
        //mDb.remove(fileName,null,0);
	//Database.remove(fileName,null,config);
	  mdbEnv.removeDatabase(null,fileName,fileName);
      }
    } catch (Exception e) {
      System.out.println("Error opening table: " + e.getMessage());
      e.printStackTrace();
    }
  }

  public void close() {
    try {
      mDb.close();
    } catch (Exception e) {
      System.out.println("Error Closing DBTable: " + e.getMessage());
      e.printStackTrace();
    }
  }

  public Database getDB() {
    return mDb;
  }

/*  public int getInt(String aKey) {
    int value = 0;
    try {
      DatabaseEntry theData = new DatabaseEntry();
      if (mDb.get(null, new DatabaseEntry(aKey.getBytes("UTF-8")),
                  theData, LockMode.DEFAULT) == OperationStatus.SUCCESS)
        value = IntegerBinding.entryToInt(theData);
    } catch (Exception e) {
      e.printStackTrace();
    }
    return value;
  }

        public void putInt(String aKey, int value) {
                DatabaseEntry dataEntry = new DatabaseEntry();
                IntegerBinding.intToEntry(value, dataEntry);
                try {
                        OperationStatus status = mDb.put(null, new DatabaseEntry(aKey
                                        .getBytes("UTF-8")), dataEntry);

                        if (status != OperationStatus.SUCCESS) {
//                                logger.info("Data insertion got status " + status);
                          System.out.println("Data insetion got status " + status);
                        }

                } catch (Exception e) {
//                        logger.log(Level.WARNING, "Error inserting count ", e);
                  System.out.println("WARNING: Error inserting count ");
                  e.printStackTrace();
                }
        }

        public double getDouble(String aKey) {
                double value = 0.0;
                try {
                        DatabaseEntry theData = new DatabaseEntry();

                        if (mDb.get(null, new DatabaseEntry(aKey.getBytes("UTF-8")),
                                        theData, LockMode.DEFAULT) == OperationStatus.SUCCESS)
                                value = DoubleBinding.entryToDouble(theData);
                } catch (Exception e) {
//                        logger.log(Level.WARNING, "Error getting count ", e);
                  e.printStackTrace();
                }
                return value;
        }


        public void putDouble(String aKey, double value) {
                DatabaseEntry dataEntry = new DatabaseEntry();
                DoubleBinding.doubleToEntry(value, dataEntry);
                try {
                        OperationStatus status = mDb.put(null, new DatabaseEntry(aKey
                                        .getBytes("UTF-8")), dataEntry);

                        if (status != OperationStatus.SUCCESS) {
//                                logger.info("Data insertion got status " + status);
                          System.out.println("Data insertion got status " + status);
                        }

                } catch (Exception e) {
//                        logger.log(Level.WARNING, "Error inserting count ", e);
                  System.out.println("WARNING: Error inserting double");
                  e.printStackTrace();
                }
        }
*/
        public String getString(String aKey) {
          StringDbt key = new StringDbt(aKey);
          StringDbt data = new StringDbt();
          try
          {
              if (mDb.get(null,key,data,LockMode.DEFAULT) == OperationStatus.SUCCESS)
                return data.getString();
              return "";
          }
          catch (DatabaseException dbe)
          {
              System.err.println("Error retrieving value for " + aKey);
              dbe.printStackTrace();
              return null;
          }
        }

        public void putString(String aKey, String aValue) {
          StringDbt key = new StringDbt(aKey);
          StringDbt value = new StringDbt(aValue);
          int err = 0;
          try {
		OperationStatus status = mDb.put(null,key,value);
                if (status != OperationStatus.SUCCESS) {
              		System.err.println("Problem with adding the key/data: " + key + "/" +
                                 value);
              		System.err.println("Error code: " + status);
            }
          } catch (Exception e) {
            System.err.println("Problem with adding the key/data: " + key + "/" +
                                 value);
              e.printStackTrace();
          }
        }

/*        public void printIntTable() {
                try {
                        Cursor cursor = mDb.openCursor(null, null);
                        DatabaseEntry keyEntry = new DatabaseEntry();
                        DatabaseEntry dataEntry = new DatabaseEntry();
                        while (cursor.getNext(keyEntry, dataEntry, LockMode.DEFAULT) == OperationStatus.SUCCESS) {
                                String key = new String(keyEntry.getData());
                                System.out.println(key + "|" + IntegerBinding.entryToInt(dataEntry));
                        }
                        cursor.close();
                } catch (Exception e) {
                        System.out.println("WARNING: Error printing int table " + e);
                        e.printStackTrace();
                        //logger.log(Level.WARNING, "Error inserting count ", e);
                }
        }

        public void printDoubleTable() {
                try {
                        Cursor cursor = mDb.openCursor(null, null);
                        DatabaseEntry keyEntry = new DatabaseEntry();
                        DatabaseEntry dataEntry = new DatabaseEntry();
                        while (cursor.getNext(keyEntry, dataEntry, LockMode.DEFAULT) == OperationStatus.SUCCESS) {
                                String key = new String(keyEntry.getData());
                                System.out.println(key + "|" + DoubleBinding.entryToDouble(dataEntry));
                        }
                        cursor.close();
                } catch (Exception e) {
                        System.out.println("WARNING: Error printing double table " + e);
                        e.printStackTrace();
                        //logger.log(Level.WARNING, "Error inserting count ", e);
                }
        }
*/
        public void printStringTable() {
          try {
            Cursor dbcp = mDb.openCursor(null,null);
            StringDbt key = new StringDbt();
            StringDbt data = new StringDbt();
            while (dbcp.getNext(key, data, null) == OperationStatus.SUCCESS)
              System.out.println("TABLE:"+ key.getString() + " : " + data.getString());
            dbcp.close();
          } catch (Exception e) {
            System.err.println("Cursor error");
            e.printStackTrace();
          }

        }

}
