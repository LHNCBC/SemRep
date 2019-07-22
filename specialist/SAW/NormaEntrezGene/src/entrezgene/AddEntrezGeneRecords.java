package entrezgene;

import java.io.*;
import com.sleepycat.db.*;

public class AddEntrezGeneRecords {
  private static String envHomeDirectory;
  private Environment dbenv = null;

  public AddEntrezGeneRecords() {
    try {
        EnvironmentConfig config = new EnvironmentConfig();
        File envHomeDir = new File(envHomeDirectory);
        config.setErrorStream(System.err);
        config.setErrorPrefix("addEntrezGeneRecords");
        config.setCacheSize(64 * 1024);
        config.addDataDir(envHomeDir);
        config.setAllowCreate(true);
        config.setInitializeCache(true);
        config.setTransactional(true);
        config.setInitializeLocking(true);

        dbenv = new Environment(envHomeDir, config);
        System.out.println("DB ENV:" + dbenv.getConfig().toString());
    } catch (Exception e) {
      System.err.println("Error creating environment");
      e.printStackTrace();
    }
  }

  public void close()
  {
    try {
	if (dbenv != null) 
	    dbenv.close();
    } catch (Exception e) {
      System.err.println("Error closing environment");
      e.printStackTrace();
    }
  }

  // create the symbol and alias tables.
  public void createSymbolTable(String file, String dbName)
  {
    String inputLine = new String();
    String id = new String();
    String value = new String();

    try {
      BufferedReader in = new BufferedReader(new FileReader(file));
      System.out.println("Opened file " + file + " for reading.");
      DBTable table = new DBTable(dbenv,dbName,true);
      while ( (inputLine = in.readLine()) != null)
      {
        System.out.println("Line: " + inputLine);
        id = inputLine.substring(0,inputLine.indexOf("|"));
        value = inputLine.substring(inputLine.indexOf("|")+1);
	if (id != null && !(id.equals(""))) {
	    System.out.println("ID: " + id + "^^" + value);
	    table.putString(id,value);
	}
      }
      //table.printStringTable();
      table.close();
      System.out.println("Finished creating " + dbName);
    }
    catch (Exception e) {
      e.printStackTrace();
      e.getMessage();
    }
  }

  public void createAliasTable(String file, String dbName)
  {
    String inputLine = new String();
    String id = new String();
    String value = new String();

    try {
      BufferedReader in = new BufferedReader(new FileReader(file));
      DBTable table = new DBTable(dbenv,dbName,true);
      while ( (inputLine = in.readLine()) != null)
      {
        System.out.println("Line: " + inputLine);
        id = inputLine.substring(0,inputLine.indexOf("|")).toUpperCase();
        value = inputLine.substring(inputLine.indexOf("|")+1);
	if (id != null && !(id.equals(""))) {
	    System.out.println("ID: " + id + "^^" + value);
	    String tmp = table.getString(id);
	    if (tmp.equals(""))
		table.putString(id,value);
	    else
	    {
		System.out.println("In db: " + tmp);
		if (!(tmp.indexOf("|" + value) > 0)) {
		    tmp += "|" + value;
		    table.putString(id,tmp);
		}
	    }
	}
      }
      //table.printStringTable();
      table.close();
      System.out.println("Finished creating " + dbName);
    }
    catch (Exception e) {
      e.printStackTrace();
      e.getMessage();
    }
  }


  public static String getOption(char flag, String[] args)
  {
    for (int i = 0; i < args.length; i++) {
            if ((args[i].length() > 0) && (args[i].charAt(0) == '-')) {
                    if (args[i].charAt(1) == flag) {
                            if (i + 1 == args.length)
                                    return "";
                            return args[i + 1];
                    }
            }
    }
    return "";

  }
  public static void main(String[] args) {

    long start = System.currentTimeMillis();
    if (args.length < 10) {
      System.out.println("Arguments: -e [Environment Dir]");
      System.out.println("           -s [EntrezGene Symbol File Name]");
      System.out.println("           -a [EntrezGene Alias File Name]");
      System.out.println("           -t [Symbol DB Name]");
      System.out.println("           -b [Alias DB Name]");
      System.exit(0);
    }

    envHomeDirectory = getOption('e',args);
    String symbolFile = getOption('s',args);
    String aliasFile = getOption('a',args);
    String symbolDBName = getOption('t',args);
    String aliasDBName = getOption('b',args);

    AddEntrezGeneRecords op = new AddEntrezGeneRecords();
    op.createSymbolTable(symbolFile, symbolDBName);
    op.createAliasTable(aliasFile, aliasDBName);

    long end = System.currentTimeMillis();
    long duration = end - start;
    System.out.println("Processing took " + duration + " miliseconds.");
  }

}
