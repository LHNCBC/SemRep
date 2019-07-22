
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

package wsd;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import com.sleepycat.db.Db;
import com.sleepycat.db.DbException;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

import wsd.util.SocketResourcePool;

/**
 * This class represents the environment variables that will be used during the
 * disambiguation process. When the WSD Server is initialized, this
 * class is called and variables are loaded.
 * Berkeley DB tables and socket pool are initialized, as well.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class WSDEnvironment
{

  /** the server configuration file, contains the environment
      variables, default can be overridden using system property
      "server.config.file" */
  public static final String SERVER_CONFIG_FILE =
    System.getProperty("server.config.file", "/nfsvol/nls/config/disambServer.cfg");

  /** global properties object for use by disambiguation methods */
  public static Properties properties;
  /** the TCP/IP port number */
  public static int fPort;
  /** the preferred name field separator */
  public static String fPreferredNameSeparator;
  /** A list of methodname -> classname pairs of available disambiguation methods */
  public static String fMethodsConfigFile;
  /** A list of methodname -> description pairs of available disambiguation methods */
  public static String fMethodDescriptionFile;
  /** the available disambiguation methods */
  public static Map fAvailableMethods = new HashMap(10);
  /** the disambiguation method descriptions */
  public static Map fMethodDescriptions = new HashMap(10);
  /** The log4j configuration file */
  public static String fLog4jConfigFile;
  // /** pool of socket resources for the JDI server. */
  // public static SocketResourcePool fSocketResourcePool;
  // /** size of the socket resource pool*/
  // public static int fSocketResourcePoolSize;

  /** Berkeley DB pointer to reviewed answer table used by Simple Frequency Method */
  public static Db fDbAnswers = null;
  /** Berkeley DB pointer to concept variant counts table used by MeSH Frequency Method */
  public static Db fDbVariantCounts = null;
  /** Berkeley DB pointer to concept variants table used by MeSH Frequency Method */
  public static Db fDbVariants = null;

  // /** the host where JD Indexing Method runs */
  // public static String fJdServerHostName;
  // /** the port number to access to JD Indexing Method */
  // public static int fJdServerPort;
  // /** the result type for JD Indexing */
  // public static String fJdServerResultType;


  /** the reviewed answers table used by Simple Frequency Method */
  public static String fReviewedAnswerDBFile;
  /** the MeSH variant count db file */
  public static String fMeSHVariantCountDBFile;
  /** the MeSH variants db file */
  public static String fMeSHVariantsDBFile;
  /** logger for WSDEnvironment class */
  private static Logger logger = null;

  /**
   * Reads the server configuration file, and initializes the
   * environment. Called once during the Disambiguation Server initialization.
   */
  public static void initialize()
  {
      try
      {
	// properties are now loaded into a global properties object. WSDEnvironment.properties
          //read the server configuration file
	  System.out.println("loading properties file " + SERVER_CONFIG_FILE);
          InputStream in = new FileInputStream(SERVER_CONFIG_FILE);
          properties = new Properties();
          properties.load(in);

          fPort = Integer.parseInt(properties.getProperty("DISAMB_SERVER_TCP_PORT"));
          fReviewedAnswerDBFile = properties.getProperty("DISAMB_SERVER_FREQ_DB_FILE");// frequency method
          fMeSHVariantCountDBFile = properties.getProperty("DISAMB_SERVER_VARIANT_COUNT_DB_FILE"); // MeSH Freq Method
          fMeSHVariantsDBFile = properties.getProperty("DISAMB_SERVER_VARIANTS_DB_FILE"); // MeSH Freq Method
          // fJdServerHostName = properties.getProperty("JD_SERVER_HOSTNAME"); // JDI Method
          // fJdServerPort = Integer.parseInt(properties.getProperty("JD_SERVER_PORT")); // JDI Method
          // fJdServerResultType = properties.getProperty("JD_SERVER_RESULTTYPE");// JDI Method
          // fSocketResourcePoolSize = Integer.parseInt(properties.getProperty("JDI_SOCKET_POOL_SIZE"));
          fMethodsConfigFile = properties.getProperty("METHODS_CONFIG_FILE", "methods.cfg");
          fMethodDescriptionFile = properties.getProperty("METHODS_DESCRIPTION_FILE", "methodsdesc.properties");
          fLog4jConfigFile = properties.getProperty("LOG4J_CONFIG_FILE","log4j.properties");
          fPreferredNameSeparator = properties.getProperty("PREFERRED_NAME_SEPARATOR");

          //initialize log4j
          PropertyConfigurator.configure(fLog4jConfigFile);
          logger= Logger.getLogger(WSDEnvironment.class);

          // //initialize socket pool
          // fSocketResourcePool = SocketResourcePool.getInstance();

          // load disambiguation method properties
          loadMethodProperties();
          loadDescriptionProperties();
          logger.info("WSD Server settings are loaded.");

          // loadReviewedAnswersDB();
          // loadVariantCountsDB();
          // loadVariantsDB();
	  System.out.println("WSD Server initializing disambiguation methods.");
	  System.out.flush();
	  logger.info("WSD Server initializing disambiguation methods.");
          initializeDisambiguationMethods();
          logger.info("WSD Server databases and disambiguation methods have been initialized.");
	  System.out.println("WSD Server databases and disambiguation methods have been initialized.");
	  System.out.flush();
        }
      catch (java.net.ConnectException exception)
	{
            logger.error("Error establishing socket pool" + exception.getMessage());
        }
        catch (FileNotFoundException fnfe)
        {
            System.err.print(fnfe.getMessage());
            throw new RuntimeException("WSD Server settings cannot be loaded.");
        }
        catch (IOException ioe)
        {
            System.err.print(ioe.getMessage());
            throw new RuntimeException("WSD Server settings cannot be loaded.");
        }
    }


    /**
     * Loads the available disambiguation methods and the classnames of the
     * methods from the configuration file.
     */
    private static void loadMethodProperties()
    {
	// load properties file containing a list of disambiguation methods
	// and place classnames of methods in available methods map.
	try {
	    InputStream in = new FileInputStream(fMethodsConfigFile);
	    Properties props = new Properties();
	    props.load(in);
	    Iterator keyIterator = props.keySet().iterator();
	    while (keyIterator.hasNext()) {
		String methodName = (String)keyIterator.next();
		String className = (String)props.get(methodName);
		try {
		    Class dmethodClass = Class.forName(className);
		    fAvailableMethods.put(methodName, className);

		} catch (ClassNotFoundException exception) {
                    logger.warn("Disambiguation class, " + className +
                                ", not found. Ignoring class");
		}
	    }
	} catch (Exception exception) {
            logger.fatal("Exception occurred while opening file " +
                         WSDEnvironment.fMethodsConfigFile);
	    throw new RuntimeException
		("Exception occurred while opening file " +
		 WSDEnvironment.fMethodsConfigFile,
		 exception);
	}
    }



    /**
     * Loads the disambiguation method descriptions from the configuration file.
     */
    private static void loadDescriptionProperties()
    {
	// load properties file containing a list of disambiguation methods
	// descriptions
	try {
	    InputStream in = new FileInputStream(fMethodDescriptionFile);
	    Properties props = new Properties();
	    props.load(in);
	    Iterator keyIterator = props.keySet().iterator();
	    while (keyIterator.hasNext()) {
		String methodName = (String)keyIterator.next();
		String description = (String)props.get(methodName);
		fMethodDescriptions.put(methodName, description);
	    }
	} catch (java.io.FileNotFoundException exception) {
            logger.warn("Method Description file not found: " +
                        WSDEnvironment.fMethodDescriptionFile);
	} catch (Exception exception) {
            logger.fatal("Exception occurred while opening file " +
                          WSDEnvironment.fMethodDescriptionFile);
	    throw new RuntimeException
		("Exception occurred while opening file " +
		 WSDEnvironment.fMethodDescriptionFile,
		 exception);
	}
    }

    /**
     * Loads the reviewed answers table used by Simple Frequency Method.
     */
    private static void loadReviewedAnswersDB()
    {
        try
        {
            fDbAnswers = new Db(null, 0);
            fDbAnswers.set_error_stream(System.err);
            fDbAnswers.set_errpfx("WSDEnvironment:loadReviewedAnswerDB");
            fDbAnswers.set_pagesize(1024);			// 1K page sizes.
//          fDbAnswers.set_flags(Db.DB_DUP);
            fDbAnswers.open(null,fReviewedAnswerDBFile, null, Db.DB_HASH, Db.DB_RDONLY, 0444);
        }
        catch (FileNotFoundException fnfe)
        {
            logger.error(fnfe.getMessage());
        }
        catch (DbException dbe)
        {
            logger.error(dbe.getMessage());
        }
    }

    /**
     * Loads the MeSH variants table used by MeSH Frequency Method.
     */
    private static void loadVariantsDB()
    {
        try
        {
            fDbVariants = new Db(null, 0);
            fDbVariants.set_error_stream(System.err);
            fDbVariants.set_errpfx("WSDEnvironment:loadVariantsDB");
            fDbVariants.set_pagesize(1024);			// 1K page sizes.
//          fDbVariants.set_flags(Db.DB_DUP);
            fDbVariants.open(null,fMeSHVariantsDBFile, null, Db.DB_BTREE, Db.DB_RDONLY, 0444);
        }
        catch (FileNotFoundException fnfe)
        {
            logger.error(fnfe.getMessage());
        }
        catch (DbException dbe)
        {
            logger.error(dbe.getMessage());
        }
    }

    /**
     * Loads the MeSH variant counts table used by MeSH Frequency Method.
     */
    private static void loadVariantCountsDB()
    {
        try
        {
            fDbVariantCounts = new Db(null, 0);
            fDbVariantCounts.set_error_stream(System.err);
            fDbVariantCounts.set_errpfx("WSDEnvironment:loadVariantCountsDB");
            fDbVariantCounts.set_pagesize(1024);			// 1K page sizes.
//          fDbVariantCounts.set_flags(Db.DB_DUP);
            fDbVariantCounts.open(null,fMeSHVariantCountDBFile, null, Db.DB_BTREE, Db.DB_RDONLY, 0444);
        }
        catch (FileNotFoundException fnfe)
        {
            logger.error(fnfe.getMessage());
        }
        catch (DbException dbe)
        {
            logger.error(dbe.getMessage());
        }
    }

    /**
     * Closes the socket pool and open Berkeley DB tables.
     */
    public static void shutdown()
    {
        // shutdownSocketPool();
        shutdownDB();
    }

    // /**
    //  * Closes the socket pool.
    //  */
    // private static void shutdownSocketPool()
    // {
    //   try {
    //         fSocketResourcePool.shutdown();
    //   }
    //   catch (IOException ioe)
    //   {
    //       logger.error("Problem closing the socket pool.");
    //   }

    // }

    /**
     * Shut down all Berkeley DB tables.
     */
    public static void shutdownDB()
    {
      closeReviewedAnswersDB();
      closeVariantsDB();
      closeVariantCountsDB();
    }

    /**
     * Close the reviewed answers table.
     */
    private static void closeReviewedAnswersDB()
    {
        try
        {
          fDbAnswers.close(0);
        }
        catch (DbException dbe)
        {
          logger.error(dbe.getMessage());
        }
    }

    /**
     * Close the MeSH variants table.
     */
    private static void closeVariantsDB()
    {
        try
        {
          fDbVariants.close(0);
        }
        catch (DbException dbe)
        {
            logger.error(dbe.getMessage());
        }
    }

    /**
     * Close the MeSH variant count table.
     */
    private static void closeVariantCountsDB()
    {
        try
        {
          fDbVariantCounts.close(0);
        }
        catch (DbException dbe)
        {
            logger.error(dbe.getMessage());
       }
    }

  /**
   * Instantiate any classes in the available methods list to invoke
   * any static initializers.
   */
  private static void initializeDisambiguationMethods()
    {
      try {
	Iterator methodClassIterator = fAvailableMethods.values().iterator();
	while (methodClassIterator.hasNext()) {
	  String className = (String)methodClassIterator.next(); 
          // instantiate the class to invoke any static initializers.
          Class dmethodClass = Class.forName(className);
	}
      } catch (ClassNotFoundException e) {
	e.printStackTrace(System.err);
      }
    }

    /**
     * In case, a database cannot be accessed, reinitialize the tables.
     */
    public static void reinitializeDB()
    {
       if (fDbAnswers == null)
         loadReviewedAnswersDB();
       if (fDbVariantCounts == null)
         loadVariantCountsDB();
       if (fDbVariants == null)
         loadVariantsDB();
    }
}
