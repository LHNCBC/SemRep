
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

import java.io.IOException;

import java.util.Iterator;
import java.util.Vector;

import org.apache.log4j.Logger;

/**
 * This class represents a Singleton object that handles initialization, allocation
 * and cleanup of a number of SocketResource objects that handle communication with
 * JDI Server. It is started up during application initialization.
 * <p>  The SocketResourcePool can be generalized to support connection to multiple
 * servers using a factory method with a singleton collection of SocketResourcePools.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class SocketResourcePool
{
  /** The Singleton instance */
  private static SocketResourcePool fSocketResourcePool = null; // TODO: this should probably be a synchronized map to allow multiple pools, one for each resource.
  /** The SocketResourceFactory instance that creates the SocketResource's */
  private static SocketResourceFactory fSocketResourceFactory = null;
  /** The SocketResource objects that are available */
  private Vector freeSocketResources = null;
  /** the SocketResource objects that are currently being used */
  private Vector usedSocketResources = null;
  /** # of SocketResource's in the pool */
  private int POOL_SIZE = 20; //  WSDEnvironment.fSocketResourcePoolSize;
  /** # of broken SocketResources */
  private int brokenSockets = 0;

  private static Logger logger = Logger.getLogger(SocketResourcePool.class);

  /** hostname for this resource */
  private String hostname;
  /** port number for this resource */
  private int port;

  /**
   * Constructor. Initializes the Pool.
   */
  private SocketResourcePool(String hostname, int port, int poolSize)
  {
      this.POOL_SIZE = poolSize;
      this.hostname = hostname;
      this.port = port;
      init();
  }

  /**
   * If a Pool doesn't exist, creates one. Otherwise, returns the existing instance.
   *
   * @return SocketResourcePool instance.
   */
  public static synchronized SocketResourcePool getInstance(String hostname, int port, int poolSize)
  {
      if (fSocketResourcePool == null)
      {
	fSocketResourcePool = new SocketResourcePool(hostname, port, poolSize);
      }
      return fSocketResourcePool;
  }

   /**
    * Creates a number of SocketResource's and places them in freeSocketResources list.
    */
  private void init()
  {
      int i=0;
      fSocketResourceFactory = SocketResourceFactory.getInstance();
      freeSocketResources = new Vector(POOL_SIZE);
      usedSocketResources = new Vector(POOL_SIZE);
      for (i=0; i< POOL_SIZE; i++)
      {
	freeSocketResources.add(createSocketResource());
      }
      logger.info("Created " + POOL_SIZE + " JDI Sockets.");
  }


  /**
   * Creates a SocketResource to JD Server.
   *
   * @return a SocketResource to JD Server.
   */
  private SocketResource createSocketResource()
  {
      try
      {
          // SocketResource socketResource =
          //     fSocketResourceFactory.newSocketResource(WSDEnvironment.fJdServerHostName,
          //                                              WSDEnvironment.fJdServerPort);
          SocketResource socketResource =
	    fSocketResourceFactory.newSocketResource(this.hostname, this.port);
          return socketResource;
      }
      catch (IOException ioe)
      {
          logger.error(ioe.getMessage());
          return null;
      }
  }

  /**
   * Checks to find a SocketResource that is available for use. If none of them
   * are available, waits for one to become available.
   *
   * @return a SocketResource that is available and working.
   */
  public synchronized SocketResource getSocketResource()
  {
      SocketResource socketResource = null;
      boolean broken = true;
      if (freeSocketResources.size() > 0)
      {
          if (logger.isDebugEnabled())
          {
              logger.debug("Getting socket resource: Number of available sockets: " + freeSocketResources.size());
              logger.debug("Getting socket resource: Number of used sockets: " + usedSocketResources.size());
          }
          while (broken)
          {
              socketResource = (SocketResource)freeSocketResources.remove(0);
              if (!socketResource.isSocketResourceOK())
                  releaseBrokenSocketResource(socketResource);
              else
                  broken = false;
          }
          usedSocketResources.add(socketResource);
          if (logger.isDebugEnabled())
          {
              logger.debug("Got socket resource: Number of free sockets: " + freeSocketResources.size());
              logger.debug("Got socket resource: Number of used sockets: " + usedSocketResources.size());
          }
          return socketResource;
      }
      else
      {
          try
          {
              wait();
          }
          catch (InterruptedException ie)
          {
              logger.error("Cannot fetch a socket." + ie.getMessage());
          }
           return getSocketResource();
      }
  }

  /**
   * Releases a used SocketResource and returns it to freeSocketResources list.
   * Notifies the processes that are waiting for a SocketResource to become
   * available.
   *
   * @param socketResource  the SocketResource object to be released.
   */
  public synchronized void releaseSocketResource(SocketResource socketResource)
  {
      int index = usedSocketResources.indexOf(socketResource);
      if (index != -1) {
          freeSocketResources.add(usedSocketResources.remove(index));
      }
      if (logger.isDebugEnabled())
      {
          logger.debug("Release: Number of free sockets: " + freeSocketResources.size());
          logger.debug("Release: Number of used sockets: " + usedSocketResources.size());
      }
      notifyAll();
  }

  /**
   * Releases a broken SocketResource object. Removes the SocketResource and
   * creates a new one, which is then added to freeSocketResources list.
   *
   * @param socketResource  a broken SocketResource to be released.
   */
  public synchronized void releaseBrokenSocketResource(SocketResource socketResource)
  {
    try
    {
          brokenSockets++;
          if (logger.isDebugEnabled())
              logger.debug("# of broken sockets: " + brokenSockets);
          socketResource.getWriter().flush();
          socketResource.getWriter().close();
          socketResource.getReader().close();
          socketResource.getSocket().close();
          usedSocketResources.remove(usedSocketResources.indexOf(socketResource));
          SocketResource newSocketResource = createSocketResource();
          freeSocketResources.add(newSocketResource);
          brokenSockets--;
    }
    catch (IOException ioe)
    {
        logger.error("Problem with releasing broken socket. " + ioe.getMessage());
    }
  }

  /**
   * Removes all existing SocketResource objects.
   *
   * @throws IOException when one of the close() or flush() operations fails.
   */
  public synchronized void shutdown() throws IOException
  {
      Iterator iterator = freeSocketResources.iterator();
      while (iterator.hasNext())
      {
          SocketResource socketResource = (SocketResource)iterator.next();
          socketResource.getWriter().flush();
          socketResource.getWriter().close();
          socketResource.getReader().close();
          socketResource.getSocket().close();
      }
      iterator = usedSocketResources.iterator();
      while (iterator.hasNext())
      {
          SocketResource socketResource = (SocketResource)iterator.next();
          socketResource.getWriter().flush();
          socketResource.getWriter().close();
          socketResource.getReader().close();
          socketResource.getSocket().close();
      }

  }

  /**
   * Reports free SocketResource object count in the Pool.
   *
   * @return the # of free SocketResource's.
   */
  public int getFreeSocketResourceCount()
  {
      return freeSocketResources.size();
  }

  /**
   * Reportns currently used SocketResource object count in the Pool.
   *
   * @return the # of currently used SocketResource's.
   */
  public int getUsedSocketResourceCount()
  {
      return usedSocketResources.size();
  }

  /**
   * Reports # of broken SocketResources.
   *
   * @return the # of broken SocketResource's.
   */
  public int getBrokenSocketResourceCount()
  {
      return brokenSockets;
  }
}
