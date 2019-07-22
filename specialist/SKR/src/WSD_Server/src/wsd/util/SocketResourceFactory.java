
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

/**
 * This class represents a Singleton object that creates sockets and associated
 * PrintWriter and BufferedReader objects upon request. It is started up during
 * application initialization.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class SocketResourceFactory {

  /** The only instance of the Factory object */
  private static SocketResourceFactory fSocketResourceFactory;

  /** Constructor. No parameter. */
  private SocketResourceFactory()
  {
  }

  /**
   * Creates an instance of the Factory object if it doesn't exist already.
   *
   * @return the SocketResourceFactory instance.
   */
  public static SocketResourceFactory getInstance()
  {
      if (fSocketResourceFactory == null)
      {
         fSocketResourceFactory = new SocketResourceFactory();
      }
      return fSocketResourceFactory;
  }

  /**
   * Creates a new SocketResource object.
   *
   * @param host the name of the host the SocketResource will open to
   * @param port the port number on the host
   *
   * @throws IOException if Reader/Writer objects cannot created.
   */
  public SocketResource newSocketResource(String hostname, int port) throws IOException
  {
      return new SocketResource(hostname, port);
  }
}