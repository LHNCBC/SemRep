
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

import java.net.Socket;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

/**
 * This class represents an object that consists of a Socket object,  and BufferedReader /
 * PrintWriter objects associated with the Socket. A SocketResource is used by a
 * SocketResourcePool, where a certain number of sockets (and associated reader/writer
 * objects) are kept. The objects that need a socket pulls a SocketResource object
 * from the pool, uses it and releases it back to the pool.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */


public class SocketResource
{
  /** A Socket */
  private Socket fSocket;
  /** The PrintWriter associated with the Socket */
  private PrintWriter fWriter;
  /** The BufferedReader associated with the Socket */
  private BufferedReader fReader;

  /**
   * Constructor. Creates a Socket object and its associated Reader/Writer objects.
   *
   * @param host the name of the host the SocketResource will open to
   * @param port the port number on the host
   *
   * @throws IOException if Reader/Writer objects cannot created.
   */
  public SocketResource(String host, int port) throws IOException
  {
      fSocket = new Socket(host,port);
      fWriter = new PrintWriter(fSocket.getOutputStream(), true);
      fReader = new BufferedReader(new InputStreamReader(fSocket.getInputStream()));
  }

  /**
   * get() method for the Socket.
   *
   * @return fSocket.
   */
  public Socket getSocket()
  {
      return fSocket;
  }

  /**
   * get() method for the PrintWriter.
   *
   * @return fWriter.
   */
  public PrintWriter getWriter()
  {
      return fWriter;
  }

  /**
   * get() method for BufferedReader.
   *
   * @return fReader.
   */
  public BufferedReader getReader()
  {
      return fReader;
  }

  /**
   * Checks whether a SocketResource is usable for socket operations.
   * A SocketResource object is usable if the Socket is not closed, and the Reader/Writer
   * objects are alive.
   *
   * @return true/false depending on whether the SocketResource is usable or not.
   */
  public boolean isSocketResourceOK()
  {
      if (fSocket.isClosed() || fSocket.isInputShutdown() || fSocket.isOutputShutdown())
          return false;
      return true;
  }

}