
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

package wsd.server;

import java.io.IOException;
import java.net.ServerSocket;

import wsd.WSDEnvironment;

/**
 * The main WSD Server class. Receives disambiguation requests from the client
 * over a socket, creates a thread to handle the request and returns the results
 * back to the client.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class DisambiguatorServer
{
  /** whether the WSD server is currently listening */
  private boolean fListening = true;
  /** the port on which the WSD Server is listening */
  private int fPort;
  /** the server socket associated with the WSD Server */
  private ServerSocket fServerSocket = null;

  /**
   * The WSD Server constructor. Creates a server process on the port specified.
   *
   * @param port the server port.
   */
  private DisambiguatorServer(int port)
  {
      fPort = port;
      try
      {
          fServerSocket = new ServerSocket(fPort);
      }
      catch (IOException ioe)
      {
	  System.out.println("Could not listen on port : " + fPort + " : " + ioe.getMessage() );
	  fListening = false;
          System.exit(1);
      }
  }

  /**
   * The main() method for the WSD Server. Initializes the WSD Environment,
   * creates a server process and waits for the incoming disambiguation requests.
   */
  public static void main(String[] args)
  {
      // the server program takes no argument
      if (args.length > 0)
      {
          usage();
      }
      else
      {
          try
          {
              WSDEnvironment.initialize();
              DisambiguatorServer server = new DisambiguatorServer(WSDEnvironment.fPort);
              while (server.fListening)
                  new DisambiguatorServerThread(server.fServerSocket.accept()).start();
              server.fServerSocket.close();
              WSDEnvironment.shutdown();

          }
          catch (IOException ioe)
          {
              System.err.print("DisambiguatorServer->main()::IOException: " + ioe.getMessage());
          }
          catch (Exception e)
          {
              System.err.println("Server cannot be started.");
	      e.printStackTrace(System.err);
          }
      }
      System.exit(0);
  }

  /**
   * If the program is run with wrong options etc., this method is called.
   */
  private static void usage()
  {
      System.out.println("Usage: java wsd.DisambiguatorServer");
      System.out.println("Cannot start the WSD Server");
  }
}

