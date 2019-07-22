
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

package wsd.methods;

import java.util.*;
import java.io.*;
import java.net.*;

import org.apache.log4j.Logger;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.Namespace;

import wsd.model.*;
import wsd.util.SocketBrokenException;
import wsd.util.SocketResourcePool;
import wsd.util.SocketResource;
import wsd.WSDEnvironment;

/**
 * Journal Descriptor Based Disambiguation Method.
 * <p>
 * Available Properties in CONFIG FILE:
 *<dl>
 * <dt>JD_SERVER_HOSTNAME   <dd>hostname of server</dd>
 * <dt>JD_SERVER_PORT       <dd>port of server</dd>
 * <dt>JD_SERVER_RESULTTYPE <dd>set to "word" to get result based on word count</dd>
 *</dl>
 *</p>
 * Created: Fri May 31 10:26:32 2002
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: JdDisambiguator.java,v 1.9 2006/05/17 13:21:42 wrogers Exp $
 */

public class JdDisambiguator implements DisambiguationMethod {

  /** pool of socket resources for the JDI server. */
  public static SocketResourcePool fSocketResourcePool;
  /** size of the socket resource pool*/
  public static int SocketResourcePoolSize =
    Integer.parseInt(WSDEnvironment.properties.getProperty("JDI_SOCKET_POOL_SIZE"));

  /** JD disambiguator hostname */
  public static String serverHostname =
    WSDEnvironment.properties.getProperty("JD_SERVER_HOSTNAME");
  /** JD disambiguator port number */
  public static int serverPort =
    Integer.parseInt(WSDEnvironment.properties.getProperty("JD_SERVER_PORT"));
  /** result type -> "word": word count, "cite": citation count */
  public static String resultType =
    WSDEnvironment.properties.getProperty("JD_SERVER_RESULTTYPE");
  /** lisp server socket */
  private Socket socket = null;
  /** lisp server output writer using socket */
  private PrintWriter out = null;
  /** lisp server input reader  using socket */
  private BufferedReader in = null;
  /** are we connected to lisp server */
  protected static boolean connected = false;

  private SocketResource sockRes;

  static {
    //initialize socket pool
    fSocketResourcePool =
      SocketResourcePool.getInstance
      (serverHostname, serverPort, SocketResourcePoolSize);
  }

  /** Logger */
  private static Logger logger = Logger.getLogger(JdDisambiguator.class);

 /**
  * Set result type of disambiguation method: "word": word count, "cite":
  * citation count
  *
  * @param type result type to be used.
  */
  public void setResultType(String type)
  {
    this.resultType = type;
  }

  /**
   * Get current value of disambiguation result type.
   *
   * @return disambiguation result type.
   */
  public String getResultType()
  {
    return this.resultType;
  }

  /**
   * set server hostname
   *
   * @param hostname server hostname
   */
  public void setServerHostname(String hostname)
  {
    this.serverHostname = hostname;
  }

  /**
   * get server port number
   *
   * @return port port number (int)
   */
  public int getServerPort()
  {
    return this.serverPort;
  }

  /**
   * set server port number
   *
   * @param port port number (int)
   */
  public void setServerPort(int port)
  {
    this.serverPort = port;
  }

  /** establish connection with server */
  private void establish() throws IOException, UnknownHostException
  {
      logger.debug("entering establish()");
      sockRes = SocketResourcePool.getInstance
	(serverHostname, serverPort, SocketResourcePoolSize).getSocketResource();
      this.socket = sockRes.getSocket();
      this.out = sockRes.getWriter();
      this.in = sockRes.getReader();
      this.connected = true;
      logger.debug("leaving establish()");
  }

  /** release any resources, close files, sockets, etc. */
  private void release() throws IOException
  {
    logger.debug("entering release()");
    SocketResourcePool.getInstance
      (serverHostname, serverPort, SocketResourcePoolSize).releaseSocketResource(sockRes);
    logger.debug("leaving release()");
  }

  /** release any resources */
  protected void finalize() throws IOException
  {
      SocketResourcePool.getInstance
	(serverHostname, serverPort, SocketResourcePoolSize).releaseSocketResource(sockRes);
  }

  /**
   * get match
   *
   * @param text      target text
   * @param context   vector of target contexts (Context class)
   * @param semTypes  vector of semantic types (Strings)
   * @return vector
   * @deprecated As of version 1.2 of DisambiguatorServerThread.
   */
  public Vector getMatch(String text, Vector context, Vector semTypes)
  {
    /** ignore context for now */
    Vector result = new Vector();
    StringBuffer sbuf = new StringBuffer();
    sbuf.append("|").append(text).append("|");
    sbuf.append(" (");
    Iterator contextIterator = context.iterator();
    while (contextIterator.hasNext()) {
      sbuf.append("\"").append(contextIterator.next()).append("\"").append(" ");
    }
    sbuf.append(") (");
    Iterator semTypesIterator = semTypes.iterator();
    while (semTypesIterator.hasNext()) {
      sbuf.append(semTypesIterator.next()).append(" ");
    }
    sbuf.append(") ");
    if (resultType.equals("word")) {
      sbuf.append("wc");
    } else {
      sbuf.append("dc");
    }
    sbuf.append(")");
    logger.info("sending " + sbuf.toString());
    try {
      if (! this.connected) establish();
      this.out.println(sbuf.toString());
      this.out.flush();
      logger.info("sent message");
      String response = this.in.readLine();
      logger.info("response: " + response);
      if (response.trim().length() > 0)
	result.add(response.replace('"', ' ').trim());
    } catch (Exception exception) {
      exception.printStackTrace(System.err);
      return result;
    }
    return result;
  }

  public List getMatch(Document doc)
  {
      List jdResults = new Vector();
      Element root = doc.getRootElement();
      Namespace ns = root.getNamespace();

      logger.info("Disambiguating using JDI Method.");
      //get the utterance list
      List utteranceList = root.getChildren("utterance", ns);
      ListIterator utteranceIterator = utteranceList.listIterator();
      while (utteranceIterator.hasNext())
      {
          Element utteranceNode = (Element)utteranceIterator.next();
 	  Utterance utterance = new Utterance(utteranceNode,ns);

          //get the noun phrase list
          List phraseList = utteranceNode.getChildren("phrase",ns);
          ListIterator phraseIterator = phraseList.listIterator();
          while (phraseIterator.hasNext())
          {
              Element phraseNode = (Element)phraseIterator.next();
              NounPhrase nounPhrase = new NounPhrase(phraseNode,ns);
              if (phraseNode.hasChildren())
              {
                  //get the ambiguity list
                  Element ambiguitiesNode = phraseNode.getChild("ambiguities",ns);
                  List ambiguityList = ambiguitiesNode.getChildren("ambiguity",ns);
                  ListIterator ambiguityIterator = ambiguityList.listIterator();
                  while (ambiguityIterator.hasNext())
                  {
                      Element ambiguityNode = (Element)ambiguityIterator.next();
                      Ambiguity ambiguity = new Ambiguity(ambiguityNode,ns);
                      //if the ambiguity is marked to be "process"ed, process it
                      //otherwise skip.
                      if (ambiguity.getNeedProcessing())
                      {
                          List candidateList = ambiguityNode.getChildren("candidate",ns);
                          ListIterator candidateIterator = candidateList.listIterator();
                          PreferredNameVector prefNames = new PreferredNameVector();
                          List candidates = new Vector();
                          Candidate candidate = new Candidate();
                          while (candidateIterator.hasNext())
                          {
                              Element candidateNode = (Element)candidateIterator.next();
                              candidate = new Candidate(candidateNode,ns);
                              candidates.add(candidate);
                              String preferredName = candidate.getPreferredConceptName();
                              prefNames.add(preferredName);
                          }
                          List context = getContext(doc,utterance,candidate.getMatchedWords());
                          List bestPrefNames = this.getMatch(utterance,null,context,candidates);
                          //create the Result object that stores the ambiguity result data
                          if (bestPrefNames.size() > 0) {
                            if (bestPrefNames.get(0) instanceof String) {
                              if (bestPrefNames.get(0).equals("NIL")) {
                                jdResults = null;
                              } else if (bestPrefNames.get(0).equals("[Error JDI inputstring is empty or null]")) {
                                jdResults = null;
                              } else if (((String)bestPrefNames.get(0)).startsWith("[Error JDI condition:")) {
                                Result res = new Result();
                                res.setCandidatePreferredConceptNames(prefNames);
                                res.setPreferredConceptNames(bestPrefNames);
                                res.setUi(utterance.getUi());
                                res.setUtterancePos(utterance.getPos());
                                res.setPhrasePos(nounPhrase.getPos());
                                jdResults.add(res);
                              } else if (((String)bestPrefNames.get(0)).equals("[Warning: JDI unable to disambiguate input]")) {
                                jdResults = null;
                              } else {
                                Result res = new Result();
                                res.setCandidatePreferredConceptNames(prefNames);
                                res.setPreferredConceptNames(bestPrefNames);
                                res.setUi(utterance.getUi());
                                res.setUtterancePos(utterance.getPos());
                                res.setPhrasePos(nounPhrase.getPos());
                                jdResults.add(res);
                                if (logger.isDebugEnabled())
                                  logger.debug("Result: " + res.getUi() + "|" +
                                               res.getUtterancePos() + "|" +
                                               res.getPhrasePos() + "|" +
                                               res.getCandidatePreferredConceptNames() + "|" +
                                               res.getPreferredConceptNames());
                              }
                            } else {
                              Result res = new Result();
                              res.setCandidatePreferredConceptNames(prefNames);
                              res.setPreferredConceptNames(bestPrefNames);
                              res.setUi(utterance.getUi());
                              res.setUtterancePos(utterance.getPos());
                              res.setPhrasePos(nounPhrase.getPos());
                              jdResults.add(res);
                              if (logger.isDebugEnabled())
                                logger.debug("Result: " + res.getUi() + "|" +
                                             res.getUtterancePos() + "|" +
                                             res.getPhrasePos() + "|" +
                                             res.getCandidatePreferredConceptNames() + "|" +
                                             res.getPreferredConceptNames());
                            }
                          }
                      }
                  }
              }
          }
      }
      logger.info("Completed disambiguation using JDI Method.");
      if (logger.isDebugEnabled())
	{
	  logger.debug("Free socket resources: " + SocketResourcePool.getInstance
		       (serverHostname, serverPort, SocketResourcePoolSize).getFreeSocketResourceCount());
	  logger.debug("Used socket resources: " + SocketResourcePool.getInstance
		       (serverHostname, serverPort, SocketResourcePoolSize).getUsedSocketResourceCount());
	  logger.debug("Broken socket resources: " + SocketResourcePool.getInstance
		       (serverHostname, serverPort, SocketResourcePoolSize).getBrokenSocketResourceCount());
	}
      return jdResults;
  }

  /**
   * Calls the JDI Method and finds the best match in the list of concepts.
   *
   * @see wsd.methods.JdDisambiguator.buildLispSexpMessage
   *
   * @param utterance   The sentence or fragment containing the ambiguity.
   * @param phrases     The phrases in the utterance.
   * @param context     the context of the ambiguity. Initial thought was to have
   *                    several sentences associated with the ambiguity as the context.
   * @param senses      WordSenses of the concepts that cause the ambiguity
   *
   * @return  a Result object that represents the best match from JDI method
   * @see wsd.model.WordSense
   */
  public List getMatch(Utterance utterance, List phrases, List context, List candidates)
  {
    class RequestThread extends Thread {
      PrintWriter out = null;
      String msgbuf = null;

      public RequestThread(PrintWriter outWriter, String message)
      {
        this.out = outWriter;
        this.msgbuf = message;
      } // RequestThread constructor

      public void run() 
      {
        logger.info("sending " + msgbuf);
        this.out.println(msgbuf);       
        this.out.flush();
	logger.info("sent message");
      }
      
    } // JdDisambiguator

      String response = null;
      logger.debug("entering getMatch(Utterance,List,List,List)");

      List resultConcepts = new Vector();
      String resultSemtype = null;

      // logger.debug("proposed message: " + pmsgbuf);
      String msgbuf = buildLispSexpMessage(utterance, phrases, context, candidates);
      // String msgbuf = buildPipedMessage(utterance, phrases, context, candidates);
      try {
	if (this.out == null) {
          logger.debug("this.out: " + this.out);
          establish();
        }

        // attempt at avoiding a dead lock condition with the lisp server.
        RequestThread sendThread = new RequestThread(this.out, msgbuf);
        sendThread.start();
	logger.info("waiting for response...");
	response = this.in.readLine();
	logger.info("response: " + response);
        if (response.equals("NIL")) {
          resultSemtype = null;
        } else if (response.equals("[Error JDI inputstring is empty or null]")) {
          logger.error(response);
          resultSemtype = null;
        } else if (response.startsWith("[Error JDI condition:")) {
          logger.error(response);
          resultSemtype = null;
        } else if (response.startsWith("[JDI unable to disambiguate input]")) {
          logger.error(response);
          resultSemtype = null;
        } else if (response.trim().length() > 0) {
	  resultSemtype = response.replace('"', ' ').trim();
        }
        release();
      } catch (Exception exception) {
	exception.printStackTrace(System.err);
      }
      if (resultSemtype != null)
	{
	  logger.debug("---");
	  boolean done = false;
	  for (int j=0; j < candidates.size() && done == false; j++)
	    {
	      if (((Candidate)candidates.get(j)).getSemTypes().contains(resultSemtype))
		{
		  resultConcepts.add(((Candidate)candidates.get(j)).getPreferredConceptName());
		  logger.debug(utterance.getUi() + 
			      ":, " + resultSemtype + 
			      ", added: " + candidates.get(j) + ", " + done);
		  done = true;	// only add one candidate that matches semantic type, drop the others.
		  break;
		}
	    }
          return resultConcepts;
        } else { 
          /* if JDI returned nil then just copy the original list into result */
          resultConcepts.add(response);
          return  resultConcepts;
        }
  }

  /** 
   * Build message to lisp server as an S-expression.
   *
   * The method builds a lisp message of the form:
   *   (<ui>
   *    <sentence>
   *    (<context>)
   *    ((<matched-words> <umls-concept> <preferred-name> <semtypes>)
   *     (<matched-words> <umls-concept> <preferred-name> <semtypes>))
   *    <resulttype>)
   * resultTypes: wc or dc
   *
   * @param utterance   The sentence or fragment containing the ambiguity.
   * @param phrases     The phrases in the utterance.
   * @param context     the context of the ambiguity. Initial thought was to have
   *                    several sentences associated with the ambiguity as the context.
   * @param senses      WordSenses of the concepts that cause the ambiguity
   *
   * @return  a string containing an s-expresion suitable for read.
   */
  public String buildLispSexpMessage(Utterance utterance, List phrases, List context, List candidates)
  {
      logger.debug("buildLispSexpMessage(Utterance,List,List,List): Processing sentence: " + utterance.getSentence());
      List semTypes = new Vector();
      StringBuffer sbuf = new StringBuffer();

      sbuf.append("(");
      addString(sbuf, utterance.getUi());
      sbuf.append(" ");
      addString(sbuf, utterance.getSentence().replaceAll("\"","\\\\\""));
      sbuf.append(" ");
      addStringList(sbuf, context);
      sbuf.append(" (");
      Iterator candidateIterator = candidates.iterator();
      while (candidateIterator.hasNext()) {
	Candidate candidate = (Candidate)candidateIterator.next();
	sbuf.append("(");
	addString(sbuf, candidate.getMatchedWords()); sbuf.append(" ");
	addString(sbuf, candidate.getUmlsConcept()); sbuf.append(" ");
	addString(sbuf, candidate.getPreferredConceptName()); sbuf.append(" ");
        List candidateSemTypes = candidate.getSemTypes();
        for (int i=0; i < candidateSemTypes.size(); i++)
        {
          String semTypeAbbrev = (String)candidateSemTypes.get(i);
          semTypes.add(semTypeAbbrev);
          sbuf.append(semTypeAbbrev);
          if (i < candidateSemTypes.size() -1)
          {
            sbuf.append(" ");
          }
        }
	  sbuf.append(") ");
      }
      sbuf.append(") ");
      if (resultType.equals("word")) {
	sbuf.append("wc");
      } else {
	sbuf.append("dc");
      }
      sbuf.append(")");
      return sbuf.toString();
   }

  void addString(StringBuffer sbuf, String aString)
  {
    sbuf.append("\"").append(aString).append("\"");
  }

  void addStringList(StringBuffer sbuf, List stringList)
  {
    sbuf.append("(");
    Iterator iterator = stringList.iterator();
    while (iterator.hasNext()) {
      sbuf.append("\"").append(iterator.next()).append("\" ");
    }
    sbuf.append(")");
  }

  void addAtomList(StringBuffer sbuf, List atomList)
  {
    sbuf.append("(");
    Iterator iterator = atomList.iterator();
    while (iterator.hasNext()) {
      sbuf.append(iterator.next()).append(" ");
    }
    sbuf.append(")");
  }


  /** 
   * Build message to lisp server as an piped list.
   *
   * Representation in BNF:
   *
   * message    -> ui "|" sentence "|" context "|" candidates | resulttype;
   * candidates -> num-of-candidates "|" candidate ["|" candidate]
   * candidate  -> matched-words "|" umls-concept "|" preferred-name "|" semtypes "|"
   * semtypes   -> num-of-semtypes "|" semtype ["|" semtype]
   * result-type -> "wc" | "dc"
   *
   * A looser representation:
   *
   *   <ui>|<sentence>|<context>|
   *    <num-of-candidates>|
   *      <matched-words>|<umls-concept>|<preferred-name>|<number-of-semtypes>|<semtype>...|
   *      ...
   *      <result-type>
   *
   * Result types: wc or dc
   *
   * @param utterance   The sentence or fragment containing the ambiguity.
   * @param phrases     The phrases in the utterance.
   * @param context     the context of the ambiguity. Initial thought was to have
   *                    several sentences associated with the ambiguity as the context.
   * @param senses      WordSenses of the concepts that cause the ambiguity
   *
   * @return  a string containing an piped list.
   */
  public String buildPipedMessage(Utterance utterance, List phrases, List context, List candidates)
  {
      logger.debug("buildPipedMessage(Utterance,List,List,List): Processing sentence: " + utterance.getSentence());
      List semTypes = new Vector();

      StringBuffer sbuf = new StringBuffer();
      sbuf.append(utterance.getUi()).append("|");
      sbuf.append(utterance.getSentence().replaceAll("\"","")).append("|");
      sbuf.append(context).append("|");
      sbuf.append(candidates.size()).append("|");
      Iterator candidateIterator = candidates.iterator();
      while (candidateIterator.hasNext()) {
	Candidate candidate = (Candidate)candidateIterator.next();
	sbuf.append(candidate.getMatchedWords()).append("|");
	sbuf.append(candidate.getUmlsConcept()).append("|");
	sbuf.append(candidate.getPreferredConceptName()).append("|");
        List candidateSemTypes = candidate.getSemTypes();
        sbuf.append(candidateSemTypes.size()).append("|");
        for (int i=0; i < candidateSemTypes.size(); i++)
        {
          String semTypeAbbrev = (String)candidateSemTypes.get(i);
          semTypes.add(semTypeAbbrev);
          sbuf.append(semTypeAbbrev);
          if (i < candidateSemTypes.size() -1)
          {
            sbuf.append("|");
          }
        }
        if (candidateIterator.hasNext()) {
          sbuf.append("|");
        }
      }     
      
      sbuf.append("|");
      if (resultType.equals("word")) {
	sbuf.append("wc");
      } else {
	sbuf.append("dc");
      }
      logger.debug("buildPipedMessage(Utterance,List,List,List): returning: " + sbuf);
      return sbuf.toString();
  }

  public String toString()
  {
    StringBuffer sb = new StringBuffer();
    sb.append("wsd.jdserver.hostname=").append(this.serverHostname).append("\n");
    sb.append("wsd.jdserver.port=").append(this.serverPort).append("\n");
    sb.append("wsd.jdserver.resulttype=").append(this.resultType).append("\n");
    return sb.toString();
  }

  private List getContext(Document doc, Utterance currUtterance, String wordlist)
  {
      List context = new Vector();
      List words = new Vector();
      String currSentence = currUtterance.getSentence();
      StringTokenizer tokenizer = new StringTokenizer(wordlist,",");
      while (tokenizer.hasMoreTokens())
        words.add(((String)tokenizer.nextToken()).toLowerCase());
      Element root = doc.getRootElement();
      Namespace ns = root.getNamespace();

      logger.info("Computing context.");
      //get the utterance list
      List utteranceList = root.getChildren("utterance", ns);
      ListIterator utteranceIterator = utteranceList.listIterator();
      while (utteranceIterator.hasNext())
        {
          Element utteranceNode = (Element)utteranceIterator.next();
 	  Utterance utterance = new Utterance(utteranceNode,ns);
          context.add(utterance.getSentence().replaceAll("\"","\\\\\""));
                             logger.debug("added to context: " + utterance.getSentence().replaceAll("\"","\\\\\""));
        }
      logger.debug("The context is:" + context.toString());
      return context;
  }


    /**
     * Closes the socket pool.
     */
    private static void shutdownSocketPool()
    {
      try {
            fSocketResourcePool.shutdown();
      }
      catch (IOException ioe)
      {
          logger.error("Problem closing the socket pool.");
      }

    }


}// JdDisambiguator
