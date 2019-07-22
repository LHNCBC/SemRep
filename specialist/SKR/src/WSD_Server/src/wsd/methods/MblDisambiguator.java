
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

import org.apache.log4j.Logger;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.Namespace;

import wsd.model.*;
import wsd.WSDEnvironment;

/**
 * MblDisambiguator.java
 *
 *
 * Created: Fri Apr 25 18:04:04 2003
 *
 * @author <a href="mailto:wrogers@nls10.nlm.nih.gov">Willie Rogers</a>
 * @version $Id: MblDisambiguator.java,v 1.6 2004/04/15 20:07:43 wrogers Exp $
 */

public class MblDisambiguator implements DisambiguationMethod  {

  /** Logger */
  private static Logger logger = Logger.getLogger(MblDisambiguator.class);

  /** Have sharable libraries been loaded, necessary libraries:
   * libWsdMbl.so, libTimbl.so. */
  static boolean librariesLoaded = false;

  /** MBL disambiguator classifier directory */
  String classifierDirectory = 
    WSDEnvironment.properties.getProperty
    ("MBL_CLASSIFIER_DIR",
     "/net/nls10/export/home/wrogers/WSD/studio/MBL");
  
  /** MBL disambiguator reviewed results directory */
  String reviewedResultsDirectory =
    WSDEnvironment.properties.getProperty
    ("MBL_REVIEWED_RESULTS_DIR", 
     "/net/nls10/export/home/wrogers/WSD/studio/Reviewed_Results");

  /** default constructors */
  public MblDisambiguator()
  {
    if (! librariesLoaded) {
      /** necessary libraries: libWsdMbl.so, libTimbl.so */
      System.loadLibrary("Timbl");
      System.loadLibrary("WsdMbl");
      librariesLoaded = true;
    }
    setEnvironment(classifierDirectory, reviewedResultsDirectory);
  }

  // implementation of wsd.disambiguator.DisambiguationMethod interface

  public List getMatch(Document doc)
  {
    List jdResults = new Vector();
    Element root = doc.getRootElement();
    Namespace ns = root.getNamespace();
    
    logger.info("Disambiguating using MBL Method.");
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
    logger.info("Completed disambiguation using MBL Method.");
    return jdResults;
  }

  /**
   * get rank list of one or more semantic types
   *
   * @param text      any text that will be used for disambiguation, may be
   *                  a concept or a sentence etc.
   * @param context   the context of the ambiguity. Initial thought was to
   *                  have several sentences associated with the ambiguity
   *                  as the context.
   * @param semTypes  semantic types of the concepts that cause the ambiguity
   * @return  a Vector that contains the best matches found by a particular
   *          disambiguation method.
   * @deprecated As of version 1.2 of DisambiguatorServerThread.
   */
  public Vector getMatch(String text, Vector context, Vector semTypes)
  {
    String answer =
      getAnswer(text, context.toArray(), semTypes.toArray());
    Vector matches = new Vector(1);
    matches.add(answer);
    return matches;
  }

  /**
   * getMatch() should be implemented by any disambiguation method.
   *
   * @param utterance   The sentence or fragment containing the ambiguity.
   * @param phrases     The phrases in the utterance.
   * @param context     the context of the ambiguity. Initial thought was to have
   *                    several sentences associated with the ambiguity as the context.
   * @param candidates  the concepts that cause the ambiguity
   * @return  a List that contains the best matches found by a particular
   *          disambiguation method.
   */
  public List getMatch(Utterance utterance, List phrases, List context, List candidates)
  {
    List resultConcepts = new Vector();
    List resultSemtypes = new Vector();
    List semtypes = new Vector();

    String matchedWord = ((Candidate)candidates.get(0)).getMatchedWords();
    for (int j=0; j < candidates.size(); j++)
      {
	List tempSemTypes = ((Candidate)candidates.get(j)).getSemTypes();
	for (int i = 0; i < tempSemTypes.size(); i++)
          {
	    semtypes.add((String)tempSemTypes.get(i));
          }
      }
    List listContext = null;
    if (context instanceof List) {
      listContext = (List)context;
    } else {
      listContext = new Vector(context);
    }
    if (listContext.size() == 0) {
      listContext.add(utterance.getSentence());
    }
    logger.debug("calling getAnswer( " + matchedWord + ", " +
		 listContext.toArray() + ", " + semtypes.toArray());
    String answer = this.getAnswer(matchedWord, listContext.toArray(), semtypes.toArray());
    resultSemtypes = new Vector(1);
    resultSemtypes.add(answer);
    if (resultSemtypes.size() > 0)
      {
	for (int i=0; i < resultSemtypes.size(); i++)
          {
	    for (int j=0; j < candidates.size(); j++)
              {
		if (((Candidate)candidates.get(j)).getSemTypes().contains((String)resultSemtypes.get(i)))
		  resultConcepts.add(((Candidate)candidates.get(j)).getPreferredConceptName());
              }
	  }
      }
    return resultConcepts;
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

          //get the noun phrase list
          List phraseList = utteranceNode.getChildren("phrase",ns);
          ListIterator phraseIterator = phraseList.listIterator();
          while (phraseIterator.hasNext())
          {
              Element phraseNode = (Element)phraseIterator.next();
              NounPhrase nounPhrase = new NounPhrase(phraseNode,ns);
              if (phraseNode.hasChildren())
              {
                  //get the phrase elements
                  Element elementsNode = phraseNode.getChild("phrase_elements",ns);
                  List elementList = elementsNode.getChildren("phrase_element",ns);
                  ListIterator elementIterator = elementList.listIterator();
                  while (elementIterator.hasNext())
                  {
                     Element elementNode = (Element)elementIterator.next();
                     List fieldList = elementNode.getChildren("field",ns);
                     ListIterator fieldIterator = fieldList.listIterator();
                     while (fieldIterator.hasNext())
                     {
                        Element fieldNode = (Element)fieldIterator.next();
                        String fieldName = fieldNode.getAttributeValue("name",ns);
                        if (fieldName.equals("inputmatch"))
                        {
                           String value = fieldNode.getAttributeValue("value",ns).toLowerCase();
                           if (words.contains((String)value))
                           {
                              logger.debug("Field name/value: " + fieldName + ":" + value);
                               if (!context.contains(utterance.getSentence()) &&
                                   !currSentence.equals(utterance.getSentence()))
                                  context.add(utterance.getSentence().replaceAll("\"","\\\\\""));
                           }
                        }
                     }
                  }
               }
            }
        }
      logger.debug("The context is:" + context.toString());
      return context;
  }

  /**
   * stub for c++ native method that sets up the Timbl environment.
   *
   * @param classifierDir MBL classifier directory
   * @param reviewedDir   MBL reviewed results directory
   */
  private native void setEnvironment(String classifierDir, String reviewedDir);

  /**
   * stub for c++ native method that calls the Timbl classifier.
   *
   * @param ambiguity name of ambiguity
   * @param context   the context of the ambiguity. Initial thought was to
   *                  have several sentences associated with the ambiguity
   *                  as the context.
   * @param semTypes  semantic types of the concepts that cause the ambiguity
   * @return the semantic type chosen by the disambiguation method.
   * <p>
   * C++ sources: WsdMbl.c, WsdMbl.h, Tokenize.cc, Tokenize.h,
   *              Scanner.cc, Scanner.h, Token.cc, Token.h,
   *              ListUtils.cc, ListUtils.h
   * This also uses the Timbl library (libTimbl.so).
   * </p>
   */
  private native String getAnswer(String ambiguity, Object[] context, Object[] semTypes);

}// MblDisambiguator
