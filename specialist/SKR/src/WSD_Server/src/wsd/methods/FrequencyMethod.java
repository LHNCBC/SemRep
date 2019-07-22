
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

import java.util.List;
import java.util.ListIterator;
import java.util.Vector;

import com.sleepycat.db.Db;
import com.sleepycat.db.Dbc;
import com.sleepycat.db.DbException;

import org.apache.log4j.Logger;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.Namespace;

import wsd.model.*;
import wsd.util.StringDbt;
import wsd.WSDEnvironment;

/**
 * Frequency Method works for 50 ambiguities in the WSD Test Collection. It uses
 * the voting results of the jury to find the best match. The results are loaded
 * from a text file.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class FrequencyMethod implements DisambiguationMethod
{
    /** logger for FrequencyMethod class */
    private static Logger logger = Logger.getLogger(FrequencyMethod.class);

  /**
   * Implementation of GetMatch() method of DisambiguationMethod interface.
   * Finds and returns the best matching concept(s) using the file that
   * contains the highest scored concepts by the voters.
   *
   * @param   doc   XML Document that contains the ambiguity data.
   *
   * @return  a List that contains the best matches found by a particular
   *          disambiguation method.
   */
  public List getMatch(Document doc)
  {
      List freqMethodResults = new Vector();
      Element root = doc.getRootElement();
      Namespace ns = root.getNamespace();

      logger.info("Disambiguating using Simple Frequency Method.");
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
                  Element ambiguitiesNode = (Element)phraseNode.getChild("ambiguities",ns);
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
                          while (candidateIterator.hasNext())
                          {
                              Element candidateNode = (Element)candidateIterator.next();
                              Candidate candidate = new Candidate(candidateNode,ns);
                              String preferredName = candidate.getPreferredConceptName();
                              prefNames.add(preferredName);
                          }
                          Vector bestPrefNames = findReviewedAnswer(prefNames);
                          //create the Result object that stores the ambiguity result data
                          Result res = new Result();
                          res.setCandidatePreferredConceptNames(prefNames);
                          res.setPreferredConceptNames(bestPrefNames);
                          res.setUi(utterance.getUi());
                          res.setUtterancePos(utterance.getPos());
                          res.setPhrasePos(nounPhrase.getPos());
                          freqMethodResults.add(res);
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
      logger.info("Completed disambiguation using Simple Frequency Method.");
      return freqMethodResults;
  }

  /**
   * Finds the best matching concepts from a list of candidates.
   *
   * @param candidates a list of preferred names of the candidate UMLS concepts
   *
   * @return a list of preferred names of the selected UMLS concepts
   */
  private PreferredNameVector findReviewedAnswer(PreferredNameVector candidates)
  {
      PreferredNameVector matches = null;
      StringDbt key = new StringDbt();
      StringDbt data = new StringDbt();

      try
      {
        if (WSDEnvironment.fDbAnswers == null)
          WSDEnvironment.reinitializeDB();

        /* create a cursor to find the record */
        Dbc c = WSDEnvironment.fDbAnswers.cursor(null, 0);
        while (c.get(key,data,Db.DB_NEXT) == 0)
        {
          PreferredNameVector senses = new PreferredNameVector(key.getString());
          if (!senses.retainAll(candidates))
          {
            logger.debug("[Key-Data Pair: " + key.getString() + "|" + data.getString() + "]");
            matches = new PreferredNameVector(data.getString());
            break;
          }
        }
        c.close();
      }
      catch (DbException dbe)
      {
        logger.error(dbe.getMessage());
      }
      return matches;
  }
}
