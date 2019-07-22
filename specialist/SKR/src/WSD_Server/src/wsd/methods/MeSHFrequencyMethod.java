
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
import com.sleepycat.db.DbException;

import org.apache.log4j.Logger;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.Namespace;

import wsd.model.*;
import wsd.util.StringDbt;
import wsd.WSDEnvironment;


/**
 * MeSHFrequencyMethod class represents a disambiguation method that utilizes
 * MeSH term counts in MeSH headings. These terms are represented in a b-tree
 * structure and are accessed using BerkeleyDB API. This class implements
 * DisambiguationMethod interface.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class MeSHFrequencyMethod implements DisambiguationMethod
{
   /** MeSH synonyms table */
//   private Db fDbVariants = null;
   /** MeSH synonym counts table */
//   private Db fDbVariantCounts = null;

   private static Logger logger = Logger.getLogger(MeSHFrequencyMethod.class);

  /**
   * Implementation of GetMatch() method of DisambiguationMethod interface.
   * Finds and returns the best matching concept(s) using the MeSH Synonym/Count
   * tables.
   *
   * @param   doc   XML Document that contains the ambiguity data.
   *
   * @return  a List that contains the best matches found by MeSH Frequency Method/
   *          disambiguation method.
   */
  public List getMatch(Document doc)
  {
      List meshMethodResults = new Vector();
      Element root = doc.getRootElement();
      Namespace ns = root.getNamespace();

      logger.info("Disambiguating using MeSH Frequency Method.");
//      loadVariantsTable();
//      loadVariantCountsTable();

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
                          while (candidateIterator.hasNext())
                          {
                              Element candidateNode = (Element)candidateIterator.next();
                              Candidate candidate = new Candidate(candidateNode,ns);
                              String preferredName = candidate.getPreferredConceptName();
                              prefNames.add(preferredName);
                          }
                          Vector bestPrefNames = findMostFrequentVariants(prefNames);
                          //create the Result object that stores the ambiguity result data
                          Result res = new Result();
                          res.setCandidatePreferredConceptNames(prefNames);
                          res.setPreferredConceptNames(bestPrefNames);
                          res.setUi(utterance.getUi());
                          res.setUtterancePos(utterance.getPos());
                          res.setPhrasePos(nounPhrase.getPos());
                          meshMethodResults.add(res);
                          if (logger.isDebugEnabled())
                              logger.debug("Result:" + res.getUi() + "|" +
                                                 res.getUtterancePos() + "|" +
                                                 res.getPhrasePos() + "|" +
                                                 res.getCandidatePreferredConceptNames() + "|" +
                                                 res.getPreferredConceptNames());
                      }
                  }
              }
          }
      }
//      closeVariantsTable();
//      closeVariantCountsTable();
      logger.info("Completed disambiguation using MeSH Frequency Method.");
      return meshMethodResults;
  }

  /**
   * Loads the MeSH variants table.
   */
/*  private void loadVariantsTable()
  {
      try
      {
          fDbVariants = new Db(null, 0);
          fDbVariants.set_error_stream(System.err);
          fDbVariants.set_errpfx("MeSHFrequencyMethod:getVariants");
          fDbVariants.set_pagesize(1024);			// 1K page sizes.
//          fDbVariants.set_flags(Db.DB_DUP);
	  fDbVariants.open(null,WSDEnvironment.fMeSHVariantsDBFile, null, Db.DB_BTREE, Db.DB_RDONLY, 0444);
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
*/
  /**
   * Loads the MeSH variant count table.
   */
/*  private void loadVariantCountsTable()
  {
      try
      {
          fDbVariantCounts = new Db(null, 0);
          fDbVariantCounts.set_error_stream(System.err);
          fDbVariantCounts.set_errpfx("MeSHFrequencyMethod:getVariants");
          fDbVariantCounts.set_pagesize(1024);			// 1K page sizes.
//          fDbVariantCounts.set_flags(Db.DB_DUP);
	  fDbVariantCounts.open(null,WSDEnvironment.fMeSHVariantCountDBFile, null, Db.DB_BTREE, Db.DB_RDONLY, 0444);
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
*/
  /**
   * Close the MeSH variants table.
   */
/*  private void closeVariantsTable()
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
*/
  /**
   * Close the MeSH variant count table.
   */
/*  private void closeVariantCountsTable()
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
*/
  /**
   * Gets the MeSH variants for a concept and then computes their frequencies
   * and returns the ones with the highest frequency.
   *
   * @param a list of preferred names of the candidate UMLS concepts.
   *
   * @return the most frequent MeSH variants.
   */
  private PreferredNameVector findMostFrequentVariants(PreferredNameVector candidates)
  {
      PreferredNameVector matches = new PreferredNameVector();
      List variants = null;
      ListIterator candidateIter = candidates.listIterator();
      ListIterator variantIter = null;
      String candidate = null;
      String variant = null;
      int freqCount = 0;
      int prevMax = 0;

      while (candidateIter.hasNext())
      {
          candidate = (String)candidateIter.next();
          variants = getVariants(candidate);
          variantIter = variants.listIterator();

          // MeSH variants found, for each variant get the frequency count and
          // compare with previous max. count.
          while (variantIter.hasNext())
          {
              variant = (String) variantIter.next();
              freqCount = getVariantCount(variant);
              if (freqCount > prevMax)
              {
                  matches.clear();
                  matches.add(candidate);
                  prevMax = freqCount;
              }
              else if (freqCount == prevMax)
                  matches.add(candidate);
          }
      }
      matches.retainAll(candidates);
      return matches;
  }

  /**
   * This method retrieves the MeSH variants of a given concept from the MeSH
   * variant table.
   *
   * @param concept the concept which we will find the MeSH variants of.
   *
   * @return the vector containing the MeSH variants.
   */
  private Vector getVariants(String concept)
  {
      int ret;
      Vector variants = new Vector();
      try
      {
          StringDbt key = new StringDbt(concept);
          StringDbt data = new StringDbt();
          if (WSDEnvironment.fDbVariants == null)
            WSDEnvironment.reinitializeDB();

          if ((ret = WSDEnvironment.fDbVariants.get(null,key,data,0)) == 0)
              variants = new PreferredNameVector(data.getString());
          return variants;
      }
      catch (DbException dbe)
      {
          logger.error(dbe.getMessage());
          return null;
      }
  }

  /**
   * This method retrieves the count of a MeSH variant in MeSH headings using
   * the MeSH variant count table.
   *
   * @param variant the MeSH variant which we will find the count of.
   *
   * @return the # of occurrences of the MeSH variant in MeSH headings.
   */
  private int getVariantCount(String variant)
  {
      int ret;
      int count = -1;
      try
      {
          StringDbt key = new StringDbt(variant);
          StringDbt data = new StringDbt();
          if (WSDEnvironment.fDbVariantCounts == null)
            WSDEnvironment.reinitializeDB();

          ret = WSDEnvironment.fDbVariantCounts.get(null,key,data,0);
          if (ret == 0)
              count = Integer.parseInt(data.getString());
          else if (ret != Db.DB_NOTFOUND)
          {
            logger.error("Db.get() failed");
            throw new DbException("Db.get() failed", ret);
          }
          return count;
      }
      catch (DbException dbe)
      {
          logger.error(dbe.getMessage());
          return count;
      }
  }
}

