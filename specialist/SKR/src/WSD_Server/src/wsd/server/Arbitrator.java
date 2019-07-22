
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

import java.util.List;
import java.util.Vector;
import java.util.Iterator;

import org.apache.log4j.Logger;

import wsd.model.ArbitratorRecord;
import wsd.model.Result;

/**
 * This class represents an Arbitrator object that uses the weights of the
 * disambiguation methods and a list of concepts to find out
 * which one is the best match among these concepts for a particular instance of
 * an ambiguity.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class Arbitrator {

    private static Logger logger = Logger.getLogger(Arbitrator.class);

/**
 * Main entry point for the arbitration process. Scores the concepts and returns
 * the best match.
 *
 * @param results a Result vector of concepts
 * @param methods an array of disambiguation methods
 * @param weights an array of disambiguation method weights
 *
 * @return a List of the highest scored concepts.
 */
//  public String arbitrate(List results, String[] methods, double[] weights)
  public List arbitrate(List results, String[] methods, double[] weights)
  {
      List arbitrationRecords = createArbitrationRecords(results, methods, weights);
      List uniqueConceptList = createUniqueConceptList(arbitrationRecords);
      List returnConceptList = new Vector();
      double[] scores = calculateScores(arbitrationRecords, uniqueConceptList);
      int[] indexes = getBestMatches(scores);
      for (int i=0; i < indexes.length; i++)
      {
          if (indexes[i] >= 0)
         {
             String concept = (String)uniqueConceptList.get(indexes[i]);
              returnConceptList.add(concept);
          }
      }
      if (logger.isDebugEnabled())
          logger.debug("The concept list returned by Arbitrator is: " + returnConceptList.toString());
      return returnConceptList;
  }

/**
 * Creates arbitration records from the results received from the server. An
 * arbitration record consists of a disambiguation method name, a concept name
 * associated with that method and the score of the concept.
 *
 * @param results a Result vector of concepts
 * @param methods an array of disambiguation methods
 * @param weights an array of disambiguation method weights
 *
 * @return a List that consists of ArbitratorRecord objects.
 */
  private List createArbitrationRecords(List disambResults,
                                          String[] methods,
                                          double[] weights)
  {
      Result result = null;
      ArbitratorRecord rec = null;
      Iterator iter = null;
      List records = new Vector();

      for (int i=0; i < methods.length; i++)
      {
          iter = disambResults.iterator();
          while (iter.hasNext())
          {
              result = (Result)iter.next();
              if (methods[i].equals(result.getMethodName()))
              {
                  for (int j=0; j < result.getPreferredConceptNames().size(); j++)
                  {
                      rec = new ArbitratorRecord(methods[i],
                                                 (String)result.getPreferredConceptNames().get(j),
                                                 weights[i]/result.getPreferredConceptNames().size());

                      records.add(rec);
                  }
              }
          }
      }
      return records;
  }

/**
 * Creates a unique concept list. Since different disambiguation methods may
 * return the same concept, a unique list must be created to be able to calculate
 * the scores for these concepts correctly.
 *
 * @param arbitrationRecords  a List of arbitration records that hold the
 *                            concept, method and weight information
 *
 * @return a List that contains the unique concepts.
 */
  private List createUniqueConceptList(List arbitrationRecords)
  {
      List uniqueConcepts = new Vector();
      String conceptFromArbitrator = new String();
      String conceptFromList = new String();

      for (int i=0; i < arbitrationRecords.size(); i++)
      {
          boolean exists = false;
          conceptFromArbitrator = ((ArbitratorRecord)arbitrationRecords.get(i)).getPreferredConceptName();
          for (int j=0; j < uniqueConcepts.size(); j++)
          {
              conceptFromList = (String)uniqueConcepts.get(j);
              if (conceptFromArbitrator.equals(conceptFromList) && !exists)
              {
                  exists = true;
              }
          }
          if (!exists)
              uniqueConcepts.add(conceptFromArbitrator);
      }
      return uniqueConcepts;
  }

/**
 * Calculates the scores for each concept in the unique concept list.
 *
 * @param arbitrationRecords  a List of arbitration records that hold the
 *                            concept, method and weight information
 * @param uniqueConcepts      the unique concept List
 *
 * @return an array of scores
 */
  private double[] calculateScores(List arbitrationRecords, List uniqueConcepts)
  {
      Iterator iter = arbitrationRecords.iterator();
      ArbitratorRecord rec = null;
      double[] scores = new double[uniqueConcepts.size()];
      while (iter.hasNext())
      {
          rec = (ArbitratorRecord)iter.next();
          for (int i=0; i < uniqueConcepts.size(); i++)
          {
              if (rec.getPreferredConceptName().equals((String)uniqueConcepts.get(i)))
              {
                  scores[i] += rec.getWeight();
              }
          }
      }
      return scores;
  }

/**
 * Finds the highest score from a list of scores and returns the indexes of concepts
 * that have this highest socre.
 *
 * @param scores an array of scores.
 *
 * @return the array of indexes of the concepts that have the highest scores in the array.
 */
  private int[] getBestMatches(double[] scores)
  {
      double maxScore = 0;
      int index = -1;
      List indexes = new Vector();

      for (int i=0; i < scores.length; i++)
      {
        if (scores[i] > maxScore)
        {
            indexes.clear();
            indexes.add(new Integer(i));
            maxScore = scores[i];
        }
        else if (scores[i] == maxScore)
            indexes.add(new Integer(i));
      }
      int[] indexArray = new int[indexes.size()];
      for (int j=0; j < indexArray.length; j++)
      {
        indexArray[j] = -1;
      }
      for (int k=0; k < indexes.size(); k++)
      {
        indexArray[k] = ((Integer)indexes.get(k)).intValue();
      }
      return indexArray;
  }
}
