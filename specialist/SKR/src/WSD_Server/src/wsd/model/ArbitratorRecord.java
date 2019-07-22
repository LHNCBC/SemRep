
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

package wsd.model;

/**
 * This class represents an ArbitratorRecord object that consists of a
 * disambiguation method name, a concept that was returned by that method and
 * the weight of the method. It is used by the Arbitrator to compute the scores.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class ArbitratorRecord {

  /** Disambiguation method name */
  private String fMethodName;
  /** Preferred name for the UMLS Concept */
  private String fPreferredConceptName;
  /** Weight associated with the disambiguation method */
  private double fWeight;

  /**
   * Creates an ArbitratorRecord object from a UMLS concept (preferred name),
   * a method name and method weight.
   *
   * @param methodName  a disambiguation method name
   * @param conceptName a UMLS concept(preferred name) returned by the disambiguation method
   * @param score       the weight of the method
   */
  public ArbitratorRecord(String methodName, String conceptName, double weight)
  {
      fMethodName = methodName;
      fPreferredConceptName = conceptName;
      fWeight = weight;
  }

  /**
   * set() method for disambiguation method name.
   *
   * @param methodName the method name to set.
   */
  public void setMethodName(String methodName)
  {
      fMethodName = methodName;
  }

  /**
   * set() method for the UMLS concept.
   *
   * @param conceptName the concept name to set.
   */
  public void setPreferredConceptName(String conceptName)
  {
      fPreferredConceptName = conceptName;
  }

  /**
   * set() method for disambiguation method weight.
   *
   * @param weight the weight assigned to the method.
   */
  public void setWweight(double weight)
  {
      fWeight = weight;
  }

  /**
   * get() method  for the disambiguation method name..
   *
   * @return the String representation of the method name.
   */
  public String getMethodName()
  {
      return fMethodName;
  }

  /**
   * get() method for the UMLS concept.
   *
   * @return the String representation of the concept name.
   */
  public String getPreferredConceptName()
  {
      return fPreferredConceptName;
  }

  /**
   * get() method for the method weight.
   *
   * @return the weight of the method.
   */
  public double getWeight()
  {
      return fWeight;
  }

  /**
   * Converts the ArbitratorRecord object information to String representation.
   * The format is <I>[method name|UMLS concept|weight]</I>
   *
   * @return the String representation of the ArbitratorRecord object.
   */
  public String toString()
  {
      return ("[" + fMethodName +  "|" + fPreferredConceptName + "|" + fWeight + "]").trim();
  }
}
