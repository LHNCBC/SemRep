
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

import java.util.StringTokenizer;
import java.util.Vector;

import org.jdom.Element;
import org.jdom.Namespace;

/**
 * A Candidate object represents a candidate UMLS concept that maps to a
 * word (or wordlist) in the text. It consists of the wordlist, matching UMLS
 * concepts, preferred concept names and semantic types from the MetaThesaurus,
 * as well as score assigned by MetaMap, matchmap, overmatch and head flags.
 * This class contains mostly set() and get() methods for its member fields.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class Candidate {

  /* class members */
  private int fScore;
  private String fUmlsConcept;
  private String fPreferredConceptName;
  private String fMatchedWords;
  private Vector fSemTypes;
  private String fMatchMap;
  private boolean fHeadFlag;
  private boolean fOvermatchFlag;


  /**
   * Default constructor for the class. No parameters. Member fields must be set
   * later using set() methods, if the default constructor is used.
   */
  public Candidate()
  {
  }

  /**
   * Constructor. Sets all the member fields when the object is created.
   *
   * @param score                 the negated score assigned to the concept by MetaMap.
   * @param umlsConcept           the UMLS concept.
   * @param preferredConceptName  the preferred name for the UMLS concept.
   * @param matchedWords          the words that candidate matched in the phrase (comma-separated)
   * @param semTypes              semantic types associated with the UMLS concept. (comma-separated)
   * @param matchMap              match map list
   * @param headFlag              is the candidate involved with the head of the phrase?
   * @param overmatchFlag         is this an overmatch?
   */
  public Candidate(int score,
                   String umlsConcept,
                   String preferredConceptName,
                   String matchedWords,
                   Vector semTypes,
                   String matchMap,
                   boolean headFlag,
                   boolean overmatchFlag)
  {
      fScore = score;
      fUmlsConcept = umlsConcept;
      fPreferredConceptName = preferredConceptName;
      fMatchedWords = matchedWords;
      fSemTypes = semTypes;
      fMatchMap = matchMap;
      fHeadFlag = headFlag;
      fOvermatchFlag = overmatchFlag;
  }

  /**
   * Constructor. Creates a Candidate object from a XML tree node with "candidate"
   * tag.
   *
   * @param node  a "candidate" node.
   * @param ns    default namespace.
   */
  public Candidate(Element node, Namespace ns)
  {
      fScore = Integer.parseInt(node.getAttributeValue("score",ns));
      fUmlsConcept = node.getAttributeValue("umls_concept",ns);
      fPreferredConceptName = node.getAttributeValue("preferred_name",ns);
      fMatchedWords = node.getAttributeValue("matched_words",ns);

      String strSemtypes = node.getAttributeValue("semtypes",ns);
      fSemTypes = new Vector();
      StringTokenizer semTypeTokenizer = new StringTokenizer(strSemtypes,",");
      while (semTypeTokenizer.hasMoreTokens())
      {
          fSemTypes.add(semTypeTokenizer.nextToken());
      }

      fMatchMap = node.getAttributeValue("matchmap",ns);
      if (node.getAttributeValue("head_flag",ns).equals("yes"))
          fHeadFlag = true;
      else if (node.getAttributeValue("head_flag",ns).equals("no"))
          fHeadFlag = false;

      if (node.getAttributeValue("overmatch_flag",ns).equals("yes"))
          fOvermatchFlag = true;
      else if (node.getAttributeValue("overmatch_flag",ns).equals("no"))
          fOvermatchFlag = false;
  }

  /**
   * set() method for candidate score.
   *
   * @param   score the candidate score
   */
  public void setScore(int score)
  {
      fScore = score;
  }

  /**
   * set() method for MetaThesaurus UMLS concept.
   *
   * @param   umlsConcept    the UMLS concept
   */
  public void setUmlsConcept(String umlsConcept)
  {
      fUmlsConcept = umlsConcept;
  }

  /**
   * set() method for preferred name for the UMLS concept.
   *
   * @param   preferredConceptName    the preferred UMLS concept name
   */
  public void setPreferredConceptName(String preferredConceptName)
  {
      fPreferredConceptName = preferredConceptName;
  }

  /**
   * set() method for matching words from the input text.
   *
   * @param   matchedWords    the matching words
   */
  public void setMatchedWords(String matchedWords)
  {
      fMatchedWords = matchedWords;
  }

  /**
   * set() method for semantic type list.
   *
   * @param   semTypes    the list of semantic types associated with the UMLS
   *                      concept.
   */
  public void setSemTypes(Vector semTypes)
  {
      fSemTypes = semTypes;
  }

  /**
   * set() method for matchmap. A MatchMap list consists of information on how
   * the candidate concept matchs up to words in the original phrase and if there
   * are any lexical variation in the matching. It has the following format:
   * [[phrase word span begin, phrase word span end],[concept word span begin,concept word span end],variation]]
   * An example would be: [[1,1],[1,1],0]]
   * This mapping shows word 1 of the phrase maps to word 1 of the concept with
   * 0 lexical variation.
   *
   * @param   matchMap  the matchmap for the candidate
   */
  public void setMatchMap(String matchMap)
  {
      fMatchMap = matchMap;
  }

  /**
   * set() method for head flag.
   *
   * @param   headFlag    is the candidate involved with the head of phrase?
   */
  public void setHeadFlag(boolean headFlag)
  {
      fHeadFlag = headFlag;
  }

  /**
   * set() method for overmatch flag.
   *
   * @param   overmatchFlag    is this an overmatch?
   */
  public void setOvermatchFlag(boolean overmatchFlag)
  {
      fOvermatchFlag = overmatchFlag;
  }

  /**
   * get() method for the candidate score.
   *
   * @return  the negated score of the candidate.
   */
  public int getScore()
  {
      return fScore;
  }

  /**
   * get() method for the UMLS concept.
   *
   * @return  the UMLS concept from the MetaThesaurus.
   */
  public String getUmlsConcept()
  {
      return fUmlsConcept;
  }

  /**
   * get() method for the UMLS concept preferred name.
   *
   * @return  the preferred name for the UMLS concept
   */
  public String getPreferredConceptName()
  {
      return fPreferredConceptName;
  }

  /**
   * get() method for the matching words.
   *
   * @return  the words in the text that caused the ambiguity.
   */
  public String getMatchedWords()
  {
      return fMatchedWords;
  }

  /**
   * get() method for the semantic types associated with the UMLS concept.
   *
   * @return  the semantic types of the UMLS concept.
   */
  public Vector getSemTypes()
  {
      return fSemTypes;
  }

  /**
   * get() method for the matchmap list.
   *
   * @return  the matchmap list of the candidate.
   */
  public String getMatchMap()
  {
      return fMatchMap;
  }

  /**
   * get() method for the head flag.
   *
   * @return  the head flag.
   */
  public boolean getHeadFlag()
  {
      return fHeadFlag;
  }

  /**
   * get() method for the overmatch flag.
   *
   * @return  the overmatch flag.
   */
  public boolean getOvermatchFlag()
  {
      return fOvermatchFlag;
  }

  /**
   * Creates an easy-to-read string representation of the Candidate object.
   * The string representation has the following format:
   * <p><i>[score|UMLS concept|preferred name|matched words|semantic types|match map list|head flag(0,1)|overmatch_flag(0,1)]</i>
   * <p>Used mostly for debugging purposes.
   *
   * @return  the string representation of the Candidate object.
   */
  public String toString()
  {
      return ("[" + fScore + "|" + fUmlsConcept + "|" + fPreferredConceptName + "|" +
                  fMatchedWords + "|" + fSemTypes.toString()+  "|" + fMatchMap + "|" +
                  fHeadFlag + "|" + fOvermatchFlag + "]").trim();
  }
}
