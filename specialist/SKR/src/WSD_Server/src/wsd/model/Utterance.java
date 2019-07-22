
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

import java.util.List;
import java.util.Vector;

import org.jdom.Element;
import org.jdom.Namespace;

/**
 * This class represents an utterance and the noun phrases in that utterance.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class Utterance
{
  /** utterance position in the text */
  private int fPos;
  /** sentence text */
  private String fSentence;
  /** utterance id */
  private String fUi;
  /** simple noun phrases in the utterance */
  private List fNounPhrases;

  /**
   * Default constructor for the class. No parameters.
   */
  public Utterance()
  {
  }

  /**
   * Constructor. Sets the member fields when the object is created.
   */
  public Utterance(int pos,
                   String sentence,
                   String ui,
                   List nounPhrases)
  {
      fPos = pos;
      fSentence = sentence;
      fUi = ui;
      fNounPhrases = nounPhrases;
  }

  /**
   * Constructor. Creates an Utterance object from an XML tree node. Noun phrase
   * list is created empty. May use setNounPhrases() to set it later.
   *
   * @param node  utterance node in the XML tree.
   * @param ns    default namespace.
   *
   */
  public Utterance(Element node, Namespace ns)
  {
      fPos = Integer.parseInt(node.getAttributeValue("pos",ns));
      fSentence = node.getAttributeValue("sentence",ns);
      fUi = node.getAttributeValue("ui",ns);
      fNounPhrases = new Vector();
  }

  /**
   * set() method for utterance position.
   *
   * @param  pos    utterance position
   */
  public void setPos(int pos)
  {
      fPos = pos;
  }

  /**
   * set() method for sentence.
   *
   * @param   sentence    the sentence
   */
  public void setSentence(String sentence)
  {
      fSentence = sentence;
  }

  /**
   * set() method for utterance id.
   *
   * @param   ui    the utterance id
   */
  public void setUi(String ui)
  {
      fUi = ui;
  }

  /**
   * set() method for noun phrases.
   *
   * @param   nounPhrases    the noun phrases vector
   */
  public void setNounPhrases(List nounPhrases)
  {
      fNounPhrases = nounPhrases;
  }

   /**
   * get() method for utterance position.
   *
   * @return  utterance position
   */
  public int getPos()
  {
      return fPos;
  }

  /**
   * get() method for sentence.
   *
   * @return  sentence
   */
  public String getSentence()
  {
      return fSentence;
  }

  /**
   * get() method for utterance id.
   *
   * @return the utterance id
   */
  public String getUi()
  {
      return fUi;
  }

  /**
   * get() method for noun phrases.
   *
   * @return the noun phrases vector
   */
  public List getNounPhrases()
  {
      return fNounPhrases;
  }

  /**
   * Creates an easy-to-read string representation of the utterance.
   * The string representation has the following format:
   * <p><i>[utterance id|utterance position|the sentence]</i>
   * <p>Used mostly for debugging purposes.
   *
   * @return  the string representation of the utterance.
   */
  public String toString()
  {
      return ("[" + fUi + "|" +  fPos + "|" + fSentence + "]").trim();
  }
}