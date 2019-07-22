
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
 * A Mapping object represents a mapping produced by MetaMap. It consists of a
 * mapping score and the candidates that make up the mapping.
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

public class Mapping {

  /* class members */
  private int fScore;
  private List fCandidates;

  /**
   * Default constructor for the class. No parameters. Member fields must be set
   * later using set() methods, if the default constructor is used.
   */
  public Mapping()
  {
  }

  /**
   * Constructor. Sets all the member fields when the object is created.
   *
   * @param score        the negated score assigned to the mapping by MetaMap.
   * @param candidates   the list of candidates that make up the mapping.
   */
  public Mapping(int score,
                   List candidates)
  {
      fScore = score;
      fCandidates = candidates;
  }

  /**
   * Constructor. Creates a Mapping object from a XML tree node with "mapping"
   * tag. Candidate list is created empty. Use setCandidates() to set it later.
   *
   * @param node  a "mapping" node.
   * @param ns    default namespace.
   */
  public Mapping(Element node, Namespace ns)
  {
      fScore = Integer.parseInt(node.getAttributeValue("score",ns));
      fCandidates = new Vector();
  }

  /**
   * set() method for mapping score.
   *
   * @param   score the mapping score
   */
  public void setScore(int score)
  {
      fScore = score;
  }

  /**
   * set() method for the candidates
   *
   * @param   candidates    the candidate list of the mapping
   */
  public void setCandidates(List candidates)
  {
      fCandidates = candidates;
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
   * get() method for the candidates.
   *
   * @return  the candidate list.
   */
  public List getCandidates()
  {
      return fCandidates;
  }

  /**
   * Creates an easy-to-read string representation of the Mapping object.
   * The string representation has the following format:
   * <p><i>[score|[candidate1]|[candidate2]...]</i>
   * <p>Used mostly for debugging purposes.
   *
   * @return  the string representation of the Mapping object.
   */
  public String toString()
  {
      StringBuffer buf = new StringBuffer();

      buf.append("[");
      buf.append(fScore);
      buf.append("|");

      for (int i = 0; i < fCandidates.size(); i++)
      {
          buf.append(((Candidate)fCandidates.get(i)).toString());
          if (i < fCandidates.size() -1)
          {
              buf.append("|");
          }
      }
      buf.append("]");
      return buf.toString().trim();
  }
}
