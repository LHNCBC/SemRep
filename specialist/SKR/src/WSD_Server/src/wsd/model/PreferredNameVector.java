
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

import java.util.Collection;
import java.util.Vector;
import java.util.StringTokenizer;

import wsd.WSDEnvironment;

/**
 * This class extends Vector class and provides an easy way to
 * convert a preferred name concept array to a string and vice versa. The string
 * format of a PreferredNameVector is used extensively throughout the WSD system.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */
public class PreferredNameVector extends Vector {

  /**
   * Default constructor. No parameters.
   */
  public PreferredNameVector()
  {
      super();
  }

  /**
   * Constructor that creates a PreferredNameVector object from a Collection object.
   *
   * @param c the Collection object that forms the basis of the PreferredNameVector
   *          object
   */
  public PreferredNameVector(Collection c)
  {
      super(c);
  }

  /**
   * Constructor that creates a PreferredNameVector object from a string. The
   * input string is expected to have one of the following formats:
   * <P> <i>[concept1$concept2$...]</i>
   * <P> or
   * <P> <i> concept1$concept2$......</i>
   * <P> where $ is the PreferredNameVector delimiter. It may be set to a different
   * character using WSD Server configuration file.
   *
   * @param text  the input string
   */
  public PreferredNameVector(String text)
  {
      super();
      String trimmedText = text;
      if (text.charAt(0) == '[' && text.charAt(text.length()-1) == ']')
        trimmedText = text.substring(1,text.length()-1);
      StringTokenizer tokenizer = new StringTokenizer(trimmedText,WSDEnvironment.fPreferredNameSeparator);

      while (tokenizer.hasMoreTokens())
      {
          add(tokenizer.nextToken());
      }
  }

  /**
   * Converts a PreferredNameVector object to String.
   * <P> Output has the format: <i>[concept1$concept2$ ...]</i>
   *
   * @return the string representation of the PreferredNameVector object.

   */
  public String convertToString()
  {
      StringBuffer buf = new StringBuffer();
      buf.append("[");
      if (this.size() > 0)
      {
          for (int i=0; i< size(); i++)
          {
             buf.append((String)elementAt(i));
             if (i < size() - 1)
                buf.append(WSDEnvironment.fPreferredNameSeparator);
          }
      }
      buf.append("]");
      return buf.toString().trim();
  }

}