
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

package gov.nih.nlm.nls.utils;

import java.util.StringTokenizer;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * StringUtils.java
 *
 *
 * Created: Tue Jul 10 09:05:44 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: StringUtils.java,v 1.5 2004/09/16 15:15:31 divita Exp $
 */

public final class StringUtils extends Object
{

  /**
   * Split string into substrings based on delimitor characters supplied;
   * resulting substrings are placed in array list in order of appearance.
   *
   * @param textstring string to be split.
   * @param delimitchars delimitor characters.
   * @return List containing substrings or empty list if no strings
   *         were split.
   */
  public static List split(String textstring, String delimitchars)
  {
    StringTokenizer st = new StringTokenizer(textstring, delimitchars, true);
    List list = new ArrayList((st.countTokens() + 1)/ 2);
    String previousToken = "";
    while (st.hasMoreTokens()) 
      {
	String token = st.nextToken();
	if (token.indexOf(delimitchars) < 0 ) {
	  list.add(token);
	} else if (previousToken.indexOf(delimitchars) > -1) {
	  list.add("");
	} 
	previousToken = token;
      }
    if (previousToken.indexOf(delimitchars) > -1) {
      list.add("");
    } 
    return list;
  }

  /**
   * Split string into substrings based on delimitor characters supplied;
   * return substring specified by position.  If there are less tokens than
   * pos then return null.
   *
   * @param textstring string to be split.
   * @param delimitchars delimitor characters.
   * @param pos position of substring to be returned.
   * @return substring at position pos or null if string has less tokens than pos.
   */

  public static String getToken(String textstring, String delimitchars, int pos)
  {
    int i = 0;
    StringTokenizer st = new StringTokenizer(textstring, delimitchars, true); 
    String previousToken = "";
    while (st.hasMoreTokens()) {
      String tok = (String)st.nextToken();

      if (tok.indexOf(delimitchars) < 0 ) {
	if (i == pos) {
	  return tok;
	}
	i++;
      } else if (previousToken.indexOf(delimitchars) > -1) {
	if (i == pos) {
	  return "";
	}
	i++;
      }
      previousToken = tok;
    }
    if (previousToken.indexOf(delimitchars) > -1 && i == pos) {
      return "";
    }
    return "";
  }

  /**
   * Write out string elements of arraylist as a brace encapsulated
   * list separated by spaces; if a token contains spaces then
   * encapsulate the token with braces.
   *
   * @param list  array list of strings to be joined
   * @return string joined strings of array list separated by spaces
   *         encapsulated by braces.
   */
  public static String list(List list)
  {
    StringBuffer sb = new StringBuffer();
    Iterator it = list.iterator();
    sb.append("{");
    while ( it.hasNext() )
      {
	String token = (String)it.next();
	if (token.length() == 0 || token.indexOf(" ") > -1 ) {
	  // if token contains spaces or is of length zero 
	  // then encapsulate token with braces.
	  sb.append("{");
	  sb.append(token);
	  sb.append("}");
	} else {
	  sb.append(token);
	}
	if (it.hasNext()) sb.append(" ");
      }
    sb.append("}");
    return sb.toString();
  }

  /**
   * Write out string elements of arraylist separated by joinstring specified
   * by user.
   * @param list       array list of strings to be joined
   * @param joinString character to join strings together
   * @return string containing joined strings of array list separated by
   *         joinstring.
   */
  public static String join(List list, String joinString)
  {
    StringBuffer sb = new StringBuffer();
    Iterator it = list.iterator();
    while ( it.hasNext() )
      {
	sb.append(it.next());
	if (it.hasNext()) sb.append(joinString);
      }
    return sb.toString();
  }

  /**
   * Take a path (a string containing file system separators) and replace
   * the separators with a pair of separators for operating systems where
   * java treats the separator as an escape character.
   * @param path       the file system path to be modified.
   * @return string containing the new path.
   */
  /*  public static String escapePath( String path)
  {
    String joinstring = U.FS + U.FS ;
    return join( split(path, U.FS), joinstring);
    }*/


/**
 * toCleanedUpString removes embedded whitespace (nl's tabs, multiple spaces)
 * and trims the string up for display
 * 
 * @param pOldString
 * @return String
 * @exception Exception  
*/
public static String toCleanedUpString( String pOldString ) throws Exception
  { 

    String buff = null;  

    if ( pOldString != null ) {
      buff = new String( pOldString.trim() );
      String newBuff = null;

      // -----------------------------
      // remove embedded cr's and tabs
      // -----------------------------
      newBuff = buff.replaceAll( TAB_s,      SPACE_s ); buff = newBuff;
      newBuff = buff.replaceAll( NEWLINE,    SPACE_s ); buff = newBuff;  
      newBuff = buff.replaceAll( RETURN_s,   SPACE_s ); buff = newBuff; 
      newBuff = buff.replaceAll( FORMFEED_s, SPACE_s ); buff = newBuff; 
      
      // -------------------------------------------------
      // Hunt down multiple spaces, replace with one space
      // -------------------------------------------------
      while ( buff.indexOf("  ") > -1 ) {
	newBuff = buff.replaceAll("  ", SPACE_s ); 
	buff = newBuff; 
      }
      
    } // end of if there is an oldString
      
    return( buff );

   } // ***End public String getTrimmedString()

/**
 * isLowercase returns true if the string does not have upper case chars in them
 * and that the string has at least one character (a-z) in it. This is not
 * a utf-8 compliant method, because it only tests within the ranges of [a-z] and
 * [A-Z].
 * 
 * @param pString
 * @return boolean 
*/
public static boolean isLowercase( String pString ) 
  { 
    boolean returnVal = true;
    boolean hasChars = false;
    int lengthOfString = 0;
    char cBuff = ' ';
    
    if ( pString != null ) {
      
      lengthOfString = pString.length();
      for (int i = 0; i < lengthOfString; i++ ) {
	cBuff = pString.charAt(i);
	
	if (( cBuff >= 'a') && ( cBuff <= 'z' ) ) 
	  hasChars = true; 

	if (( cBuff >= 'A') && ( cBuff <= 'Z' ) ) {
	  returnVal = false;
	  break;
	}
      }
	
      if ( hasChars == false )
	returnVal = false;
    }

    return( returnVal);

   } // ***End public String getTrimmedString()


     private static final String      TAB_s = "\t";
     private static final String    NEWLINE = System.getProperty("line.separator");
     private static final String    SPACE_s = " ";
     private static final String FORMFEED_s = "\f";
     private static final String   RETURN_s = "\r";


  /** ----------------
   * main
   * @param args
   *
   *
  public static void main(String[] args)
  {
    if (args.length > 0)
      {
	StringBuffer testSB = new StringBuffer(args[0]);
	for (int i=1; i < args.length; i++) {
	  testSB.append(" ").append(args[i]);
	}
	String testString = testSB.toString();      
	String result = escapePath(testString);
	System.out.println(testString + " path  -> " + result);
      }
  }
   */
} // StringUtils

