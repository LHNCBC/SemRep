
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

package wsd.util;

/**
 * BSPIndexInvalidException.java
 *
 *
 * Invalidd: Fri Jul 20 10:51:39 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: BSPIndexInvalidException.java,v 1.1 2006/09/25 18:33:06 wrogers Exp $
 */

public class BSPIndexInvalidException extends Exception {
  /**
   * Instantiate new index validation exception.
   */
  public BSPIndexInvalidException () {
    super("BSPIndexInvalidException");
  }
  /**
   * Instantiate new index validation exception.
   * @param message additional information about exception.
   */
  public BSPIndexInvalidException (String message) {
    super(message);
  }
}// BSPIndexInvalidException
