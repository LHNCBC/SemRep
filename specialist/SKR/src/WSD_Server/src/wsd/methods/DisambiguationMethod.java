
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

import org.jdom.Document;

 /**
  * The public interface for all disambiguation methods. When a new disambiguation
  * method is added to the WSD system, it needs to implement getMatch()
  * method of this interface.
  *
  * <P>The XML structure a disambiguation method receives from the WSD Server
  * is expected to have the following format:
  *
  * <pre>
  * <b>  &lt;machine_output&gt;</b>
  * <b>      &lt;utterance ui</b>=".." <b>pos</b>=".." <b>sentence</b>="..."<b>&gt;</b>
  * <b>          &lt;phrase phrase_pos</b>=".." <b>noun_phrase</b>="..." <b>&gt;</b>
  * <b>              &lt;phrase_elements&gt;</b>
  * <b>                 &lt;phrase_element type</b>=".."<b>&gt;</b>
  * <b>                     &lt;field name</b>=".." <b>value</b>=".."<b>&gt;</b>
  * <b>                     &lt;field name</b>=".." <b>value</b>=".."<b>&gt;</b>
  * <b>                     &lt;field .... &gt;</b>
  * <b>                 &lt;/phrase_element&gt;</b>
  * <b>              &lt;/phrase_elements&gt;</b>
  * <b>              &lt;candidates&gt;</b>
  * <b>                 &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                 &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                 &lt;candidate ....&gt;</b>
  * <b>              &lt;/candidates&gt;</b>
  * <b>              &lt;mappings&gt;</b>
  * <b>                 &lt;mapping score</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate ....&gt;</b>
  * <b>                 &lt;/mapping&gt;</b>
  * <b>                 &lt;mapping score</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate ....&gt;</b>
  * <b>                 &lt;/mapping&gt;</b>
  * <b>                 &lt;mapping .... &gt;</b>
  * <b>              &lt;/mappings&gt;</b>
  * <b>              &lt;ambiguities&gt;</b>
  * <b>                 &lt;ambiguity process</b>="<i>yes/no</i>"<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate score</b>=".." <b>cui</b>=".." <b>umls_concept</b>=".." <b>preferred_name</b>=".." <b>matched_words</b>=".." <b>semtypes</b>=".." <b>matchmap</b>=".." <b>head_flag</b>=".." <b>overmatch_flag</b>=".."<b>&gt;</b>
  * <b>                     &lt;candidate ....&gt;</b>
  * <b>                 &lt;/ambiguity&gt;</b>
  * <b>               &lt;/ambiguities&gt;</b>
  * <b>          &lt;/phrase&gt;</b>
  * <b>      &lt;/utterance&gt;</b>
  * <b>      &lt;methods&gt;</b>
  * <b>          &lt;method method_name</b>="..." <b>weight</b>=".."<b>&gt;</b>
  * <b>          &lt;method method_name ......  &gt;</b>
  * <b>       &lt;/methods&gt;</b>
  * <b>  &lt;/machine_output&gt;</b>
  * </pre>
  *
  * <P>This code was developed for National Library of Medicine, Cognitive
  * Science Branch.
  *
  * <p>Description: Word Sense Disambiguation</p>
  *
  * @version  04/02/02
  * @author   Halil Kilicoglu
  */
public interface DisambiguationMethod
{
   /**
     * getMatch() should be implemented by any disambiguation method.
     *
     * @param doc   a DOM tree that represents the MetaMap machine output that
     *              contains some ambiguity instances.
     *
     * @return      a List that contains the best matches found by this particular
     *              disambiguation method.
     *
     */
   List getMatch(Document doc);
}
