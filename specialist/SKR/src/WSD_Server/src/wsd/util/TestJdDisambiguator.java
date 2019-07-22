
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

import java.util.Vector;
import java.util.List;
import java.util.Iterator;

import wsd.methods.DisambiguationMethod;
import wsd.methods.JdDisambiguator;
import wsd.methods.MblDisambiguator;

/**
 * TestJdDisambiguator.java
 *
 *
 * Created: Tue Jun  4 11:35:59 2002
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: TestJdDisambiguator.java,v 1.1 2004/03/08 23:04:51 halil Exp $
 */

public class TestJdDisambiguator {
  public static void main(String[] args)
  {
    wsd.WSDEnvironment.initialize();
    DisambiguationMethod method = null;
    if (args.length > 0 && args[0].equals("mbl")) {
      method = new MblDisambiguator();
    } else {
      method = new JdDisambiguator();
    }
    Vector context = new Vector(2);
    Vector semtypes = new Vector(5);
    String utterance;
    if (args.length > 1) {
      utterance = args[0];
      for (int i = 1; i < args.length; i++) {
	semtypes.add(args[i]);
      }
    } else {
      utterance = "degree";
   // utterance = "The degree of lipid peroxidation, expressed as fluorescence intensity f547, was assessed for stimulation of lymphocytes with concanavalin A (Con A), and was related to lymphocyte proliferation in response to Con A if Se was administered.";
      context.add("The mechanism for the effect of selenium supplementation on immunity.  ");
      context.add("Lipid peroxide (LPO) in lymphocytes from mice was evaluated by measuring substances reactive to thiobarbituric acid (TBA). The product resulting from the reaction of TBA with lymphocytes was extracted with n-butyl and fluorescence intensity was determined.  The degree of lipid peroxidation, expressed as fluorescence intensity f547, was assessed for stimulation of lymphocytes with concanavalin A (Con A), and was related to lymphocyte proliferation in response to Con A if Se was administered. The lymphocyte proliferation was determined by [3H]thymidine incorporation, expressed as cpm. The effect of superoxide dismutase (SOD), added to cell culture on lymphocyte proliferation was also evaluated. It was found that LPO in lymphocytes before Con A stimulation was significantly less than that after stimulation (p < 0.001), and that SOD promoted lymphocyte proliferation dose dependently. The addition of Na2Seo3 to lymphocyte culture or supplementation in drinking water to mice decreased the produced LPO in lymphocyte in response to Con A. In the presence of Se, there is an inverse correlation between the levels of LPO in lymphocyte and the stimulated proliferation (r = -0.8902, r = -0.9439). In conclusion, active oxygen species scavenging was proposed as one of the mechanisms for Se to promote immunity.");
      semtypes.add("qlco");
      semtypes.add("inpr");
    }
    if (method instanceof JdDisambiguator)
      System.out.println("connecting to port: " +
			 ((JdDisambiguator)method).getServerPort());
    /*Vector result = method.getMatch(utterance, context, semtypes);*/
    /*List result = method.getMatch(preferredNames, text, context, senses);*/
    List result = new Vector();/*fixme*/
    Iterator iter = result.iterator();
    while (iter.hasNext()) {
      System.out.println("(TestJdDisambiguator): semtype: " + iter.next());
    }
  }

}// TestJdDisambiguator
