
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

import java.io.*;
import java.util.*;

/**
 * Describe class InvertedFileIndex here.
 *
 *
 * Created: Tue Aug 29 16:12:34 2006
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class InvertedFileIndex implements Serializable {

 public static void main(String[] args)
   throws FileNotFoundException, IOException, ClassNotFoundException, BSPIndexCreateException
  {
    if (args.length < 3) {
      System.out.println("usage: wsd.util.InvertedFileIndex [index <sourcefile> <indexdir>] [get <indexdir> <term>]");
      System.exit(0);
    }
    if (args[0].equals("index")) {
      String srcfilename = args[1];
      String indexdirpath = args[2];
      File indexdir = new File(indexdirpath);
      if (! indexdir.exists()) 
        indexdir.mkdir();
      if (! indexdir.isDirectory()) {
        System.out.println(indexdir + " is not a directory!");
        System.exit(0);
      }
      String parent = indexdir.getParent();
      if (parent == null) parent = ".";
      List formatList = new ArrayList();
      formatList.add(srcfilename);
      formatList.add(indexdir.getName());
      formatList.add("2");
      formatList.add("0");
      formatList.add("term");
      formatList.add("data");
      formatList.add("TXT");
      formatList.add("TXT");
      InvertedFile invfile = new InvertedFile(indexdir.getName(), srcfilename, parent, formatList);
      invfile.load_map();
      invfile.create();
      // delete temporary files.
    } else if (args[0].equals("get")) {
      String indexdirpath = args[1];
      String term = args[2];
      File indexdir = new File(indexdirpath);
      String parent = indexdir.getParent();
      if (parent == null) parent = ".";
      InvertedFile invfile = InvertedFile.getInstance(indexdir.getName(), parent);
      BSPTuple result = invfile.lookup(term);
      List list = (List)result.getValue();
      for (Iterator j = list.iterator(); j.hasNext(); ) {
	System.out.println(j.next());
      }
    } else {
      System.out.println("usage: wsd.util.InvertedFileIndex [index <sourcefile> <indexdir>] [get <indexdir> <term>]");
      System.exit(0);
    }
  }
}


