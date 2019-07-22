
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;

import com.sleepycat.db.Db;
import com.sleepycat.db.DbException;

import gov.nih.nlm.nls.utils.StringUtils;

/**
 * Berkeley DB Loader for Susanne's Semantic Type Indexing tables.
 * 
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.  Adapted from Halil's MeSHFrequencyCalculator.java
 * database loader.
 *
 * <p>$Id</p>
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  27jun2006
 * @author   Willie Rogers
 */

public class StiPopulateDb
 {

  public StiPopulateDb(String inputFile, String outputFile)
  {
    createStiBtree(inputFile, outputFile);
  }

  private void createStiBtree(String inputFile, String outputFile)
  {
    String line;
    int err;
    try {
      new File(outputFile).delete();
      Db table = new Db(null, 0);
      table.set_error_stream(System.err);
      table.set_flags(Db.DB_DUPSORT);
      table.set_errpfx("StiPopulateDb");
      table.open(null, outputFile, null, Db.DB_BTREE, Db.DB_CREATE, 0644);
      BufferedReader in = new BufferedReader(new FileReader(new File(inputFile)));
      int i = 0;
      while ((line = in.readLine()) != null) {
        if ((i % 1000) == 0) System.out.print(i + "*");
             // System.out.println("Processing " + line);
          String term = StringUtils.getToken(line, " ", 0);
          if (term != null) {
            StringDbt key = new StringDbt(term);
            StringDbt data = new StringDbt(line);
            if ((err = table.put(null, key, data, 0)) != 0) {
              System.out.println("Problem with adding the key/data: " + term + "+" + line);
              System.out.println("Error code: " + err);
            }
          }
          i++;
        }
      table.close(0);
    } catch (DbException dbe) {
        System.err.println("StiPopulateDb->createStiBtree()::DbException: " + dbe.toString());
    } catch (FileNotFoundException fnfe) {
        System.err.println("StiPopulateDb->createStiBtree()::FileNotFoundException: " + fnfe.getMessage());
    } catch (IOException ioe) {
        System.err.println("StiPopulateDb->createStiBtree()::IOException: " + ioe.getMessage());
    }
  }

  public final static void main(String[] args)
  {
    if (args.length > 1) {
      new StiPopulateDb(args[0],args[1]);
    } else {
      System.err.println("usage: java wsd.util.StiPopulateDb tablefile dbfile");
      System.exit(1);
    }
  }
}
