
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

import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.StringTokenizer;
import java.util.Vector;

import java.net.MalformedURLException;

import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

import com.sleepycat.db.Db;
import com.sleepycat.db.DbException;

import gov.nih.nlm.kss.api.KSSRetrieverV2_1;
import gov.nih.nlm.kss.models.meta.concept.Concept;
import gov.nih.nlm.kss.models.meta.concept.Term;
import gov.nih.nlm.kss.models.meta.concept.StringInfo;
import gov.nih.nlm.kss.models.meta.concept.StringSource;
import gov.nih.nlm.kss.util.DatabaseException;
import gov.nih.nlm.kss.util.XMLException;

import wsd.util.StringDbt;

/**
 * This class calculates MeSH term frequencies from the MeSH Term counts. Since
 * there are approximately 20K MeSH terms currently, the concepts related to
 * these terms and the counts of these terms are loaded to a Berkeley DB btree
 * for faster retrieval. This program needs to be run offline when new version
 * of UMLS is released.
 *
 * <P>This code was developed for National Library of Medicine, Cognitive
 * Science Branch.
 *
 * <p>Description: Word Sense Disambiguation</p>
 *
 * @version  04/02/02
 * @author   Halil Kilicoglu
 */

public class MeSHFrequencyCalculator
{
  /** UMLS KSS Host */
  private String fHost;
  /** UMLS Release */
  private String fRelease; //UMLS Release

  /**
   * Constructor for the class.
   *
   * @param host    UMLS KSS host
   * @param release UMLS release
   */
  public MeSHFrequencyCalculator(String host, String release)
  {
      fHost = host;
      fRelease = release;
  }

  /**
   * Creates a text file with the concepts associated with a MeSH term and all
   * the MeSH variants of that concept. This file will be used as the basis of
   * the b-tree's.
   *
   * @param inputFile   the MeSH terms count file
   * @param outputFile  the output file with MeSH terms and related concept information.
   */

  private void createVariantsFile(String inputFile, String outputFile)
  {
      KSSRetrieverV2_1 retriever = null;
      String service = "KSSRetriever";

      char[] result;
      StringReader reader = null;

      Concept concept = null;
      Term term = null;
      StringInfo variant = null;
      StringSource source = null;

      List conceptList = new Vector();
      ListIterator conceptIterator = null;
      Iterator termIterator = null;
      Iterator termVariantIterator = null;
      Iterator sourceIterator = null;

      boolean removed = false;
      boolean hasMeSH = false;
      int count;
      int countMajor;
      String MeSHTerm;

      String line = new String();
      StringTokenizer tokenizer = null;
      String outputStr = new String();
      String name = "//" + fHost + "/" + service;

      try
      {
          retriever = (KSSRetrieverV2_1)Naming.lookup(name);
          BufferedReader in = new BufferedReader(
                new FileReader(new File(inputFile)));
          PrintWriter out = new PrintWriter(
                new BufferedWriter(new FileWriter(outputFile, false)));

          while ((line = in.readLine()) != null)
          {
              //read the MeSH term from the input file
              System.out.println("Processing " + line);
              tokenizer = new StringTokenizer(line,"|");
              count = Integer.parseInt(tokenizer.nextToken());
              countMajor = Integer.parseInt(tokenizer.nextToken());
              MeSHTerm = tokenizer.nextToken();

              //get the concept information for the MeSH term
              result = retriever.findBasicConcept(fRelease,MeSHTerm,null,"ENG",KSSRetrieverV2_1.NormalizeString,false);
              reader = new StringReader(new String(result));
              SAXBuilder builder = new SAXBuilder(false);
              Document doc = builder.build(reader);
              Element root = doc.getRootElement();
              removed = root.removeChild("release");
              if (removed && doc.getRootElement().hasChildren())
              {
                  conceptList = doc.getRootElement().getChildren("concept",doc.getRootElement().getNamespace());
              }
              conceptIterator = conceptList.listIterator();

              //for each concept, find the MeSH synonyms and write to the output file
              while (conceptIterator.hasNext())
              {
                  concept = new Concept((Element)conceptIterator.next(), doc.getRootElement().getNamespace());
                  hasMeSH = false;
                  outputStr = new String();
                  outputStr = concept.getCN() + "|[";
                  termIterator = concept.getTerms().iterator();
                  while (termIterator.hasNext())
                  {
                      term = (Term)termIterator.next();
                      termVariantIterator = term.getTV().iterator();
                      while (termVariantIterator.hasNext())
                      {
                          variant = (StringInfo)termVariantIterator.next();
                          sourceIterator = variant.getSS().iterator();
                          while (sourceIterator.hasNext())
                          {
                              source = (StringSource) sourceIterator.next();
                              if (source.getSAB().startsWith("MSH"))
                              {
                                  hasMeSH= true;
                                  outputStr += variant.getSTR() + "$";
                                  break;
                              }
                          }
                      }
                  }
                  outputStr = outputStr.substring(0,outputStr.length()-1);
                  outputStr += "]";
                  if (hasMeSH)
                      out.println(outputStr);
              }

          }
          out.flush();
          out.close();
          in.close();
      }
      catch (FileNotFoundException fnfe)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantsFile()::FileNotFoundException: " + fnfe.getMessage());
      }
      catch (RemoteException re)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantsFile()::RemoteException: " + re.getMessage());
      }
      catch (MalformedURLException mfurle)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantsFile()::MalformedURLException: " + mfurle.getMessage());
      }
      catch(IOException ioe)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantsFile()::IOException: " + ioe.getMessage());
      }
      catch (XMLException xmle)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantsFile()::XMLException: " + xmle.getMessage());
      }
      catch (NotBoundException nbe)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantsFile()::NotBoundException: " + nbe.getMessage());
      }
      catch (DatabaseException de)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantsFile()::DatabaseException-> " + de.getMessage());
      }
      catch (JDOMException jde)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantsFile()::JDOMException-> " + jde.getMessage());
      }
  }

  /**
   * Creates the b-tree representation of the file created by createVariantsFile()
   * method.
   *
   * @param inputFile   the name of the file created by createVariantsFile() method
   * @param outputFile  the file that will hold the b-tree representation of the
   *                    input file
   */
  private void createVariantBtree(String inputFile, String outputFile)
  {
      String line = new String();
      StringTokenizer tokenizer = null;
      String concept = new String();
      String variants = new String();
      StringDbt key = null;
      StringDbt data = null;
      int err;

      try
      {
          new File(outputFile).delete();
          Db table = new Db(null, 0);
          table.set_error_stream(System.err);
          table.set_flags(Db.DB_DUPSORT);
          table.set_errpfx("MeSHFrequencyCalculator");
          table.open(null, outputFile, null, Db.DB_BTREE, Db.DB_CREATE, 0644);

          BufferedReader in = new BufferedReader(
                  new FileReader(new File(inputFile)));
          while ((line = in.readLine()) != null)
          {
              System.out.println("Processing " + line);
              tokenizer = new StringTokenizer(line,"|");
              concept = tokenizer.nextToken();
              variants = tokenizer.nextToken();

              key = new StringDbt(concept);
              data = new StringDbt(variants);

              if ((err = table.put(null, key, data, 0)) != 0)
              {
                  System.out.println("Problem with adding the key/data: " + concept + "/" + variants);
                  System.out.println("Error code: " + err);
              }
          }
          table.close(0);
      }
      catch (DbException dbe)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantBtree()::DbException: " + dbe.toString());
      }
      catch (FileNotFoundException fnfe)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantBtree()::FileNotFoundException: " + fnfe.getMessage());
      }
      catch (IOException ioe)
      {
          System.err.println("MeSHFrequencyCalculator->createVariantBtree()::IOException: " + ioe.getMessage());
      }

  }

  /**
   * Creates the b-tree representation of the MeSH term count file. Along with
   * the variants b-tree, this b-tree is used to find the best match by MeSH
   * Frequency Method.
   *
   * @param inputFile   the name of the MeSH term count file
   * @param outputFile  the file that will hold the b-tree representation of the
   *                    input file
   */
  private void createCountBtree(String inputFile, String outputFile)
  {
      String line = new String();
      StringTokenizer tokenizer = null;
      String term = new String();
      String count = new String();
      String dummy = new String();
      int err;

      try
      {
          new File(outputFile).delete();
          Db table = new Db(null, 0);
          table.set_error_stream(System.err);
          table.set_errpfx("MeSHFrequencyCalculator");
          table.open(null, outputFile, null, Db.DB_BTREE, Db.DB_CREATE, 0644);

          BufferedReader in = new BufferedReader(
              new FileReader(new File(inputFile)));
          while (( line = in.readLine()) != null)
          {
              tokenizer = new StringTokenizer(line,"|");
              count = tokenizer.nextToken();
              dummy = tokenizer.nextToken();
              term = tokenizer.nextToken();
              StringDbt key = new StringDbt(term);
              StringDbt data = new StringDbt(count);
              if ((err = table.put(null, key, data, 0)) != 0)
              {
                  System.out.println("Problem with adding the key/data: " + term + "/" + count);
                  System.out.println("Error code: " + err);
              }
          }
          table.close(0);
          in.close();
      }
      catch (DbException dbe)
      {
          System.err.println("MeSHFrequencyCalculator->createCountBtree()::DbException: " + dbe.toString());
      }
      catch (FileNotFoundException fnfe)
      {
          System.err.println("MeSHFrequencyCalculator->createCountBtree()::FileNotFoundException: " + fnfe.getMessage());
      }
      catch (IOException ioe)
      {
          System.err.println("MeSHFrequencyCalculator->createCountBtree()::IOException: " + ioe.getMessage());
      }

  }


  /**
   * the main method for the pre-computation of MeSH term frequencies.
   */
    public static void main(String[] args)
    {
        MeSHFrequencyCalculator calculator = new MeSHFrequencyCalculator(args[0],args[1]);
        calculator.createVariantsFile(args[2], args[3]);
        calculator.createVariantBtree(args[3], args[2]+ ".variant.db");
        calculator.createCountBtree(args[2], args[2]+ ".count.db");
        System.exit(0);
    }

}
