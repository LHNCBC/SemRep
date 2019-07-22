package entrezgene;

import java.io.*;
import java.util.*;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;

public class ParseEntrezGene {

  private static String inFile = "";
  private static String symbolFile = "";
  private static String aliasFile = "";

  public void parse()
  {
    String inputLine = new String();
    int docCount =0;
    StringBuffer text = new StringBuffer();

    try {
      BufferedReader in = new BufferedReader(new FileReader(inFile));
      PrintWriter symbolOut = new PrintWriter(new FileWriter(symbolFile));
      PrintWriter aliasOut = new PrintWriter(new FileWriter(aliasFile));
      while ( (inputLine = in.readLine()) != null)
      {
//        System.out.println("InputLine: " + inputLine);
        if(inputLine.equals(""))
        {	  
          String geneid = new String();
          String official_symbol = new String();
          String preferred_symbol = new String();
          String status = new String();
	  String org_name = new String();
          List alias_symbols = new Vector();
          String official_gene_name = new String();
          String preferred_gene_name = new String();
          List GO_anns_func = new Vector();
          List GO_anns_proc = new Vector();
          List GO_anns_comp = new Vector();
          StringBuffer GO_ann_buf = new StringBuffer();
          boolean writeGO = false;

          Document doc = null;
          SAXBuilder builder = new SAXBuilder();
          StringReader sr = new StringReader(text.toString());
          doc = builder.build(sr);
          if (doc != null) {
            Element root = doc.getRootElement();
            status = root.getChild("Entrezgene_track-info").getChild(
                "Gene-track").getChild("Gene-track_status").getAttributeValue("value");
            if (status.equals("live")) {
              docCount++;
              geneid = root.getChild("Entrezgene_track-info").getChild(
                  "Gene-track").getChildTextTrim("Gene-track_geneid");
	      org_name = root.getChild("Entrezgene_source").getChild("BioSource").getChild("BioSource_org").getChild("Org-ref").getChildTextTrim("Org-ref_taxname");								  
              System.out.print(geneid + "|" + docCount);
              Element gene_ref = root.getChild("Entrezgene_gene").getChild(
                  "Gene-ref");
              if (gene_ref.getChild("Gene-ref_locus") != null) {
                official_symbol = gene_ref.getChildTextTrim("Gene-ref_locus");
                alias_symbols.add(official_symbol);
                if (gene_ref.getChild("Gene-ref_desc") != null) {
                  official_gene_name = gene_ref.getChildTextTrim(
                      "Gene-ref_desc");
                  alias_symbols.add(official_gene_name);
                }
              }
              else {
                if (gene_ref.getChild("Gene-ref_desc") != null) {
                    preferred_symbol = gene_ref.getChildTextTrim("Gene-ref_desc");
                    preferred_gene_name = preferred_symbol;
                    alias_symbols.add(preferred_gene_name);
                }
              }
              if (gene_ref.getChild("Gene-ref_syn") != null) {
                Iterator iter = gene_ref.getChild("Gene-ref_syn").getChildren().
                    iterator();
                while (iter.hasNext()) {
                  alias_symbols.add( ( (Element) iter.next()).getTextTrim());
                }
              }
              if (root.getChild("Entrezgene_properties") != null) {
                Iterator commentary_iter1 = root.getChild(
                    "Entrezgene_properties").
                    getChildren("Gene-commentary").iterator();
                while (commentary_iter1.hasNext()) {
                  Element commentary1 = (Element) commentary_iter1.next();
                  if (commentary1.getChild("Gene-commentary_heading") != null &&
                      commentary1.getChildTextTrim("Gene-commentary_heading").
                      equals("GeneOntology")) {
                    //Function
                    Iterator commentary_iter2 = commentary1.getChild(
                        "Gene-commentary_comment").getChildren(
                        "Gene-commentary").
                        iterator();
                    while (commentary_iter2.hasNext()) {
                      Element commentary2 = (Element) commentary_iter2.next();
//                    System.out.println(commentary2.getChildTextTrim("Gene-commentary_label"));
                      String GO_ann = commentary2.getChildTextTrim(
                          "Gene-commentary_label");
                      Iterator commentary_iter3 = commentary2.getChild(
                          "Gene-commentary_comment").getChildren(
                          "Gene-commentary").iterator();
                      while (commentary_iter3.hasNext()) {
                        Element commentary3 = (Element) commentary_iter3.next();
                        if (GO_ann.equals("Function")) {
                          GO_anns_func.add(commentary3.getChild(
                              "Gene-commentary_source").getChild("Other-source").
                                           getChildTextTrim(
                              "Other-source_anchor"));
                        }
                        else if (GO_ann.equals("Process")) {
                          GO_anns_proc.add(commentary3.getChild(
                              "Gene-commentary_source").getChild("Other-source").
                                           getChildTextTrim(
                              "Other-source_anchor"));
                        }
                        else if (GO_ann.equals("Component")) {
                          GO_anns_comp.add(commentary3.getChild(
                              "Gene-commentary_source").getChild("Other-source").
                                           getChildTextTrim(
                              "Other-source_anchor"));
                        }
                      }
                    }
                  }
                }
              }
              if (official_symbol.length() > 0) {
//              System.out.print(geneid + "|" + official_symbol + "|OFFICIAL_SYMBOL");
                symbolOut.print(geneid + "|" + normalize(official_symbol) + "|" +
                                official_symbol + "|" + "OFFICIAL_SYMBOL" + "|" + org_name);
                writeGO = true;
              }
              else {
//              System.out.print(geneid + "|" + preferred_symbol + "|PREFERRED_SYMBOL");
                if (preferred_symbol.length() > 0) {
                  symbolOut.print(geneid + "|" + normalize(preferred_symbol) +
                                  "|" + preferred_symbol + "|" +
                                  "PREFERRED_SYMBOL" + "|" + org_name);
                  writeGO = true;
                }
              }
              if (writeGO)  {
                symbolOut.print("|");
                if (GO_anns_func.size() > 0) {
                  GO_ann_buf = new StringBuffer();
                  symbolOut.print("MF:");
                  Iterator iter3 = GO_anns_func.iterator();
                  while (iter3.hasNext()) {
                    GO_ann_buf.append( (String) iter3.next() + "^");
                  }
                  if (GO_ann_buf.length() > 0)
                    symbolOut.print(GO_ann_buf.substring(0, GO_ann_buf.length() - 1));
                }
                symbolOut.print("|");
                if (GO_anns_proc.size() > 0) {
                  GO_ann_buf = new StringBuffer();
                  symbolOut.print("BP:");
                  Iterator iter3 = GO_anns_proc.iterator();
                  while (iter3.hasNext()) {
                    GO_ann_buf.append( (String) iter3.next() + "^");
                  }
                  if (GO_ann_buf.length() > 0)
                    symbolOut.print(GO_ann_buf.substring(0,GO_ann_buf.length() - 1));
                }
                symbolOut.print("|");
                if (GO_anns_comp.size() > 0) {
                  GO_ann_buf = new StringBuffer();
                  symbolOut.print("CC:");
                  Iterator iter3 = GO_anns_comp.iterator();
                  while (iter3.hasNext()) {
                    GO_ann_buf.append( (String) iter3.next() + "^");
                  }
                  if (GO_ann_buf.length() > 0)
                    symbolOut.print(GO_ann_buf.substring(0, GO_ann_buf.length() - 1));
                }
                symbolOut.println("|");
                if (alias_symbols.size() > 0) {
                  Iterator iter2 = alias_symbols.iterator();
                  while (iter2.hasNext()) {
                    String alias = (String) iter2.next();
//                System.out.print("|" + (String)iter2.next());
                    if (alias != null)
                      aliasOut.println(normalize(alias) + "|" + alias + "|" +
                                     geneid);
                   }
                 }
              }
//            System.out.println("|");
            }
            }
            text = new StringBuffer();
            System.out.println("|Finished.");
        }
        else
          text.append(inputLine);
      }
      symbolOut.flush();
      aliasOut.flush();
      symbolOut.close();
      aliasOut.close();
      in.close();
    }
    catch (Exception e) {
      e.printStackTrace();
      e.getMessage();
    }

  }

  public String normalize(String x)
  {
    return x.toLowerCase().replaceAll("[^a-z0-9]+","");
  }

  public static String getOption(char flag, String[] args)
  {
    for (int i = 0; i < args.length; i++) {
          if ((args[i].length() > 0) && (args[i].charAt(0) == '-')) {
                  if (args[i].charAt(1) == flag) {
                          if (i + 1 == args.length)
                                  return "";
                          return args[i + 1];
                  }
          }
        }
        return "";

  }

  public static void main(String[] args) {
    if (args.length < 6) {
    System.out.println("Arguments: -f [XML File Name]");
    System.out.println("           -s [Symbol Output File Name]");
    System.out.println("           -a [Alias Output File Name]");
    System.exit(0);
  }

  inFile = getOption('f',args);
  symbolFile = getOption('s',args);
  aliasFile = getOption('a',args);

  ParseEntrezGene parser = new ParseEntrezGene();
  parser.parse();
  }

}
