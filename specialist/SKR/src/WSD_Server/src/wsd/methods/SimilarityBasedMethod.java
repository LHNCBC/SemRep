
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

import java.util.*;
import java.io.*;

import org.apache.log4j.Logger;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.Namespace;

import wsd.model.*;
import wsd.util.*;
import wsd.WSDEnvironment;
import gov.nih.nlm.nls.utils.StringUtils;
import java.text.DecimalFormat;

/**
 * Describe class SimilarityBasedMethod here.
 *
 *
 * Created: Fri Aug 25 11:30:13 2006
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class SimilarityBasedMethod implements DisambiguationMethod 
{

  /** string containing known punctuation */
  public static final String PUNCTUATION = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
  /** string containing known whitespace */
  public static final String WHITESPACE = " \t\n\r\f";
  /** index of affinity vectors by sense (umls concept) */
  public static InvertedFile avInvFile = null;
  /** cui -> preferred form index */
  public static InvertedFile cuiPpfInvFile = null;
  /** preferred form -> cui index */
  public static InvertedFile strCuiInvFile = null;
  /** word frequency index */
  public static IntBinSearchPool wordFreqIndex = null;

  /** Logger for this class */
  private static Logger logger = Logger.getLogger(SimilarityBasedMethod.class);
  /** set format for scores in test method */
  private static DecimalFormat scoreFormat = new DecimalFormat("0.0000");

  static {
    logger.info("initializing Similarity Method...");
    logger.info("SimilarityBasedMethod:class init: avinvFile:" + avInvFile);        
    try {
      if (avInvFile == null)
        avInvFile = InvertedFile.getInstance
          (WSDEnvironment.properties.getProperty("DISAMB_SERVER_SBWSD_AFFVEC_INDEXNAME","av.index"),
           WSDEnvironment.properties.getProperty("DISAMB_SERVER_SBWSD_AFFVEC_INDEXPATH"));
      logger.info("SimilarityBasedMethod:class init: avinvFile:" + avInvFile);        
      if (wordFreqIndex == null) {
        IntIndex wordFreqIndexWrapper = IntIndex.getInstance
          (WSDEnvironment.properties.getProperty("DISAMB_SERVER_SBWSD_WORDFREQ_INDEXPATH","wordfreq.index"));
        wordFreqIndex = wordFreqIndexWrapper.getIntBinSearchPool();
      }
      if (cuiPpfInvFile == null)
        cuiPpfInvFile = InvertedFile.getInstance
          (WSDEnvironment.properties.getProperty("DISAMB_SERVER_SBWSD_CUIPPF_INDEXNAME","cui_ppf.index"),
           WSDEnvironment.properties.getProperty("DISAMB_SERVER_SBWSD_CUIPPF_INDEXPATH"));
      if (strCuiInvFile == null)
        strCuiInvFile = InvertedFile.getInstance
          (WSDEnvironment.properties.getProperty("DISAMB_SERVER_SBWSD_STRCUI_INDEXNAME","str_cui.index"),
           WSDEnvironment.properties.getProperty("DISAMB_SERVER_SBWSD_STRCUI_INDEXPATH"));
      Map globalStats = loadGlobalStats();
      if (globalStats.containsKey("sentences"))
        no_sentences = Integer.parseInt((String)globalStats.get("sentences"));
      if (globalStats.containsKey("words"))
        no_words = Integer.parseInt((String)globalStats.get("words"));
    } catch (FileNotFoundException e) {
      logger.error(e.getMessage());
    } catch (IOException e) {
      logger.error(e.getMessage());
    } catch (ClassNotFoundException e) {
      logger.error(e.getMessage());
    }
      logger.info("SimilarityBasedMethod:class init: SBWSD indices initialized");
  logger.info("SimilarityBasedMethod:class init: avinvFile:" + avInvFile);
  }

  public static int no_sentences = 0;
  public static int no_words = 0;

  /**
   * Creates a new <code>SimilarityBasedMethod</code> instance.
   *
   */
  public SimilarityBasedMethod() {
  }

  // Implementation of wsd.methods.DisambiguationMethod

  /**
   * Describe <code>getMatch</code> method here.
   *
   * @param document a DOM tree that represents the MetaMap machine output that
   *                 contains some ambiguity instances.
   * @return a List that contains the best matches found by this particular
   *         disambiguation method.
   */
  public final List getMatch(final Document document)
  {
    List jdResults = new Vector();
    Element root = document.getRootElement();
    Namespace ns = root.getNamespace();

    logger.info("Disambiguating using SBWSD Method.");
    //get the utterance list
    List utteranceList = root.getChildren("utterance", ns);
    ListIterator utteranceIterator = utteranceList.listIterator();
    while (utteranceIterator.hasNext())
      {
	Element utteranceNode = (Element)utteranceIterator.next();
	Utterance utterance = new Utterance(utteranceNode,ns);

	//get the noun phrase list
	List phraseList = utteranceNode.getChildren("phrase",ns);
	ListIterator phraseIterator = phraseList.listIterator();
	while (phraseIterator.hasNext())
          {
	    Element phraseNode = (Element)phraseIterator.next();
	    NounPhrase nounPhrase = new NounPhrase(phraseNode,ns);
	    if (phraseNode.hasChildren())
              {
		//get the ambiguity list
		Element ambiguitiesNode = phraseNode.getChild("ambiguities",ns);
		List ambiguityList = ambiguitiesNode.getChildren("ambiguity",ns);
		ListIterator ambiguityIterator = ambiguityList.listIterator();
		while (ambiguityIterator.hasNext())
                  {
		    Element ambiguityNode = (Element)ambiguityIterator.next();
		    Ambiguity ambiguity = new Ambiguity(ambiguityNode,ns);
		    //if the ambiguity is marked to be "process"ed, process it
		    //otherwise skip.
		    if (ambiguity.getNeedProcessing())
                      {
			List candidateList = ambiguityNode.getChildren("candidate",ns);
			ListIterator candidateIterator = candidateList.listIterator();
			PreferredNameVector prefNames = new PreferredNameVector();
			List candidates = new Vector();
			Candidate candidate = new Candidate();
			while (candidateIterator.hasNext())
                          {
			    Element candidateNode = (Element)candidateIterator.next();
			    candidate = new Candidate(candidateNode,ns);
			    candidates.add(candidate);
			    String preferredName = candidate.getPreferredConceptName();
			    prefNames.add(preferredName);
                          }
			List context = getContext(document,utterance,candidate.getMatchedWords());
			List bestPrefNames = this.getMatch(utterance,null,context,candidates);
			//create the Result object that stores the ambiguity result data
			Result res = new Result();
			res.setCandidatePreferredConceptNames(prefNames);
			res.setPreferredConceptNames(bestPrefNames);
			res.setUi(utterance.getUi());
			res.setUtterancePos(utterance.getPos());
			res.setPhrasePos(nounPhrase.getPos());
			jdResults.add(res);
			if (logger.isDebugEnabled())
			  logger.debug("Result: " + res.getUi() + "|" +
				       res.getUtterancePos() + "|" +
				       res.getPhrasePos() + "|" +
				       res.getCandidatePreferredConceptNames() + "|" +
				       res.getPreferredConceptNames());
                      }
                  }
              }
          }
      }
    logger.info("Completed disambiguation using SBWSD Method.");
    return jdResults;
  }


  private List getContext(Document doc, Utterance currUtterance, String wordlist)
  {
      List context = new Vector();
      List words = new Vector();
      String currSentence = currUtterance.getSentence();
      StringTokenizer tokenizer = new StringTokenizer(wordlist,",");
      while (tokenizer.hasMoreTokens())
        words.add(((String)tokenizer.nextToken()).toLowerCase());
      Element root = doc.getRootElement();
      Namespace ns = root.getNamespace();

      if (logger.isInfoEnabled())
        logger.info("Computing context.");
      //get the utterance list
      List utteranceList = root.getChildren("utterance", ns);
      ListIterator utteranceIterator = utteranceList.listIterator();
      while (utteranceIterator.hasNext())
        {
          Element utteranceNode = (Element)utteranceIterator.next();
 	  Utterance utterance = new Utterance(utteranceNode,ns);
          context.add(utterance.getSentence());
          if (logger.isDebugEnabled())
            logger.debug("added to context: " + utterance.getSentence());
        }
      if (logger.isDebugEnabled())
        logger.debug("The context is:" + context.toString());
      return context;
  }


  /**
   * Calls the Similarity Based Method and finds the best match in the
   * list of concepts.
   *
   * @param utterance   The sentence or fragment containing the ambiguity.
   * @param phrases     The phrases in the utterance.
   * @param context     the context of the ambiguity. Initial thought was to have
   *                    several sentences associated with the ambiguity as the context.
   * @param candidates  List of WordSenses of the concepts that cause the ambiguity
   *
   * @return a Result object that represents the best match from SBWSD method
   * @see wsd.model.WordSense
   */
  public List getMatch(Utterance utterance, List phrases, List context, List candidates)
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(utterance.getSentence().replaceAll("\"","")).append(" ");
    Iterator contextIter = context.iterator();
    while (contextIter.hasNext()) 
      sbuf.append(contextIter.next()).append(" ");

    List matchedWordsList = new ArrayList();
    List conceptList = new ArrayList();
    Iterator candidateIterator = candidates.iterator();
    while (candidateIterator.hasNext()) {
      Candidate candidate = (Candidate)candidateIterator.next();
      conceptList.add(candidate.getUmlsConcept());
      matchedWordsList.add(candidate.getMatchedWords());
    }
    /* using the set of concept ids from mapped word senses */
    String resultConceptName = this.disambiguate(matchedWordsList, sbuf.toString(), conceptList);

    List resultConcepts = new ArrayList();
    if (resultConceptName == null) {
      resultConcepts.add("[SBWSD unable to disambiguate input]");
    } else {
      boolean done = false;    if (resultConceptName != null) {
      for (int j=0; j < candidates.size() && done == false; j++)
	{
	  if (((Candidate)candidates.get(j)).getPreferredConceptName().equals(resultConceptName))
	    {
	      resultConcepts.add(((Candidate)candidates.get(j)).getPreferredConceptName());
	      if (logger.isDebugEnabled())
		logger.debug(utterance.getUi() + 
			     ":, " + resultConceptName + 
			     ", added: " + candidates.get(j) + ", " + done);
	      done = true;	// only add one candidate that matches concept name, drop the others.
	      break;
	    }
	}
      }
    }
    return resultConcepts;
  }

  /**
   * Get preferred name for Concept.
   *
   * @param cui concept unique identifier
   * @return preferred name of concept
   */
  public String getPreferredName(String cui)
  {
    // table format:  @see wsd.util.DictionaryEntry
    try {
      BSPTuple result = cuiPpfInvFile.lookup(cui);
      List list = (List)result.getValue();
      Iterator j = list.iterator();
      while (j.hasNext()) {
        List toks = StringUtils.split((String)j.next(), "|");
        if (toks.size() > 1) {
          return (String)toks.get(1);
        }
      }
    } catch (FileNotFoundException e) {
      logger.error("getCui: " + e.getMessage());
    } catch (IOException e) {
      logger.error("getCui: " + e.getMessage());
    }
    return null;
  }


  /**
   * Get CUI for concept using concept's preferred name.
   *
   * @param conceptString preferred name of concept (STR)
   * @return concept unique identifier (CUI)
   */
  public String getCui(String conceptString)
  {
    // table format:  @see wsd.util.DictionaryEntry

    try {
      BSPTuple result = strCuiInvFile.lookup(conceptString);
      List list = (List)result.getValue();
      Iterator j = list.iterator();
      while (j.hasNext()) {
        List toks = StringUtils.split((String)j.next(), "|");
        if (toks.size() > 1) {
          return (String)toks.get(1);
        }
      }
    } catch (FileNotFoundException e) {
      logger.error("getCui: " + e.getMessage());
    } catch (IOException e) {
      logger.error("getCui: " + e.getMessage());
    }
    return null;
  }


  /** 
   * @param targetTerm target term  
   * @return map of affinity vectors for term keyed by sense (or UMLS concept)
   */
  public static Map getAffinityVectors(String targetTerm)
  {
    Map avmap = new HashMap();
    try {
      logger.debug("targetTerm: " + targetTerm);
      logger.debug("avInvFile: " + avInvFile);
      //      if (avInvFile == null) {
      //        initializeDisambiguationMethod();        
      //      }
      BSPTuple result = avInvFile.lookup(targetTerm);
      List list = (List)result.getValue();
      Iterator j = list.iterator();
      while (j.hasNext()) {
        Map affvector = new HashMap();
	List toks = StringUtils.split((String)j.next(), "|");
	Iterator tokenIter = toks.iterator();
	String term = (String)tokenIter.next();
	String sense = (String)tokenIter.next();
	while (tokenIter.hasNext()) {
	  List pair = StringUtils.split((String)tokenIter.next(), ":");
	  if (pair.size() > 1               ) {
	    affvector.put(pair.get(0), Double.valueOf((String)pair.get(1)));
	  }
	}
        // System.out.print("getAffinityVectors: affvector: ");
        // printVector(affvector);
	avmap.put(sense, affvector);
      }
    } catch (FileNotFoundException e) {
      System.err.println("Error: getAffinityVectors: error opening index file.");
      e.printStackTrace(System.err);
    } catch (IOException e) {
      System.err.println("Error: getAffinityVectors: error opening index file.");
      e.printStackTrace(System.err);
    }
    return avmap;
  }

  class ResultTuple implements Comparable {
    Double score;
    String conceptCui;
    public ResultTuple(Double score, String name)
    {
      this.score = score;
      this.conceptCui = name;
    }
    public int compareTo(Object o)
    {
      return this.score.compareTo(((ResultTuple)o).score);
    }
  }

  /// should disambiguate only provide one term?

  /** 
   * @param terms targetterms
   * @param context text context to used with evaluating concepts.
   * @param conceptList list of concepts to be considered.
   * @return concept with highest score in conceptList.
   */
  public String disambiguate(List targetTerms, String context, List conceptList)
  {
    Object[] rt;
    System.out.println("terms: " + targetTerms);
    logger.debug("terms: " + targetTerms);
    logger.debug("conceptList: " + conceptList);
    if (targetTerms != null) {
      Iterator termIterator = targetTerms.iterator();
      if (termIterator.hasNext()) {
        rt = disambiguateTerm((String)termIterator.next(), context, conceptList);
        return getPreferredName(((ResultTuple)rt[0]).conceptCui);
      } 
    }
    return null;
  }

  public Object[] disambiguateTerm(String targetTerm, String context, List conceptList)
  {
    List resultList = new ArrayList();
    List stokens = new ArrayList();
    StringTokenizer tokenizer = new StringTokenizer(context, PUNCTUATION + WHITESPACE);
    while (tokenizer.hasMoreTokens())
      stokens.add(((String)tokenizer.nextToken()).toLowerCase());

    Iterator conceptIterator = conceptList.iterator();
    while (conceptIterator.hasNext()) {
      String concept = (String)conceptIterator.next();
      logger.debug("concept: " + concept);
      String cui = getCui(concept);
      logger.debug("cui: " + cui);
      Map affvectors = getAffinityVectors(targetTerm);
      resultList.add(new ResultTuple(new Double(this.newsentence_cluster_sim(affvectors, stokens, cui)), cui));
    }
    Object[] resultArray = resultList.toArray();
    Arrays.sort(resultArray);
    return resultArray; 
  }

  public String displayDisambiguation(List targetTerms, String context, List conceptList)
  {
    Object[] rt;
    Iterator termIterator = targetTerms.iterator();
    if (termIterator.hasNext()) {
      rt = disambiguateTerm((String)termIterator.next(), context, conceptList);
      for (int i=0; i<rt.length; i++) {
        System.out.println(((ResultTuple)rt[i]).conceptCui + ": " + ((ResultTuple)rt[i]).score);
      }
      return getPreferredName(((ResultTuple)rt[0]).conceptCui);
    }
    return null;
  }

  public double newsentence_cluster_sim(Map affvectors, List stokens, String concept)
  {
    double sum=0.0;
    Iterator tokenIterator = stokens.iterator();
    while (tokenIterator.hasNext()) {
      String word = (String)tokenIterator.next();
      sum += this.cluster_ws_weight(word, stokens)*this.cluster_word_aff(word,affvectors,concept);
    }
    return sum;
  }

  public static void printVector(Map affvector)
  {
    Iterator keyIter = affvector.keySet().iterator();
    while (keyIter.hasNext()) {
      Object key = keyIter.next();
      System.out.print(key + ":" + affvector.get(key) + ", ");
    }
  }

  public double cluster_word_aff(String word, Map affvectors, String sense)
  {
    // System.out.println("sense: " + sense);
    //printVector(affvectors);
    if (affvectors.containsKey(sense)) {
      Map vector = (Map)affvectors.get(sense);
      // printVector(vector);
      if (vector.containsKey(word)) {
        // System.out.println("cluster_ws_aff of " + word + ", sense: " + sense +" -> " + ((Double)vector.get(word)).doubleValue());
	return ((Double)vector.get(word)).doubleValue();
      }
    }
    //    System.out.println("cluster_ws_aff of " + word + ", sense: " + sense +" -> " + 0.0);
    return 0.0;
  }

  public double cluster_ws_weight(String w, List stokens)
  {
    double sum = 0.0;
    Iterator tokenIterator = stokens.iterator();
    while (tokenIterator.hasNext()) {
      String wprime = (String)tokenIterator.next();
      sum += this.cluster_ws_factor(wprime,stokens);
    }
    // System.out.println("cluster_ws_weight of " + w + " -> " + this.cluster_ws_factor(w, stokens)/sum);
    return this.cluster_ws_factor(w, stokens)/sum;
  }

  public double cluster_ws_factor(String w, List stokens)
  {
    double result = 1.0/(this.w_probability(w)*(double)stokens.size());
    // System.out.println("cluster_ws_factor of " + w + " -> " + result);
    return result;
  }
  
  public double w_probability(String w)
  {
    int sfreq = this.word_freq(w); /* number of sentences word occurs in: float(len(self.wsmrd[w])) */
    // System.out.println("probability of " + w + " -> " + sfreq/(double)sentence_range());
    return sfreq/(double)sentence_range(); /* number of sentences */
  }

  public int word_freq(String word)
  {
    // format of wordfreq table: see IntBinSearchMap
    try {
      int freq = wordFreqIndex.get(word);
      if (freq == -1)
        return 1;
      // System.out.println("frequencey of " + word + " -> " + freq);
      return freq;
    } catch (FileNotFoundException e) {
      e.printStackTrace(System.err);
    } catch (ClassNotFoundException e) {
      e.printStackTrace(System.err);
    } catch (IOException e) {
      e.printStackTrace(System.err);
    } catch (RuntimeException e) {
      System.err.println("Exception when retrieving term " + word);
      e.printStackTrace(System.err);
    } catch (Exception e) {
      System.err.println("RuntimeException when retrieving term " + word);
      e.printStackTrace(System.err);
    }
    return 0;  /* lookup in wordfreq.table */
  }

  public int sentence_range()
  {
    return this.no_sentences;
  }

  public static Map loadGlobalStats()
    throws FileNotFoundException, IOException
  {
    Map map = new HashMap();
    String line;
    BufferedReader br = new BufferedReader(new FileReader("globalstats.txt"));
    while ((line = br.readLine()) != null) {
      List tokens = StringUtils.split(line, "|");
      map.put(tokens.get(0), tokens.get(1));
    } 
    return map;
  }

 public final static void main(String[] args)
  {
    if (args.length > 2) {
      String term = args[0];
      // read senses
      List senses = StringUtils.split(args[1], ",");
      // read query
      StringBuffer query = new StringBuffer();
      for (int i = 2; i < args.length; i++) {
        query.append(args[i]).append(" ");
      }
      List terms = new ArrayList();
      terms.add(term);
      SimilarityBasedMethod sbm = new SimilarityBasedMethod();
      String resultConceptName = sbm.disambiguate(terms, query.toString(), senses);
      System.out.println("result: " + resultConceptName);
      
      sbm.displayDisambiguation(terms, query.toString(), senses);

    } else {
      System.err.println("usage: java wsd.util.SimilarityBasedMethod term senses query");
      System.err.println("  senses are of the form: sense,sense.. (with no spaces)");
      System.exit(1);
    }
  }
}

