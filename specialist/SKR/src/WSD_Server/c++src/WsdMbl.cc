// wsd_mbl
//
// given: utterance+context+st candidates
//
// pseudo code:
//   convert utterance+context to features
//   classify features using the classifier for the specified ambiguity
//     result: a set tuples containing: semantic type and distance.
//   filter tuples with semantic type not contained in ST candid list.
//   sort tuples by distance
// return semantic type of highest ranked tuple
//

#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>
#include <list>
#include <map>
#include "Tokenize.h"
#include "ListUtils.h"

#include "WsdMbl.h"
#include "wsd_methods_MblDisambiguator.h"
#include "TimblAPI.h"
#include <jni.h>

using namespace std;

// the lab
#define WORKDIR "/net/nls10/export/home/wrogers/WSD/studio"

// reviewed results directory
#define REVIEWDIR  WORKDIR "/Reviewed_Results"

// classifier parent directory
#define CLASDIR  WORKDIR "/MBL";

#define NUMBER_OF_KEYWORDS "50"

string classifier_path = CLASDIR;
string reviewed_path = REVIEWDIR;
map<string, int> cite_freq;
map<string, int> cite_bigr_freq;

int cnt = 0;
map<string, string> loadChoices(string ambiguity);
string convert_to_features(string utterance, string context);
list<string> make_bigrams(string str,
			  map<string, int>& freq, map<string, int>& bigr_freq);

string make_features(string ambiguity, list<string> bigrams);
string classify(string utterance, string features);
string get_semtype_from_class (string ambiguity, string result);

template<class Container>
void print(Container& c, char* comment = "") {
  cout << "\n" << comment << ":\n";
  typename Container::iterator it;
  for (it = c.begin(); it !=  c.end(); it++)
    cout << *it << " ";
  cout << endl;
}

string
listToString(list<string> alist)
{
  string str("{ ");
  for (list<string>::iterator it = alist.begin(); it != alist.end(); ++it) {
    str.append("\"");
    str.append(*it);
    str.append("\" ");
  }
  str.append("}\n");
  return str;
}

// Java Native Interface 
/*
 * Class:     wsd_methods_MblDisambiguator
 * Method:    setEnvironment
 * Signature: (Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_wsd_methods_MblDisambiguator_setEnvironment
  (JNIEnv *env, jobject obj, jstring classifierDir, jstring reviewedDir)
{
  // classifier_path.assign(env->GetStringUTFChars(classifierDir, 0));
  // reviewed_path.assign(env->GetStringUTFChars(reviewedDir, 0));
}


// Java Native Interface 
/*
 * Class:     wsd_methods_MblDisambiguator
 * Method:    getAnswer
 * Signature: (Ljava/lang/String;[Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_wsd_methods_MblDisambiguator_getAnswer
  (JNIEnv *env, jobject obj, 
   jstring ambiguity, jobjectArray context, jobjectArray semTypes)
{ 
  string classifier;
  string contextBody;
  classifier.assign(env->GetStringUTFChars(ambiguity, 0));
  unsigned int pos = classifier.find(",");
  if (pos != string::npos) {
    classifier.replace(pos,1,"_");
  }
  jsize len = env->GetArrayLength(context);
  for (int i = 0; i < len; i++) {
    jstring jstr = (jstring)env->GetObjectArrayElement(context, i);
    contextBody.append(env->GetStringUTFChars(jstr, 0));
  }
  /* current not used --
  len = env->GetArrayLength(semTypes);
  string st_cands[len];
  for (int i = 0; i < len; i++) {
    jstring jstr = (jstring)env->GetObjectArrayElement(semTypes, i);
    st_cands[i] = env->GetStringUTFChars(jstr, 0);
    }*/
  cout << "JNI:WsdMbl:ambiguity classifier: " << classifier << "\n";
  cout << "JNI:WsdMbl:context: \"" << contextBody << "\"\n"; 
  string features = convert_to_features(classifier, contextBody);
  cout << "JNI:WsdMbl:features converted, classifying" << "\n";
  string semtype = classify(classifier, features);
  cout << "JNI:WsdMbl:getAnswer: semtype: " << semtype << "\n";
  return env->NewStringUTF(semtype.data());
}

string 
getMatch(string classifier, string context, string st_candidates[])
{
  cout << "classifier: " << classifier << "\n";
  cout << "context: " << context << "\n";
  string features = convert_to_features(classifier, context);
  cout << "features converted" << "\n";
  string semtype = classify(classifier, features);
  return semtype;
}

string
classify(string ambiguity, string features)
{ 
  string result;
  double distance;
  string features_cit = classifier_path; 
  features_cit.append("/");
  features_cit.append(ambiguity);
  features_cit.append("/features_citation.");
  features_cit.append(NUMBER_OF_KEYWORDS);

  /** the two line below should be in the initialization. */  
  TimblAPI *My_Experiment = new TimblAPI("", "wsd");
  My_Experiment->SetOptions( "-v s" );
  My_Experiment->Learn(features_cit.data());

  //  cout << "features: " << features.data() << "\n";
  My_Experiment->Classify(features, result, distance);
  //  cout << "result: " << result << "\n";
  return get_semtype_from_class (ambiguity, result);
}

string 
convert_to_features(string classifier, string context)
{
  list<string> cite_bigrams = 
    make_bigrams(context, cite_freq, cite_bigr_freq);
  return make_features(classifier, cite_bigrams);
}


// make_bigrams
//
// params:
//   str       - string
//   frq       - frequency
//   bigr_frq  - bigram frequency
//
list<string>
make_bigrams(string str, map<string, int>& freq, map<string, int>& bigr_freq)
{
  list<string> tokens = tokenize( str );
  for (list<string>::iterator it = tokens.begin(); it != tokens.end(); ++it) {
    freq[*it]++;
  }
  list<string> bigrams;
  string tok = tokens.front();
  for (list<string>::iterator it = tokens.begin(); it != tokens.end(); ++it) {
    string bigram = tok + " " + *it; tok = *it;
    bigr_freq[bigram]++;
    bigrams.push_back(bigram);	
  }
  return bigrams;
}

//
// using bigram version of citation, generate features suitable for
// use by Timbl.
//
string
make_features(string ambiguity, list<string> bigrams)
{
  int bin = -1;
  string ambiguitydir = CLASDIR; 
  ambiguitydir.append("/");
  ambiguitydir.append(ambiguity);

  // open keyword file
  list<string> keywords;
  string keywordsfn(ambiguitydir);
  keywordsfn.append("/keywords.");
  keywordsfn.append(NUMBER_OF_KEYWORDS);
  ifstream keywordsin(keywordsfn.data());
  char buf[1024];
  while (keywordsin.getline(buf, 1024)) {
    // cout << buf << "\n";
    list<string> tokens = split(buf, '\t');
    keywords.push_back(tokens.front());
  }  
  keywordsin.close();
  //cout << "keywords : " << listToString(keywords) << "\n";

  // using bigram data, populate map
  map<string, int> bigramsMap;
  for (list<string>::iterator bgIt = bigrams.begin();
       bgIt != bigrams.end();
       ++bgIt) {
    //    cout << "bigram: " << *bgIt << "\n";
    int val = 
      bigramsMap.find(*bgIt) != bigramsMap.end() ? bigramsMap[*bgIt] : 0;
    bigramsMap[*bgIt] = bin == -1 ? 1 : val + 1;
  }

  // loop through keywords
  list<string> features_cita;
  for (list<string>::iterator kwIt = keywords.begin(); 
       kwIt != keywords.end(); 
       ++kwIt) {
    int val = bigramsMap.find(*kwIt) != bigramsMap.end() ? bigramsMap[*kwIt] : 0;
    // yikes!! this is bad; this should be using a more robust method of conversion.
    char c[2]; c[0] = val + 48; c[1] = '\0';
    string f; f.assign(c);
    features_cita.push_back(f);
  }
  return join(features_cita, ' ') + " XX";
}

map<string, string>
loadChoices(string ambiguity)
{
  map<string, string> choices;
  string choicefn = reviewed_path; 
  choicefn.append("/");
  choicefn.append(ambiguity + "/choices");

  // cout << choicefn << "\n";
  string ambiguity_class = "xx";
  string shortform = "xxxx";
  ifstream choicef(choicefn.data());
  char buf[1024];
  while (choicef.getline(buf, 1024)) {
    // get class, and short and long forms of semantic type 
    // use short form
    string line(buf);
    list<string> aList = split(line, '|');
    ambiguity_class = aList.front();
    string key = ambiguity + ambiguity_class;
    shortform = split(aList.back(), ',').front();
    // cout << "key: " << key << ", semtype: " << shortform << "\n";
    choices[ key ] = shortform;
  }
  choicef.close();
  return choices;
}

void printChoices(map<string, string> choices)
{
  typedef map<string, string>::const_iterator CI;
  for (CI p = choices.begin(); p != choices.end(); ++p) {
    cout << p->first << "\t->\t" << p->second << '\n';
  }
}

// get_semtype_from_class
//
// using assigned class determine approriate semantic type.
//
string
get_semtype_from_class (string ambiguity, string ambiguity_class)
{
  map<string, string> choices = loadChoices(ambiguity);
  // printChoices(choices);
  string key = ambiguity + ambiguity_class;
  // cout << "class: " << ambiguity_class << ", ambiguity: " << 
  // ambiguity << " => semtype: " <<  choices[key] << "\n";
  return choices[key];
}
