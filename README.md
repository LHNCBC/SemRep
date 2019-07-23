# Installing and Running the Public Version of SemRep

## Introduction

SemRep is a program that extracts semantic predications (subject-relation-object triples) from biomedical free text. For a detailed description of SemRep, see the article [The interaction of domain knowledge and linguistic structure in natural language processing: interpreting hypernymic propositions in biomedical text](https://www.sciencedirect.com/science/article/pii/S1532046403001175).  This guide is designed to help install SemRep on the Linux operating system.

## Prerequisites

### SemRep Terms and Conditions and UMLS License

_Important_: Users are responsible for compliance with the [SemRep Terms and Conditions](SemRepTermsConditions.txt).

To use this application, you must have signed the [UMLS agreement](https://uts.nlm.nih.gov/license.html). The UMLS agreement requires those who use the [UMLS](https://www.nlm.nih.gov/research/umls/) to file a brief report once a year to summarize their use of the UMLS. It also requires the acknowledgment that the UMLS contains copyrighted
material and that those copyright restrictions be respected. The UMLS agreement requires users to agree to obtain agreements for EACH copyrighted source prior to its use within a commercial or production application.

### Platform Prerequisites

SemRep relies on MetaMap. Therefore, the platform prerequisites for MetaMap  also apply to SemRep. The public version of MetaMap should be downloaded from the [MetaMap portal](https://metamap.nlm.nih.gov), then installed and tested before proceeding to SemRep installation. The current SemRep release is compatible with MetaMap 2016v2.  Note also that SemRep, by default, uses the Word Sense Disambiguation (WSD) Server, optionally  installed with MetaMap. Therefore, SemRep users should take into consideration WSD Server platform prerequisites when installing MetaMap.

It is recommended that this SemRep distribution be installed alongside the public MetaMap distribution, i.e., they have the same parent directory.

### Space Requirements

In addition to MetaMap space requirements, the current SemRep release requires about 60GB disk space. This assumes that the entire SemRep data is installed. The data files that take up the most space are UMLS data (Lexicon and Metathesaurus) files (approx. 42GB) and UMLS hierarchy files (approx. 15.5GB), including files for 2006AA and 2018AA UMLS releases. If you are only interested in a specific UMLS release and/or specific MetaMap mode (relaxed vs. strict), space requirements are much lower. See installation instructions below (Step 2) to configure SemRep to run with only a specific UMLS release. The following are the approximate data file sizes for UMLS releases available  with this distribution:

- 2006AA (23G)
- 2018AA (34G)

If you do not intend to use relaxed mode option (-M), the requirements are as follows:

- 2006AA (16.6G)
- 2018AA (15.8G)

By default, SemRep uses and is optimized for 2006AA release. UMLS data and hierarchy files are provided separately.


## Getting UMLS Data Files

The UMLS Lexicon, Metathesaurus and hierarchy data files can be downloaded from [ftp://lhcftp.nlm.nih.gov/Open-Access-Datasets/SKR/SemRep/dist](ftp://lhcftp.nlm.nih.gov/Open-Access-Datasets/SKR/SemRep/dist). 
 
Use the following command to extract the UMLS SPECIALIST Lexicon and Metathesaurus. For the current release, available years are 06 and 18. You can download and use both, or select an appropriate year. 

`% tar xvfj public_semrep_v1.8_lex_db_{year}_{os}.tar.bz2`

Use the following command to extract the UMLS hierarchy files. Again, you can download for both 06 and 12, or select an appropriate year. 

`% tar xvfj public_semrep_v1.8_hier_{year}_{os}.tar.bz2`

## Installation

SemRep installation is similar to MetaMap installation, so much of the material below has been adapted from MetaMap installation instructions.

### Step 1

To begin the initial install, go to the top-level SemRep directory.  You can speed up the process by telling the install program where your Java installation is by setting the environment variable JAVA_HOME to the Java installation directory.  If you don't set the variable the program will prompt you for the information.

To find out where your java installation is located type the following at command line:

`% which java`

To set the environment variable JAVA_HOME:

If the command above returns /usr/local/jre1.6.0/bin/java, then JAVA_HOME should be set to /usr/local/jre1.6.0/.

For example: 
- In C Shell (csh or tcsh), use `% setenv JAVA_HOME /usr/local/jre1.6.0`.
- In Bourne Again Shell (bash), use `% export JAVA_HOME=/usr/local/jre1.6.0`.

You also need to add the bin directory to your program path (<SemRep's parent dir> refers to the directory where SemRep is located): 

- In C Shell (csh or tcsh), `% set path = ( $path <SemRep's parent dir>/SemRep/bin )`
- In Bourne Again Shell (bash), `% export PATH=$PATH:<SemRep's parent dir>/Semrep/bin`


### Step 2

Run the installation script as follows:

NOTE: If you are only interested in using a single UMLS dataset (2006, or 2018), you can  save on space requirements by only downloading and installing the relevant dataset and 
changing the line in install.sh 

`AVAILABLE_DATA="2006 2018"`

to 

`AVAILABLE_DATA="2018"` (If you only want to use 2018 UMLS datasets).

````
% cd SemRep
% ./bin/install.sh
````

An example of a successful installation looks as follows:

````cd public_semrep
bin/install.sh

Enter basedir of SemRep installation [/rhome/kilicogluh/SemRep]

Enter basedir of MetaMap installation [/rhome/kilicogluh/public_mm]

Basedir is set to /rhome/kilicogluh/SemRep
MetaMap basedir is set to /rhome/kilicogluh/public_mm

Where does your distribution of Sun\'s JRE reside?
Enter home path of JRE (JDK) [/usr/local/jdk1.6.0_11]: 

Using /usr/local/jdk1.6.0_11 for JAVA_HOME.

Copying 2006 lexicon data files...
Copying 2018 lexicon data files...
Copying 2006 MetaMap DB data files...
Copying 2018 MetaMap DB data files....
Setting up bin directory scripts:
/rhome/kilicogluh/SemRep/bin/SEMREPrun.v1.8 generated.
/rhome/kilicogluh/SemRep/bin/semrep.v1.8 generated.
Setting up test suite:
/rhome/kilicogluh/SemRep/TestSuite/runTest_v1.8.sh generated.
Checking for required datafiles
Checking for required MetaMap datafiles
/rhome/kilicogluh/public_mm/lexicon/morph/dm_translated_facts
/rhome/kilicogluh/public_mm/lexicon/morph/lm_translated_rules
/rhome/kilicogluh/public_mm/lexicon/morph/dm_translated_rules
/rhome/kilicogluh/public_mm/lexicon/data/lexiconStatic2006IndByInfl.dbx
/rhome/kilicogluh/public_mm/lexicon/data/lexiconStatic2006IndByEui.dbx
/rhome/kilicogluh/public_mm/lexicon/data/lexiconStatic2006
/rhome/kilicogluh/public_mm/DB/DB.USAbase.2006AA.base
/rhome/kilicogluh/public_mm/DB/DB.USAbase.2018AA.base
/rhome/kilicogluh/public_mm/DB/DB.USAbase.2006AA.strict
/rhome/kilicogluh/public_mm/DB/DB.USAbase.2018AA.strict
/rhome/kilicogluh/public_mm/DB/DB.USAbase.2006AA.relaxed
/rhome/kilicogluh/public_mm/DB/DB.USAbase.2018AA.relaxed
/rhome/kilicogluh/public_mm/WSD_Server
/rhome/kilicogluh/public_mm/src/WSD_Server
Checking for required SemRep datafiles
hshset_aa.ad
hshset_aa.ha
hshset_aa.nm
hshset_aa.str
hshset_bani.ad
hshset_bani.ha
hshset_bani.nm
hshset_bani.str
hshset_bann.ad
hshset_bann.ha
hshset_bann.nm
hshset_bann.str
hshset_ce.ad
hshset_ce.ha
hshset_ce.nm
hshset_ce.str
hshset_freq.ad
hshset_freq.ha
hshset_freq.nm
hshset_freq.str
hshset_g.ad
hshset_gaft.ad
hshset_gaft.ha
hshset_gaft.nm
hshset_gaft.str
hshset_gbef.ad
hshset_gbef.ha
hshset_gbef.nm
hshset_gbef.str
hshset_gen.ad
hshset_gene.ad
hshset_gene.ha
hshset_gene_lb-s1.ad
hshset_gene_lb-s1.ha
hshset_gene_lb-s1.nm
hshset_gene_lb-s1.str
hshset_gene_lb-s2.ad
hshset_gene_lb-s2.ha
hshset_gene_lb-s2.nm
hshset_gene_lb-s2.str
hshset_gene.nm
hshset_gene_rb-s1.ad
hshset_gene_rb-s1.ha
hshset_gene_rb-s1.nm
hshset_gene_rb-s1.str
hshset_gene_rb-s2.ad
hshset_gene_rb-s2.ha
hshset_gene_rb-s2.nm
hshset_gene_rb-s2.str
hshset_gene-s1.ad
hshset_gene-s1.ha
hshset_gene-s1.nm
hshset_gene-s1.str
hshset_gene-s2.ad
hshset_gene-s2.ha
hshset_gene-s2.nm
hshset_gene-s2.str
hshset_gene.str
hshset_gen.ha
hshset_gen.nm
hshset_gen.str
hshset_g.ha
hshset_gin.ad
hshset_gin.ha
hshset_gin.nm
hshset_gin.str
hshset_g.nm
hshset_g.str
hshset_gt.ad
hshset_gt.ha
hshset_gt.nm
hshset_gt.str
hshset_j.ad
hshset_j.ha
hshset_j.nm
hshset_j.str
hshset_lj.ad
hshset_lj.ha
hshset_lj.nm
hshset_lj.str
hshset_lt.ad
hshset_lt.ha
hshset_lt.nm
hshset_lt.str
hshset_mg.ad
hshset_mg.ha
hshset_mg.nm
hshset_mg.str
hshset_multi.ad
hshset_multi.ha
hshset_multi.nm
hshset_multi.str
hshset_ng.ad
hshset_ng.ha
hshset_ng.nm
hshset_ng.str
hshset_org.ad
hshset_org.ha
hshset_org.nm
hshset_org.str
hshset_re.ad
hshset_re.ha
hshset_re.nm
hshset_re.str
hshset_single.ad
hshset_single.ha
hshset_single.nm
hshset_single.str
hshset_stop.ad
hshset_stop.ha
hshset_stop.nm
hshset_stop.str
lexset_gene.bit
lexset_gene.nm
lexset_gene.pt
linset_geneterms-hsh.ad
linset_geneterms-hsh.ha
linset_geneterms-hsh.nm
linset_geneterms-hsh.str
linset_geneterms.wt
postbrillset_gene.aa
postbrillset_gene.bani
postbrillset_gene.bann
postbrillset_gene.ce
postbrillset_gene.freq
postbrillset_gene.g
postbrillset_gene.gaft
postbrillset_gene.gbef
postbrillset_gene.gen
postbrillset_gene.gin
postbrillset_gene.gt
postbrillset_gene.j
postbrillset_gene.lj
postbrillset_gene.lt
postbrillset_gene.mg
postbrillset_gene.mng
postbrillset_gene.multi
postbrillset_gene.ng
postbrillset_gene.org
postbrillset_gene.re
postbrillset_gene.rstr_fn
postbrillset_gene.rstr_fp
postbrillset_gene.single
postbrillset_gene.stop
relatea_gene_lb.ad
relatea_gene_lb.map
relatea_gene_lb.nm
relatea_gene_rb.ad
relatea_gene_rb.map
relatea_gene_rb.nm
tagset_gene.b
tagset_gene.cr
tagset_gene.l
tagset_gene.lr
wordset_gene.stop
DB
/rhome/kilicogluh/public_semrep/DATA/ENTREZGENE/HUMAN_SYMBOL
/rhome/kilicogluh/public_semrep/DATA/ENTREZGENE/HUMAN_ALIAS
/rhome/kilicogluh/public_semrep/DATA/UMLS_HIERARCHY/hrel-UMLS_btree.2006AA_1
/rhome/kilicogluh/public_semrep/DATA/UMLS_HIERARCHY/hrel-UMLS_btree.2006AA_2
/rhome/kilicogluh/public_semrep/DATA/UMLS_HIERARCHY/hrel-UMLS_btree.2006AA_3
/rhome/kilicogluh/public_semrep/DATA/UMLS_HIERARCHY/hrel-UMLS_btree.2006AA_3
/rhome/kilicogluh/public_semrep/DATA/UMLS_HIERARCHY/hrel-UMLS_btree.2006AA_4
/rhome/kilicogluh/public_semrep/DATA/UMLS_HIERARCHY/hrel-UMLS_btree.2018AA_1
./bin/install.sh ended Fri Feb 15 17:28:33 EST 2018
Public MetaMap basedir: /rhome/kilicogluh/public_mm
Public SemRep basedir: /rhome/kilicogluh/SemRep
Java Home dir: /usr/local/jdk1.6.0_11
````


## Starting SemRep

SemRep requires that two servers associated with MetaMap, SKR-MedPost Part-of-Speec Tagger and Word Sense Disambiguation (WSD) Servers, be already running. For more details 
on starting and stopping these processes, see [MetaMap README documentation](http://metamap.nlm.nih.gov).

### Determining whether the servers are running

You can determine if the server are running by the command:

````
% ps ax | grep java
````

The output should look something like this:

````
16105 pts/0    Sl     0:00 /usr/local/jdk1.6.0_01/bin/java -Xmx2g -Dserver.config...
16158 pts/0    R+     0:00 grep java
21914 pts/0    Sl     0:02 /usr/bin/gij -Djava.version=1.6.0 -Djava.home=/usr/lib/...
```


#### Invoking SemRep

If there are no errors starting the WSD and Tagger servers then SemRep can be run as follows:

````
% ./bin/semrep.v1.8
````

This command will run SemRep with the 2006 UMLS dataset. To run SemRep with the 2018 dataset, 
use the following command:

````
% ./bin/semrep.v1.8 -L 2018 -Z 2018AA
````

Other SemRep options are explained [elsewhere](doc/SemRep.v1.8_Options.txt).

### Testing SemRep

SemRep distribution comes with a test suite to ensure that the results obtained with the distribution matches those obtained with our internal version. To test SemRep, go to TestSuite directory under SemRep, and run the following script: 

````
% ./runTest_v1.8.sh 
````

This script will run the SemRep program with several different option combinations, compare the  results with the results of our internal version and display the differences, if any. 
Otherwise, the script will simply print out DIFFS OK messages. 

### Questions

If you have problems then send an email to [halil.kilicoglu@gmail.com](mailto:halil.kilicoglu@gmail.com).

### Using SemRep

For more information on running SemRep and its options, see these references:
- [Semrep v1.8 options](doc/SemRep.v1.8_Options.txt)
- [Full-fielded output format description](doc/SemRep_full_fielded_output.pdf)
- [XML output description](doc/SemRep.v1.8_XML_output_desc.txt)

