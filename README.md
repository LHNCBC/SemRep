*Title: Installing and Running the Public Version of SemRep
*Author: Halil Kilicoglu

*TOC

* Introduction

SemRep is a program that extracts semantic predications (subject-
relation-object triples) from biomedical free text. For a detailed
description of SemRep, see (1). 
This guide is designed to help install SemRep on the Linux operating system.

* Prerequisites

** SemRep Terms and Conditions and UMLS License

_Important_: Users are responsible for compliance with the 
[SemRep Terms and Conditions (http://semrep.nlm.nih.gov/SRTnCs.shtml)].

To use this application, you must have signed 
[the UMLS agreement (http://wwwcf.nlm.nih.gov/umlslicense/snomed/license.cfm)].
The UMLS agreement requires those who use the 
[UMLS (http://www.nlm.nih.gov/research/umls/)] to file a brief
report once a year to summarize their use of the UMLS. It also
requires the acknowledgment that the UMLS contains copyrighted
material and that those copyright restrictions be respected. The UMLS
agreement requires users to agree to obtain agreements for EACH
copyrighted source prior to it's use within a commercial or production
application.

To download SemRep, you must have access to an UTS account. For
information on how to acquire an UTS account, please see the 
[UMLS Terminology Services Page (http://uts.nlm.nih.gov/)].

** Platform Prerequisites

SemRep relies on MetaMap. Therefore, the platform prerequisites for MetaMap 
also apply to SemRep. The public version of MetaMap should
be downloaded from the [MetaMap portal (http://metamap.nlm.nih.gov)], 
then installed and tested before proceeding to SemRep installation.
The current SemRep release is compatible MetaMap 2014. Note that SemRep may run with 
newer versions of MetaMap; however, we cannot guarantee that the results remain the same.
Note also that SemRep, by default, uses the Word Sense Disambiguation (WSD) Server, optionally 
installed with MetaMap. Therefore, SemRep users should take into consideration WSD Server platform prerequisites
when installing MetaMap.

** Space Requirements

In addition to MetaMap space requirements, the current SemRep release requires about 71G disk space. 
This assumes that the entire SemRep data is installed. The data files that take up the most space are 
UMLS data (Lexicon and Metathesaurus) files (approx. 53GB) and UMLS hierarchy files (approx. 18GB), including files 
for 2006AA and 2015AA UMLS releases. If you are only interested in a specific UMLS release and/or specific MetaMap mode (relaxed vs. strict),
space requirements are much lower. See installation instructions below (Step 2) to configure SemRep to run with
only a specific UMLS release. The following are the approximate data file sizes for UMLS releases available 
with this distribution:

2006AA (23G)
2015AA (23G)

If you do not intend to use relaxed mode option (-M), the requirements are as follows:

2006AA (16.6G)
2015AA (11.3G)

By default, SemRep uses and is optimized for 2006AA release. UMLS data and hierarchy files are provided separately.


* Getting the distribution

The public distribution can be downloaded from:

  http://semrep.nlm.nih.gov/

* Extracting the distribution

Use the following |tar| command to extract the distribution:

^<<
% tar xvfj public_semrep_{version}_{os}.tar.bz2
^>>

|Tar| command will create the distribution directory |public_semrep|. It is recommended that
public_semrep distribution be installed alongside public_mm distribution, i.e., they
have the same parent directory.

The UMLS Lexicon, Metathesaurus and hierarchy data files can also be downloaded from:
 
  http://semrep.nlm.nih.gov/

Use the following |tar| command to extract the UMLS SPECIALIST Lexicon and Metathesaurus. For the current
release, available years are 06 and 15. You can download and use both, or select an appropriate
year. 

^<<
% tar xvfj public_semrep_{version}_lex_db_{year}_{os}.tar.bz2
^>>

Use the following |tar| command to extract the UMLS hierarchy files. Again, you can download for both
06 and 15, or select an appropriate year. 

^<<
% tar xvfj public_semrep_{version}_hier_{year}_{os}.tar.bz2
^>>

* Install

SemRep installation is similar to MetaMap installation, so much of the material
below has been adapted from MetaMap installation instructions.

** Step 1

To begin the initial install, go to the directory created when you
extracted the distribution (|public_semrep|).  You can speed up the
process by telling the install program where your java installation is
by setting the environment variable JAVA_HOME to the Java installation
directory.  If you don't set the variable the program will prompt you
for the information.

To file out where your java installation is located type the following
at command line:

^<<
% which java
^>>

To set the environment variable JAVA_HOME:

If the command: |which java| returns /usr/local/jre1.8.0/bin/java,
then JAVA_HOME should be set to /usr/local/jre1.8.0/.

For example:

^<<
# in C Shell (csh or tcsh)
setenv JAVA_HOME /usr/local/jre1.8.0

# in Bourne Again Shell (bash)
export JAVA_HOME=/usr/local/jre1.8.0

# Bourne Shell (sh)
JAVA_HOME=/usr/local/jre1.8.0
export JAVA_HOME
^>>

You also need to add the public_semrep/bin directory to your
program path (<public_semrep's parent dir> refers to the directory
where |public_semrep| is located): 

^<<
# in C Shell (csh or tcsh)
set path = ( $path <public_semrep's parent dir>/public_semrep/bin )

# in Bourne Again Shell (bash)
export PATH=$PATH:<public_semrep's parent dir>/public_semrep/bin

# Bourne Shell (sh)
PATH=$PATH:<public_semrep's parent dir>/public_semrep/bin
export PATH
^>>


** Step 2

Run the installation script as follows:

NOTE: If you are only interested in using a single UMLS dataset (2006, or 2015), you can 
save on space requirements by only downloading and installing the relevant dataset and 
changing the line in install.sh 

AVAILABLE_DATA="2006 2015"

to 

AVAILABLE_DATA="2015" (If you only want to use 2015 UMLS datasets).

^<<
% cd public_semrep
% ./bin/install.sh
^>>

An example of a successful installation looks as follows:

^<<
$ cd public_semrep
$ bin/install.sh 
Enter basedir of SemRep installation [/rhome/shindongwoo/public_semrep_v1.7] 

Enter basedir of MetaMap installation [/rhome/shindongwoo/public_mm] 

Basedir is set to /rhome/shindongwoo/public_semrep_v1.7.
MetaMap basedir is set to /rhome/shindongwoo/public_mm.

Where does your distribution of Sun\'s JRE reside?
Enter home path of JRE (JDK) [/usr/local/jdk1.8.0_60]: 

Using /usr/local/jdk1.8.0_60 for JAVA_HOME.

Delete UMLS Metathesaurus and SPECIALIST Lexicon data files after installation (recommended)? [y/n]:
n
... will keep the data files after installation. DATA/DB and DATA/LEXICON directories may be deleted later after successful installation.
Using /usr/local/jdk1.8.0_60 for JAVA_HOME.

Copying 2006 lexicon data files...
Copying 2015 lexicon data files...
Copying 2006 MetaMap DB data files...
Copying 2015 MetaMap DB data files...
Setting up bin directory scripts:
/rhome/shindongwoo/public_semrep_v1.7/bin/SEMREPrun.v1.7 generated.
/rhome/shindongwoo/public_semrep_v1.7/bin/semrep.v1.7 generated.
Setting up test suite:
/rhome/shindongwoo/public_semrep_v1.7/TestSuite/runTest_v1.7.sh generated.
Checking for required datafiles
Checking for required MetaMap datafiles
Checking for required SemRep datafiles
Public SemRep Install complete.

Public SemRep Install Settings:

Public MetaMap basedir: /rhome/shindongwoo/public_mm
Public SemRep basedir: /rhome/shindongwoo/public_semrep_v1.7
Public SemRep Program Dir: /rhome/shindongwoo/public_semrep_v1.7/bin
Java Home dir: /usr/local/jdk1.8.0_60
$ 
^>>


* Starting SemRep

SemRep requires that two servers associated with MetaMap, SKR-MedPost Part-of-Speech
Tagger and Word Sense Disambiguation (WSD) Servers, be already running. For more details 
on starting and stopping these processes, see [MetaMap README documentation (http://metamap.nlm.nih.gov)].

** Determining whether the servers are running

You can determine if the server are running by the command:

^<<
% ps ax | grep java
^>>

The output should look something like this:

^<<
16105 pts/0    Sl     0:00 /usr/local/jdk1.8.0_01/bin/java -Xmx2g -Dserver.config...
16158 pts/0    R+     0:00 grep java
21914 pts/0    Sl     0:02 /usr/bin/gij -Djava.version=1.4.2 -Djava.home=/usr/lib/...
%
^>>


** Invoking SemRep

If there are no errors starting the WSD and Tagger servers then  
SemRep can be run as follows:

^<<
% ./bin/semrep.v1.7
^>>

This command will run SemRep with the 2006 UMLS dataset. To run SemRep with the 2015 dataset, 
use the following command:

^<<
% ./bin/semrep.v1.7 -L 2015 -Z 2015AA
^>>

Other SemRep options are explained [elsewhere (http://semrep.nlm.nih.gov/SemRep_v1_7_Options.html)].

* Testing SemRep

SemRep distribution comes with a test suite to ensure that the results obtained with the distribution
matches those obtained with our internal version. To test SemRep, go to |TestSuite| directory under |public_semrep|, a
nd run the following script: 

^<<
% ./runTest_v1.7.sh 
^>>

This script will run the SemRep program with several different option combinations, compare the 
results with the results of our internal version and display the differences, if any. 
Otherwise, the script will simply print out DIFFS OK messages. 

* Questions

If you have problems then send an email to semrep@nlm.nih.gov.

* Using SemRep

For more information on running SemRep and its options, please
see these references:

	1. [Semrep v1.7 options (http://semrep.nlm.nih.gov/SemRep.v1.7_Options.html)]
	2. [Full-fielded output format description (http://semrep.nlm.nih.gov/semrep_full_fielded_output.pdf)]
	3. [XML output description (http://semrep.nlm.nih.gov/SemRep.v1.7._XML_output_desc.html)]


(1) Rindflesch,  T.C. and Fiszman, M. (2003). The interaction of domain knowledge and
linguistic structure in natural language processing: interpreting hypernymic propositions 
in biomedical text. Journal of Biomedical Informatics, 36(6):462-477. 
