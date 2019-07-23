# Semrep v1.8 Release Notes


## Introduction

SemRep v1.8 is the fourth publicly available version of SemRep. It is also be the final publicly available version developed in Prolog programming language. The future development will be done in Java. 

This distribution includes the SemRep binary as well as the scripts that  are needed to run the binary, the required libraries, data sets, and the test suite.

## Enhancements

Recent enhancements to SemRep include:

1. **Negation processing:** SemRep's negation processing has been enhanced. The enhanced processing incorporates a modified and enhanced version of the [NegEx program](https://code.google.com/archive/p/negex/), better handling of argument and predicate negation, as well as pseudo-negation. 

2. **Better argument identification rules:** Identification of arguments of prepositional predicates (_in_, _for_, etc.) and hyphenated adjectives (i.e., _-induced_) and _-ing_ forms has been improved, in addition to many small changes that lead to better argument identification. 

3. **Generic knowledge extensions:** To address some infelicities/missing information in UMLS, a small supplementary knowledge-base can now be used by SemRep. Two non-default options (`-n:--use_generic_domain_modification` and `-N:--use_generic_domain_extension`) allow use of this knowledge-base. See [SemRep Options](doc/SemRep.v1.8_Options.txt) for more details. These extensions are a generalization of [earlier domain adaptations of SemRep](https://www.sciencedirect.com/science/article/pii/S1532046413001202).

4. **Better coordination handling:** Noun phrase coordination handling has ben improved, and as a result, some reciprocal predications can now be extracted (e.g., _interaction between BRCA1 and BRCA2_ -> BRCA1-INTERACTS_WITH-BRCA2).

5. **MetaMap options:** Some MetaMap options can now directly be invoked from SemRep to restrict/enhance SemRep behavior (e.g., the output can be restricted to certain semantic types, using `-J (--restrict_to_sts)` option). For a full list of current SemRep options, use `-h (--help)` option.

6. **UMLS dataset options:** We now provide the capability to run SemRep with 2018 dataset.  To run SemRep with this release, use the options `-L 2018 -Z 2018AA`. It is recommended that  corresponding Lexicon and Metathesaurus releases are used (i.e., 2018 with 2018AA).

7. **New NCBI Gene data:** SemRep v1.8 uses the NCBI Gene DB snapshot from November, 2017.

8. **Other:** SemRep v1.8 also includes many small bug fixes and enhancements,  updates to semantic interpretation rules as well as inclusion of sentence offsets and consistent semantic types in the output and improved hypernymic relation (ISA) extraction.


