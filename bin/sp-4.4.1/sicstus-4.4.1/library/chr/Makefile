SICSTUS_FLAGS=-f --nologo --noinfo

TMPFILES = \
	chr_hashtable_store.pl \
	chr_messages.pl \
	hprolog.pl \
	binomialheap.pl \
	chr_compiler_utility.pl \
	chr_op2.pl \
	chr_op.pl \
	find.pl \
	guard_entailment.chr \
	listmap.pl \
	pairlist.pl \
	b_globval.pl \
	hpattvars.pl \
	swi2sicstus.pl \
	a_star.pl \
	builtins.pl \
	chr_compiler_options.pl \
	chr_runtime.pl \
	chr_swi_bootstrap.pl \
	chr_translate_bootstrap1.chr \
	chr_translate_bootstrap2.chr \
	chr_translate_bootstrap.pl \
	chr_translate.chr \
	clean_code.pl \
	chr_compiler_errors.pl \
	../chr.pl \
	chr_translate_bootstrap1.pl \
	chr_translate_bootstrap1.pl \
	chr_translate_bootstrap2.pl \
	chr_translate_bootstrap2.pl \
	guard_entailment.pl \
	chr_translate.pl \
	guard_entailment.pl \
	chr_translate.pl \

all:
	cp -f orig_files/swi2sicstus.pl .
	cp -f orig_files/chr_hashtable_store.pl .
	cp -f orig_files/chr_messages.pl .
	cp -f orig_files/hprolog4.pl     hprolog.pl
	cp -f orig_files/binomialheap.pl .
	cp -f orig_files/chr_compiler_utility.pl .
	cp -f orig_files/chr_op2.pl .
	cp -f orig_files/chr_op.pl .
	: cp -f orig_files/find.pl .
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/find.pl','find.pl'), halt."
	cp -f orig_files/guard_entailment.chr .
	cp -f orig_files/listmap.pl .
	cp -f orig_files/pairlist.pl .
	cp -f orig_files/b_globval.pl .
	cp -f orig_files/hpattvars.pl .
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/a_star.pl','a_star.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/builtins.pl','builtins.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/chr_compiler_options.pl','chr_compiler_options.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/chr_runtime.pl','chr_runtime.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/chr_swi_bootstrap.pl','chr_swi_bootstrap.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/chr_translate_bootstrap1.chr','chr_translate_bootstrap1.chr'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/chr_translate_bootstrap2.chr','chr_translate_bootstrap2.chr'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/chr_translate_bootstrap.pl','chr_translate_bootstrap.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/chr_translate.chr','chr_translate.chr'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/clean_code.pl','clean_code.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/chr_compiler_errors.pl','chr_compiler_errors.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l swi2sicstus.pl --goal "transform('orig_files/chr_swi.pl','../chr.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l chr_swi_bootstrap.pl --goal "chr_compile_step1('chr_translate_bootstrap1.chr','chr_translate_bootstrap1.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l chr_swi_bootstrap.pl --goal "chr_compile_step2('chr_translate_bootstrap1.chr','chr_translate_bootstrap1.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l chr_swi_bootstrap.pl --goal "chr_compile_step2('chr_translate_bootstrap2.chr','chr_translate_bootstrap2.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l chr_swi_bootstrap.pl --goal "chr_compile_step3('chr_translate_bootstrap2.chr','chr_translate_bootstrap2.pl'), halt."
	$(SICSTUS) $(SICSTUS_FLAGS) -l chr_swi_bootstrap.pl --goal "chr_compile_step3('guard_entailment.chr','guard_entailment.pl'), halt." 
	$(SICSTUS) $(SICSTUS_FLAGS) -l chr_swi_bootstrap.pl --goal "chr_compile_step3('chr_translate.chr','chr_translate.pl'), halt." 
	$(SICSTUS) $(SICSTUS_FLAGS) -l chr_swi_bootstrap.pl --goal "chr_compile_step4('guard_entailment.chr','guard_entailment.pl'), halt." 
	$(SICSTUS) $(SICSTUS_FLAGS) -l chr_swi_bootstrap.pl --goal "chr_compile_step4('chr_translate.chr','chr_translate.pl'), halt." 

.PHONY: clean realclean
clean realclean:

.PHONY: distclean maintainerclean mostlyclean
distclean maintainerclean mostlyclean:
	rm -f $(TMPFILES)

