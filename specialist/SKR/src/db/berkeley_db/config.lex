NUM_TABLES: 6
#
############################################################################
#
# Format:
#
# input_file|tablename|num_fields|fieldname1|...|N|fieldtype1|...|N|
#
#          E.g.: stopwords.txt|stpwrds|2|word|score|TXT|TXT
# 
# Field Types: 
#     TXT  -- data you wish to be accessed as text (or floats)
#     INT  -- data you wish to be accessed as integers
# 
# Maximum Number of Columns: 10
# Maximum Filename Length: 30
# Maximum Filename Length: 30
# Maximum Fieldname Length: 20
#
# Key Field: Assumed to be the first column & currently must be type TXT
#
# Processing:
#
#   To Add Entries:
#     1) Add line(s) @ bottom of file with your new table information
#     2) Increment first line (NUM_TABLES) to reflect additions
#     3) Run create_bulk from this directory
#
#   To Remove Entries:
#     1) Remove line(s) from file as necessary
#     2) Decrement first line (NUM_TABLES) to reflect deletions
#     3) There is no need to rerun create_bulk
#
# SPECIAL NOTE: If you are only adding a few tables, add them here
#   then copy the file over to config.tmp and remove all of the tables
#   except what you want to add, change the NUM_TABLES to reflect the
#   actual number to create, and then run create_bulk.  This eliminates
#   the need to run through the ENTIRE file of table creations to add
#   a single table.  You then specify {PATH}/config.tmp when prompted 
#   from create_bulk for the config file location.
#
############################################################################
#
BaseForm.txt|base_form|3|word|lexcat|baseform|TXT|TXT|TXT
CitationForm.txt|citation_form|3|word|lexcat|baseform|TXT|TXT|TXT
DMVariants.txt|dm_vars|4|base_form|base_lexcat|dm_variant|dm_variant_lexcat|TXT|TXT|TXT|TXT
InflVariants.txt|im_vars|4|citation_form|infl_variant|infl_lexcat|infl_feature|TXT|TXT|TXT|TXT
LexForm.txt|lex_form|3|word|lexcat|baseform|TXT|TXT|TXT
LexRec.txt|lex_rec|2|EUI|lexrec|TXT|TXT
