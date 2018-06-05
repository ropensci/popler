rm(list = ls())

library(DependenciesGraphs)
library(popler)

# Create initial dependency graph. Unfortunately, this package only seems to
# link between exported functions. So, we can manually add in the others and 
# create out dependency overview.

deps <- envirDependencies('package:popler')
plot(deps)

# start at the top and work our way down
deps$Nomfun

# authors
# - no calls to other funs
# browse
# - summary_table_import already accounted
deps$fromto[ ,11] <- c(2, 46) # vars_spell 
deps$fromto[ ,12] <- c(2, 25) # possible_vars
deps$fromto[ ,13] <- c(2, 3) # call_update
deps$fromto[ ,14] <- c(2, 16) # keyword_subset
deps$fromto[ ,15] <- c(2, 32) # select_by_criteria
deps$fromto[ ,16] <- c(2, 43) # vars_check
deps$fromto[ ,17] <- c(2, 41) # taxa_nest
deps$fromto[ ,18] <- c(2, 42) # trim_display

# cov_unpack
# - no calls to other funs

# dictionary
deps$fromto[ ,19] <- c(11, 25) # possible_vars
deps$fromto[ ,20] <- c(11, 44) # vars_dict
deps$fromto[ ,21] <- c(11, 12) # dictionary_explain
deps$fromto[ ,22] <- c(11, 10) # dict_list

# vars_dict
# - no calls to other funs

# verify_vars
# - no calls to other funs

# dict_list
deps$fromto[ ,23] <- c(10, 47) # verify_vars

# dictionary_explain
# - no calls to other funs

# get_data
deps$fromto[ ,24] <- c(15, 9) # db_open
deps$fromto[ ,25] <- c(15, 45) # vars_query
deps$fromto[ ,26] <- c(15, 5) # concatenate_queries
deps$fromto[ ,27] <- c(15, 3) # call_update
deps$fromto[ ,28] <- c(15, 13) # expr_vars_get
deps$fromto[ ,29] <- c(15, 20) # parse_to_sql_search
deps$fromto[ ,30] <- c(15, 24) # popler_query
deps$fromto[ ,31] <- c(15, 4) # colname_change
deps$fromto[ ,32] <- c(15, 6) # cov_unpack
deps$fromto[ ,33] <- c(15, 7) # data_message
deps$fromto[ ,34] <- c(15, 9) # db_close

# vars_query
deps$fromto[ ,35] <- c(45, 26) # query_get

# concatenate_queries
# - no calls to other funs

# popler_query
deps$fromto[ ,36] <- c(24, 26) # query_get

# data_message
# - no calls to other funs

# lter_maps
deps$fromto[ ,37] <- c(17, 27) # rebrowse

# metadata_url
# summary_table_import already accounted for

# parse_to_sql_search
# - no calls to other funs

# popler_citation
deps$fromto[ ,38] <- c(21, 27) # rebrowse

# report_dictionary
# - no calls to other funs

# report_metadata







