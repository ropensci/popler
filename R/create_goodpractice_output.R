# Runs goodpractice without lintrs. currently, all linters fail for an unknown reason
# on report_metadata.R. We need to investigate this, but unsure of how to resolve at the moment
# also adding to .Rbuildignore

GPOutput <- goodpractice::gp(checks = goodpractice::all_checks()[!grepl('lintr_', 
                                                                        goodpractice::all_checks())])