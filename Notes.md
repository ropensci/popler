Notes re: R package
================
Sam Levin
May 19, 2018

Must fix before submitting
--------------------------

-   Bug in report\_dictionary that causes species epithets to be listed under structure rather than species (only observed in R 3.5, but may be an issue going back further)

-   We need more/updated unit tests.

-   I will update this as I have time/find more

Things that don't need fixing, but are worth thinking about
-----------------------------------------------------------

-   Does it make more sense to initiate the connection to popler at the beginning of each session rather than the initiation of a query? might reduce data retrieval times for users with high request rate. I don't think this is particularly pressing before submitting

-   currently, *cov\_unpack* returns a data frame with twice as many columns as covariates, storing the labels and their values in separate columns. would it make more sense to switch the label column to value column name, shrinking the tbl's size?
