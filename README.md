This R function just aims at constituting an alternative to the function `write.foreign` from the package `foreign`. The existing `write.foreign` misses several features that may be useful when translating R data into SAS:
* Naming of the table is a parameter, as well as the library (which prevents the user to modify the script manually).
* Labels for the variables (since it is now very well handled in R, especially when using RStudio).
* Management of possible different end-of-line characters, in particular when R and SAS are working on different OSs.

Additionally, this function is meant to work along with SAS: the script generates txt files and SAS scripts that must then be run on a SAS session in order to import the data into SAS (just as `write.foreign` already does). There is also the library `haven` that enables to directly exports R dataframes into SAS tables; however, it seems that depending on the version of SAS that is used, the tables turn sometimes not to be readable.