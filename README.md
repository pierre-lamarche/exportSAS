This R function just aims at constituting an alternative to the function `write.foreign` from the package `foreign`. The existing `write.foreign` misses several features that may be useful when translating R data into SAS:
* Labels for the variables (since it is now very well handled in R with the `tibble` object, especially when using RStudio); the labelling process relies on the function `label` from the library `Hmisc`.
* Possibility to switch on/off the `PROC FORMAT` applied to factors. When the labelling of the values is switched off, the choice has been made to write the numerical value of the levels (instead of the character string of the label, which would be more demanding in terms of memory).
* Management of possible different end-of-line characters, in particular when R and SAS are working on different OSs.
* An issue with missing values in character variables existing in `write.foreign` has been solved in this function.

Additionally, this function is meant to work along with SAS: the script generates txt files and SAS scripts that must then be run on a SAS session in order to import the data into SAS (just as `write.foreign` already does). There is also the library `haven` that enables to directly exports R dataframes into SAS tables; however, it seems that depending on the version of SAS that is used, the tables turn sometimes not to be readable.