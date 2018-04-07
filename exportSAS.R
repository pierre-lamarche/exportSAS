#' Export R data.frame objects into SAS datasets
#'
#' This R function just aims at constituting an alternative 
#' to the function `write.foreign` from the package `foreign`.
#' The existing `write.foreign` misses several features that may 
#' be useful when translating R data into SAS:
#' 
#'   * Labels for the variables (since it is now very well handled 
#'   in R with the `tibble` object, especially when using RStudio); 
#'   the labelling process relies on the function `label` from the 
#'   library `Hmisc`.
#'   * Possibility to switch on/off the `PROC FORMAT` applied to factors. 
#'   When the labelling of the values is switched off, the choice has 
#'   been made to write the numerical value of the levels (instead of 
#'   the character string of the label, which would be more demanding 
#'   in terms of memory).
#'   * Management of possible different end-of-line characters, in 
#'   particular when R and SAS are working on different OSs.
#'   * An issue with missing values in character variables existing in 
#'   `write.foreign` has been solved in this function.
#'   
#' Additionally, this function is meant to work along with SAS: the 
#' script generates txt files and SAS scripts that must then be run 
#' on a SAS session in order to import the data into SAS (just as 
#' `write.foreign` already does). There is also the library `haven` 
#' that enables to directly exports R dataframes into SAS tables; 
#' however, it seems that depending on the version of SAS that is 
#' used, the tables turn sometimes not to be readable.
#' 
#' # Usage
#' exportSAS(x, nameTab, nameFile, nameScript, folder, ...)
#'
#' @param x A data.frame object to be exported into a SAS dataset.
#' @param nameTab The name of the SAS table to be created when importing
#' the data in SAS (default is <name of x>).
#' @param nameFile The name of the ASCII file that will serve the
#' importation (default is <name of x>.txt).
#' @param nameScript The name of the SAS script that will be run 
#' in a SAS session to import the data (default is <name of x>.sas).
#' @param folder The path of the folder where the dataset has to 
#' be stored (default is the working directory).
#' @param separator The column separator in the ASCII file (default
#' is `,`).
#' @param labelVar logical; if TRUE (the default), retrieve the labels
#' in the data.frame and add them in the DATA STEP.
#' @param labelVal logical; if TRUE, retrieve the value labels for each
#' factor variable. Default is NULL; in case factors are found in the table
#' the user is asked whether to import value labels or not.
#' @param endofline a character value, "CRLF", "LF" or "CR". It sets the
#' end-of-line character to be used when SAS will read the ASCII file.
#' By default, it takes the one naturally used in the OS.
#' @param encoding a character value, indicating which encoding has to be
#' used to read the ASCII file.
#'
#' @return
#' @export
#'
#' @examples
exportSAS <- function(x, nameTab = NULL, nameFile = NULL, nameScript = NULL, folder = getwd(), separator = ",", 
                      labelVar = TRUE, labelVal = NULL, endofline = NULL, encoding = "") {
  oldPath <- getwd()
  setwd(folder)
  
  #assign default values to nameTab, nameFile and nameScript
  if (is.null(nameTab)) {
    nameTab <- deparse(substitute(x))
  }
  if (is.null(nameFile)) {
    nameFile <- paste0(deparse(substitute(x)), ".txt")
  }
  if (is.null(nameScript)) {
    nameScript <- paste0(deparse(substitute(x)), ".sas")
  }
  
  # manage end-of-line character
  if (is.null(endofline)) {
    if (.Platform$OS.type == "windows") endofline <- "CRLF" else
      if (.Platform$OS.type == "unix") endofline <- "LF"
  } else if (!endofline %in% c("CRLF","LF","CR"))
    stop("End-of-line parameter mispecified. Must be equal to CRLF, CR or LF.")
  
  # input list
  inputType <- sapply(x, class)
  if (!is.vector(inputType))
    inputType <- inputType[nrow(inputType),]
  inputT <- ifelse(inputType == "character", " $", "")
  inputList <- paste0(paste0(names(x), inputT), collapse = " \n ")
  
  # dealing with factor
  if (length(which(inputType == "factor")) > 0) {
    if (is.null(labelVal)) {
      aLabelVal <- readline("Import value labels as well? (Y/N)")
      while (!toupper(aLabelVal) %in% c("Y","N","YES","NO")) {
        cat("ERROR: answer shoud be Y/N. \n")
        aLabelVal <- readline("Import value labels as well? (Y/N)")
      }
      labelVal <- ifelse(toupper(aLabelVal) %in% c("Y","YES"), TRUE, FALSE)
    }
    codeFmt <- " \n"
    codeFmt2 <- "FORMAT \n"
    listVarFac <- names(x)[inputType == "factor"]
    addCode <- function(...) paste0(codeFmt, ...)
    addCode2 <- function(...) paste0(codeFmt2, ...)
    for (v in 1:length(listVarFac)) {
      var <- listVarFac[v]
      lvls <-levels(x[, var])
      if (labelVal == TRUE) {
        codeFmt <- addCode("PROC FORMAT ; \n")
        codeFmt <- addCode("VALUE FMT", v, " \n")
        for (i in 1:length(lvls)) {
          codeFmt <- addCode("  ", i, " = \"", lvls[i], "\" \n")
        }
        codeFmt <- addCode("; \n RUN ; \n \n")
        codeFmt2 <- addCode2("  ", var, " FMT", v, ". \n")
      }
      x[, var] <- as.numeric(x[, var])
    }  
  }
  
  # dealing with the length of character variables
  varChar <- names(x)[inputType == "character"]
  maxLength <- function(x) return(apply(sapply(x,nchar), 2, max, na.rm = TRUE))
  length <- maxLength(x[,varChar])
  lengthList <- paste0(paste0(varChar, inputT[inputType == "character"], length, "."), 
                       collapse = "\n ")
  
  # dealing with the max. length of records
  tab <- apply(x, 1, paste, collapse = separator)
  maxRec <- max(nchar(tab))
  
  # dealing with variable labels
  if (labelVar == TRUE) {
    require(Hmisc)
    codeLbl <- "LABEL \n"
    addCode <- function(...) paste0(codeLbl, ...)
    listVar <- names(x)
    for (v in 1:length(listVar)) {
      var <- listVar[v]
      if (!label(x[,var]) %in% c(""," "))
        codeLbl <- addCode(var, " = \"", label(x[, var]), "\" \n")
    }
    codeLbl <- addCode(" ; \n")
  }
  
  # writing SAS script
  addCode <- function(...) paste0(code, ...)
  code <- "/* Codes generated by function exportSAS  */\n "
  code <- addCode("LIBNAME out \"", folder, "\" ; \n ")
  code <- addCode("\n ")
  if (labelVal == TRUE)
    code <- addCode(codeFmt)
  code <- addCode("DATA out.", nameTab, " ; \n ")
  code <- addCode("INFILE \"", folder, "/", nameFile, "\" DSD DLM = \"", separator, 
                  "\" TERMSTR = ", endofline, " LRECL = ",maxRec," ; \n ")
  code <- addCode("LENGTH \n ", lengthList, "\n ; \n ")
  if (labelVal == TRUE)
    code <- addCode(codeFmt2, "; \n")
  code <- addCode("INPUT \n ")
  code <- addCode(inputList, " \n ; \n ")
  if (labelVar == TRUE)
    code <- addCode(codeLbl)
  code <- addCode("RUN ; \n ")
  
  write.table(x, file = nameFile, quote = FALSE, sep = separator, row.names = FALSE, 
              col.names = FALSE, fileEncoding = encoding, na = "")
  
  # writing raw data
  write.table(code, file = nameScript, quote = FALSE, sep = "", row.names = FALSE, 
              col.names = FALSE, fileEncoding = encoding)

  # set the former working directory
  setwd(oldPath)
}