# shiWriteALN.R

#' Title.
#'
#' \code{<function>} Write an MSA to a connection in multi-FASTA format.
#'
#' Details.
#' @section Input: If input is an MsaAAMultipleAlignment or an AAStringSet it is
#'   converted into a character vector before processing. Reasonably, all
#'   sequences would have the same length, but that is not enforced and
#'   sequences that are shorter than range[2] are right-padded with "-" gap
#'   characters.
#'
#' @param ali object of class MsaAAMultipleAlignment or AAStringSet or a
#'   character vector with one sequence per element.
#' @param range two-integer vector of start and end positions if only a range of
#'   the MSA should be written, e.g. a domain. If missing, the full alignment is
#'   written.
#' @param note A string that is appended to the title incantation. Defaults to
#'   an empty string.
#' @param myCon text connection (cf. the con argument for writeLines). Defaults
#'   to stdout().
#' @param blockWidth width of sequence block. Default 60 characters.
#' @return None. The function is invoked for its side effect of printing an
#'   alignment to a text connection.
#'
#' @examples
#' mySeq <- c(firstSeq  = "MRMDPVI--MIMLWTARGPPDFVDFDCRNKRGFYNHDMRDASQYFHLE",
#'            secondSeq = "MKMDPVVKSLIVIWSAR---AFVQIDCRQQRGFY")
#' shiWriteALN(mySeq, blockWidth = 30)
#' @export
shiWriteALN <- function(ali,
                        range,
                        note = "",
                        myCon = stdout(),
                        blockWidth = 60) {

  blockWidth <- as.integer(blockWidth)
  if (is.na(blockWidth)) {
    stop("PANIC: parameter \"blockWidth\" must be numeric.")
  }
  if (blockWidth < 1) {
    stop("PANIC: parameter \"blockWidth\" must be greater than zero.")
  }
  if (blockWidth > 60) {
    warning("Programs that read CLUSTAL format might not expect blockWidth > 60.")
  }

  # Extract the raw data from the objects depending on their respective class
  # into a named vector of strings.

  # Extract XStringSet from MsaXMultipleAlignment ...
  if (class(ali) == "MsaAAMultipleAlignment" |
      class(ali) == "MsaDNAMultipleAlignment" |
      class(ali) == "MsaRNAMultipleAlignment") {
      ali <- ali@unmasked  # this is an XStringSet
  }

  # Process XStringSet
  if (class(ali) == "AAStringSet" |
      class(ali) == "DNAStringSet" |
      class(ali) == "RNAStringSet") {
    sSet <- as.character(ali)
  } else if (class(ali) == "character") {
    sSet <- ali
  } else {
    stop(sprintf("Input object of class %s is not supported.", class(ali)))
  }

  if (missing(range)) {
    range <- 1
    range[2] <- max(nchar(sSet))
  } else {
    range <- as.integer(range)
    if(length(range) != 2 ||
       any(is.na(range)) ||
       range[1] > range[2] ||
       range[1] < 1) {
      stop("PANIC: \"range\" does not consist of valid start and end index.")
    }
  }

  # Right-pad any sequence with "-" that is shorter than range[2]
    for (i in seq_along(sSet)) {
      if (nchar(sSet[i]) < range[2]) {
        sSet[i] <- paste0(sSet[i],
                          paste0(rep("-", range[2] - nchar(sSet[i])),
                                 collapse = ""))
      }
    }

  # Right-pad sequence names
  sNames <- names(sSet)
  len <- max(nchar(sNames)) + 2 # longest name plus two spaces
  for (i in seq_along(sNames)) {
    sNames[i] <- paste0(sNames[i],
                      paste0(rep(" ", len - nchar(sNames[i])),
                             collapse = ""))
  }

  # Process each sequence
  txt <- paste0("CLUSTAL W format. ", note)
  txt[2] <- ""

  iStarts <- seq(range[1], range[2], by = blockWidth)
  iEnds <- c((iStarts[-1] - 1), range[2])

  for (i in seq_along(iStarts)) {
    for (j in seq_along(sSet)) {
      txt <- c(txt,
               paste0(sNames[j], substring(sSet[j], iStarts[i], iEnds[i])))
    }
    txt <- c(txt, "")  # append a blank consenus line
    txt <- c(txt, "")  # append a separator line
  }

  writeLines(txt, con= myCon)

}


# [END]
