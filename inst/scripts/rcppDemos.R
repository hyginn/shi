# rcppDemos.R
#
# Tutorial and Demos for RCPP
#
# Boris Steipe
# ==============================================================================


#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                           Line
#TOC> -----------------------------------------------
#TOC>   1        Preparations                      22
#TOC>   2        The simplest Rcpp program         56
#TOC>   3        Scenario: Sequence to Codons      63
#TOC>   4        Benchmarking                     180
#TOC>   5        Three RCPP versions              215
#TOC>
#TOC> ==========================================================================


# =    1  Preparations  ========================================================

# Check packages availability ...
notInstalled <- function(pkg){
  return(! is.element(pkg, installed.packages()[,1]))
}

if (notInstalled("Rcpp")) { install.packages("Rcpp") }
# Package information:
#  library(help = Rcpp)       # basic information
#  browseVignettes("Rcpp")    # available vignettes

vignette("Rcpp-introduction")


# Various other packages that will not be explicitly loaded ...
if (notInstalled("microbenchmark")) { install.packages("microbenchmark") }
library(microbenchmark)

if (notInstalled("stringi"))        { install.packages("stringi") }
if (notInstalled("stringr"))        { install.packages("stringr") }
if (notInstalled("ore"))            { install.packages("ore") }
if (notInstalled("Biostrings")) {
  if (! exists("biocLite")) { source("https://bioconductor.org/biocLite.R") }
  biocLite("Biostrings")
}

# The following files are assumed to exist in ./src:
#  - hello.cpp
#  - codonSplit1.cpp
#  - codonSplit2.cpp
#  - codonSplit3.cpp


# =    2  The simplest Rcpp program  ===========================================

file.show("./src/hello.cpp")
Rcpp::sourceCpp("./src/hello.cpp")
hello()


# =    3  Scenario: Sequence to Codons  ========================================


# Assume we want to split a nucleotide sequence string into codons. This could
# be an ORF like
x1 <- "ATGCGGCATTGCAGCTGA"
x2 <- "ATGCGGCATTGCAGCTGAT"  # Note: one truncated codon at the end
# ...and it should be converted into a vector like
# c("ATG", "CGG", "CAT", "TGC", "AGC", "TGA") ... respectively
# c("ATG", "CGG", "CAT", "TGC", "AGC", "TGA", "T")

# Let's try some base-R approaches.

splitCodons1 <- function(x){  # split and paste with logical vector
    codons <- character()
    y <- unlist(strsplit(x, ""))
    codons <- paste0(y[c(TRUE, FALSE, FALSE)],
                     y[c(FALSE, TRUE, FALSE)],
                     y[c(FALSE, FALSE, TRUE)])
    return(codons)
}
splitCodons1(x1)
splitCodons1(x2)   # oops ... bitten by recycling!

splitCodons2 <- function(x){  # split and paste with seq of indices
    codons <- character()
    y <- unlist(strsplit(x, ""))
    a <- y[seq(1, length(y), by = 3)]
    b <- y[seq(2, length(y), by = 3)]
    c <- y[seq(3, length(y), by = 3)]
    codons <- paste0(a, b, c)
    return(codons)
}
splitCodons2(x1)
splitCodons2(x2)   # oops ... again: recycling appends a fantasy codon!

splitCodons3 <- function(x) { # using gsub()
    codons <- character()
    y <- unlist(strsplit(x, ""))
    temp <- gsub(x = x, pattern = "([ATCG]{3})", replacement = "\\1,", fixed = FALSE)
    codons <- unlist(strsplit(temp, split = ","))
    return(codons)
}
splitCodons3(x1)
splitCodons3(x2)   # This one gets it right.



# ... what other options do we have, and how do they compare in time and memory
# use? In principle we can use methods that search for patterns, and methods
# that extract substrings.

# Solution 1: a positive lookbehind regular expression in base R:
strsplit(x1, "(?<=...)", perl=TRUE)[[1]]
strsplit(x2, "(?<=...)", perl=TRUE)[[1]]

# Solution 2: stringi offers a function to extract all substrings matching
# a pattern:
stringi::stri_extract_all_regex(x1, '...')[[1]]
stringi::stri_extract_all_regex(x2, '...')[[1]]  # Drops the last nucleotide

# Solution 2.1:
# The Oniguruma regular expression library allows to precompile regular
# expressions, which we can expect to speed things up for repeated queries
NNN <- ore::ore("...")
ore::matches(ore::ore.search(NNN, x1, all = TRUE))
ore::matches(ore::ore.search(NNN, x2, all = TRUE))  # Drops the last nucleotide

# Solution 3:
# We could precalculate a vector of indices and then use the
# vectorized substring() or stringr::str_sub() to extract substrings
# from the indices
pos <- seq(1, nchar(x1), by = 3)
substring(x1, first=pos, last=pos+2)
stringr::str_sub(x1, start=pos, end=pos+2)

pos <- seq(1, nchar(x2), by = 3)
substring(x2, first=pos, last=pos+2)
stringr::str_sub(x2, start=pos, end=pos+2)
# both implement the requirement correctly - but require an additional vector
# of length of the sequence in memeory.


splitCodons4 <- function(x) {
    pos <- seq(1, nchar(x), by = 3)
    return(substring(x, first=pos, last=pos+2))
}
splitCodons4(x1)
splitCodons4(x2)

str_splitCodons4 <- function(x) {
  pos <- seq(1, nchar(x), by = 3)
  return(stringr::str_sub(x, start=pos, end=pos+2))
}
str_splitCodons4(x1)
str_splitCodons4(x2)


# Solution 4:
# We could strsplit() into single characters and then use our pos
# vector to reassamble the characters:
pos <- seq(1, nchar(x1), by = 3)
y <- strsplit(x1, "")[[1]]
paste0(y[pos], y[pos+1], y[pos+2])

pos <- seq(1, nchar(x2), by = 3)
y <- strsplit(x2, "")[[1]]
paste0(y[pos], y[pos+1], y[pos+2]) #Two NA in the last codon - not wrong.


# Solution 5:
# The Biostrings package has a codons() function, but it needs to work
# on a DNAString object
as.character(Biostrings::codons(Biostrings::DNAString(x1)))
as.character(Biostrings::codons(Biostrings::DNAString(x2))) # ... with warning


# =    4  Benchmarking  ========================================================


# How fast?

cDNA <- paste(sample(c("A", "T", "C", "G"), 999, replace = TRUE), collapse="")
#pos <- seq(1, nchar(cDNA), by = 3)
NNN <- ore::ore("...")



microbenchmark(y <- splitCodons1(cDNA))
microbenchmark(y <- splitCodons2(cDNA))
microbenchmark(y <- splitCodons3(cDNA))

microbenchmark(y <- strsplit(cDNA, "(?<=...)", perl=TRUE)[[1]])

microbenchmark(y <- stringi::stri_extract_all_regex(cDNA, '...')[[1]])
microbenchmark(y <- ore::matches(ore::ore.search(NNN, cDNA, all = TRUE)))

microbenchmark(y <- splitCodons4(cDNA))
microbenchmark(y <- str_splitCodons4(cDNA))

# with Biostrings functions
microbenchmark(y <- as.character(Biostrings::codons(Biostrings::DNAString(cDNA))))
# note milliseconds here not microseconds!


# In my experience, stringi::stri_extract_all_regex() is consistently the
# fastest,  it is closely followed by the str_splitCondons4() version using
# stringr::str_sub(), which requires building an additional vector of positions.

# Can we be much faster with Rcpp?


# =    5  Three RCPP versions  =================================================


file.show("./src/codonSplit1.cpp")
Rcpp::sourceCpp("./src/codonSplit1.cpp")
cpp_codonSplit1(x1)
cpp_codonSplit1(x2)

file.show("./src/codonSplit2.cpp")
Rcpp::sourceCpp("./src/codonSplit2.cpp")
cpp_codonSplit2(x1)
cpp_codonSplit2(x2)

file.show("./src/codonSplit3.cpp")
Rcpp::sourceCpp("./src/codonSplit3.cpp")
cpp_codonSplit3(x1)
cpp_codonSplit3(x2)


# How do they compare
microbenchmark(y <- cpp_codonSplit1(cDNA))  # Slower than some base-R versions
microbenchmark(y <- cpp_codonSplit2(cDNA))  # about 15 times faster !!!
microbenchmark(y <- cpp_codonSplit3(cDNA))  # even faster

# Summary: our fastest RCpp function is faster than any other by a factor of 3 -
# but we have to be careful how we write Rcpp. It's not a panacea - a naive
# implementation is quite slow, slower than some base R versions. Rcpp allows
# you to code closer "to the metal", but many of the std library functions trade
# convenience (and safety) for speed. Careful considertaion of memory use will
# do a lot. Whatever you do, you must profile carefully.

# Overall, we seem to have achieved a speedup of 3 over anything we can do in R,
# and a 250-fold speedup over Biostrings. Can we be even faster? Probably yes,
# working with pointers and strcpy() should be even faster.

# [END]
