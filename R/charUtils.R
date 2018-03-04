hex2bits <- function(xs) {
  # translate a raw byte into a vector of {0,1}
  bits <- numeric()
  a <- c(128, 64, 32, 16, 8, 4, 2, 1)
  for (x in xs) {
    x <- as.integer(x)
    bits <- c(bits, floor(x / a) %% 2)
  }
  return(bits)
}

bits2dec <- function(b) {
  # translate a vector of max 4 bytes to decimal
  a <- c(2147483648, 1073741824, 536870912, 268435456, 134217728, 67108864,
         33554432, 16777216, 8388608, 4194304, 2097152, 1048576, 524288,
         262144, 131072, 65536, 32768, 16384, 8192, 4096, 2048, 1024, 512,
         256, 128, 64, 32, 16, 8, 4, 2, 1)
  b <- leftPad(b, 32)
  return(sum(a * b))
}

leftPad <- function(b, l) {
  # left-pad a vector b with 0 to length l
  return(c(rep(0, l - length(b)), b))
}

char2codepoint <- function(chr) {
  # given a charcter in UTF-8, calculate its codepoint in decimal
  # cf. https://en.wikipedia.org/wiki/UTF-8
  myRaw <- charToRaw(chr)
  nBytes <- length(myRaw)
  if (nBytes == 1) {
    myCodepoint <- bits2dec(myRaw)
  } else if (nBytes == 2) {
    myBytes <- hex2bits(myRaw)
    myCodepoint <- bits2dec(leftPad(myBytes[c(4:8, 11:16)], 16))
  } else if (nBytes == 3) {
    myBytes <- hex2bits(myRaw)
    myCodepoint <- bits2dec(leftPad(myBytes[c(5:8, 11:16, 19:24)], 24))
  } else if (nBytes == 4) {
    myBytes <- hex2bits(myRaw)
    myCodepoint <- bits2dec(leftPad(myBytes[c(6:8, 11:16, 19:24, 27:32)], 32))
  }
  return(myCodepoint)
}