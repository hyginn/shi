# zzz.R
#
# Package startup and unload functions




.onLoad <- function(libname, pkgname) {

    # # Make list of shi parameters and add to global options
    #
    # # filepath of logfile
    # optShi <- list(shi.logfile = logFileName() )
    #
    # # add more options ...
    # optShi[["nameOfOption"]] <- value
    #
    # optionsToSet <- !(names(optShi) %in% names(options()))
    #
    # if(any(optionsToSet)) {
    #     options(optShi[optionsToSet])
    # }

    invisible()
}


.onAttach <- function(libname, pkgname) {
  # Startup message
  m <- character()
  m[1] <- "\nWelcome to shi.\n"

  packageStartupMessage(paste(m, collapse=""))
}


# .onUnload <- function(libname, pkgname) {
#
# }



# [END]
