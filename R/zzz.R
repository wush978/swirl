.onAttach <- function(...) {
  if (interactive()) {
    .yesno <- function(prompt) {
      if (Sys.getenv("SWIRL_DEV") == "TRUE") return(TRUE)
      is_done <- FALSE
      while(!is_done) {
        result <- readline(prompt)
        if (nchar(result) == 0) next
        result <- switch(
          tolower(substring(result, 1, 1)),
          "y" = "y",
          "n" = "n",
          NA
        )
        if (!is.na(result)) is_done <- TRUE
      }
      result == "y"
    }
    is_chinese <- .yesno("Do you want to set the language to Chinese traditional?(y/n)")
    if (is_chinese) {
      make_pretty("I am going to adjust your locale settings.")
      locale_cmd <- switch(.Platform$OS.type, 
             "windows" = {
               'Sys.setlocale(locale = "cht")'
             },
             "unix" = {
               'Sys.setlocale(locale = "en_US.UTF-8")'
             })
      cat(sprintf("This is the suggested command: `%s`\n", locale_cmd))
      cat(sprintf("It is recommended to adjust the locale for beginners.\n"))
      cat(sprintf("If you cannot see any chinese, please visit <https://gitter.im/wush978/DataScienceAndR>.\n"))
      is_adjust <- .yesno("Do you want me to adjust the locale for you? (y/n)")
      if (is_adjust) {
        eval(parse(text = locale_cmd))
      }
      default$lang <- "chinese_traditional"
    }
  }
  
  if(length(ls(envir=globalenv())) > 0) {
    packageStartupMessage(
      make_pretty(s()%N%"Hi! I see that you have some variables saved in your",
                  s()%N%"workspace. To keep things running smoothly, I recommend you clean up",
                  s()%N%"before starting swirl.", skip_after=TRUE),
      make_pretty(s()%N%"Type ls() to see a list of the variables in your workspace.",
                  s()%N%"Then, type rm(list=ls()) to clear your workspace.", skip_after=TRUE),
      make_pretty(s()%N%"Type swirl() when you are ready to begin.", skip_after=TRUE)
    )
  } else {
    packageStartupMessage(
      make_pretty(s()%N%"Hi! Type swirl() when you are ready to begin.",
                  skip_after=TRUE)
    )
  }
  invisible()
}

make_pretty <- function(..., skip_before=TRUE, skip_after=FALSE) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  mes <- str_c("| ", wrapped, collapse = "\n")
  if(skip_before) mes <- paste0("\n", mes)
  if(skip_after) mes <- paste0(mes, "\n")
  mes
}