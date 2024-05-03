
## --------------------------------------------------------------------
#' Load configuration parameters from .env into environment variables
#'
#' It has become a practice to store configuration parameters related
#' to a project, in a hidden file called \code{.env}, in the working
#' directory of a project, and then set them as environment variables.
#'
#' This package loads the variables defined in the \code{.env} file
#' in the current working directory (as reported by \code{getwd}),
#' and sets them as environment variables.
#'
#' This happens automatically when the \code{dotenv} package is loaded,
#' so the typical use-case is to just put a `library(dotenv)` code at the
#' beginning of your R script.
#'
#' Alternatively a \code{dotenv::load_dot_env()} call can be used
#' to load variables from arbitrary files.
#'
#' The format of the \code{.env} file is also a valid unix shell
#' file format, so e.g. in \code{bash} the environment variables
#' can also be set by running \code{source .env}.
#'
#' See \code{\link{load_dot_env}} for the exact file format.
#'
#' @seealso load_dot_env
#' @docType package
#' @name dotenv-package
NULL

.onLoad <- function(libname, pkgname) {
  if (file.exists(".env")) load_dot_env()
}

#' Load environment variables from the specified file
#'
#' Load variables defined in the given file, as environment
#' variables.
#'
#' @details
#' The file is parsed line by line, and line is expected
#' to have one of the following formats:
#' \preformatted{VARIABLE=value
#' VARIABLE2="quoted value"
#' VARIABLE3='another quoted variable'
#' # Comment line
#' export EXPORTED="exported variable"
#' export EXPORTED2=another}
#'
#' In more details:
#' \itemize{
#'   \item A leading \code{export} is ignored, to keep the file
#'      compatible with Unix shells.
#'   \item No whitespace is allowed right before or after the
#'      equal sign, again, to promote compatilibity with Unix shells.
#'   \item No multi-line variables are supported currently. The
#'      file is strictly parsed line by line.
#'   \item Unlike for Unix shells, unquoted values are \emph{not}
#'      terminated by whitespace.
#'   \item Comments start with \code{#}, without any leading
#'      whitespace. You cannot mix variable definitions and
#'      comments in the same line.
#'   \item Empty lines (containing whitespace only) are ignored.
#' }
#'
#' It is suggested to keep the file in a form that is parsed the
#' same way with \code{dotenv} and \code{bash} (or other shells).
#'
#' @param file The name of the file to use.
#' @export
#'
#' @examples
#' # Remove, if it exists
#' Sys.unsetenv("dotenvexamplefoo")
#' Sys.getenv("dotenvexamplefoo")
#'
#' # Load from a file
#' tmp <- tempfile()
#' cat("dotenvexamplefoo=bar\n", file = tmp)
#' load_dot_env(tmp)
#' Sys.getenv("dotenvexamplefoo")
#'
#' # Clean up
#' unlink(tmp)

load_dot_env <- function(file = ".env", recursive = FALSE) {
  # Normalize the path to handle relative paths and potential path issues uniformly
  original_path <- normalizePath(file, mustWork = FALSE)
  
  if (!file.exists(original_path)) {
    if (recursive) {
      # If the file is not found and recursive is TRUE, search in parent directories
      path <- dirname(original_path)
      found_file <- NA  # Used to store the path of the found .env file
      
      # Loop until the file is found or we cannot go further up in the directory tree
      while (is.na(found_file) && !identical(path, normalizePath(dirname(path)))) {
        test_path <- file.path(path, basename(original_path))
        if (file.exists(test_path)) {
          found_file <- test_path  # Update with the path of the found file
          break  # Stop searching once the file is found
        }
        path <- dirname(path)  # Move to the next parent directory
      }
      
      # Handle the case where no .env file was found in any parent directory
      if (is.na(found_file)) {
        stop("No .env file exists in the current or any parent directory", call. = TRUE)
      } else {
        file <- found_file  # Set file to the found file path
      }
    } else {
      # If recursive is FALSE and the file is not found, stop with an error
      stop(".env file does not exist in the specified path", call. = TRUE)
    }
  }
  
  # Process the .env file
  tmp <- readLines(file)
  tmp <- ignore_comments(tmp)
  tmp <- ignore_empty_lines(tmp)
  
  # If there's no env vars, return nothing
  if (length(tmp) == 0) return(invisible())  
  
  tmp <- lapply(tmp, parse_dot_line)
  set_env(tmp)
}

ignore_comments <- function(lines) {
  grep("^#", lines, invert = TRUE, value = TRUE)
}

ignore_empty_lines <- function(lines) {
  grep("^\\s*$", lines, invert = TRUE, value = TRUE)
}

line_regex <- paste0("^\\s*",                  # leading whitespace
                     "(?<export>export\\s+)?", # export, if given
                     "(?<key>[^=]+)",          # variable name
                     "=",                      # equals sign
                     "(?<q>['\"]?)",           # quote if present
                     "(?<value>.*)",           # value
                     "\\g{q}",                 # the same quote again
                     "\\s*",                   # trailing whitespace
                     "$")                      # end of line

parse_dot_line <- function(line) {
  match <- regexpr(line_regex, line, perl = TRUE)
  if (match == -1) stop("Cannot parse dot-env line: ", substr(line, 1, 40),
                        call. = FALSE)
  as.list(extract_match(line, match)[c("key", "value")])
}

extract_match <- function(line, match) {
  tmp <- mapply(
    attr(match, "capture.start"),
    attr(match, "capture.length"),
    FUN = function(start, length) {
      tmp <- substr(line, start, start + length - 1)
    }
  )
  names(tmp) <- attr(match, "capture.names")
  tmp
}

set_env <- function(pairs) {
  tmp <- structure(
    .Data  = lapply(pairs, "[[", "value"),
    .Names = sapply(pairs, "[[", "key")
  )
  do.call(Sys.setenv, tmp)
}
