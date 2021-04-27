#' Test if \code{mutt} is installed on the host platform
#' See \url{http://www.mutt.org/}
#'
#' @family mail
#' @export
#' @return logical, TRUE if present
has_mutt <- function() {
  basename(Sys.which("mutt")) == "mutt"
}

#' Test if \code{mail} is installed on the host platform
#' See \url{https://www.binarytides.com/linux-mail-command-examples/}
#'
#' @family mail
#' @export
#' @return logical, TRUE if present
has_mail <- function() {
  basename(Sys.which("mail")) == "mail"
}

#' Test if \code{nail} is installed on the host platform
#' See \url{http://nail.sourceforge.net/}
#'
#' @family mail
#' @export
#' @return logical, TRUE if present
has_nail <- function() {
  basename(Sys.which("nail")) == "nail"
}


#' Send an email from R using the underlying mail services.
#'
#' A simple wrapper around the platforms email engine.  Currently \code{mutt},
#' \code{nail} and \code{mail} are supported, but the host must have these installed.
#'
#' @family mail
#' @export
#' @param ... arguments for the mailer of choice.
#' @param app the name of the mail engine to use, currently 'mail' (default), nail' and 'mutt' are supported
#' @return value returned by the system-level mail program, non-zero for failure.
sendmail <- function(..., app = c('mail', 'mutt', 'nail')[1]){

  switch(tolower(app[1]),
         "mutt" = muttmail(...),
         "nail" = nailmail(...),
         "mail" = mailmail(...),
         1)
}


#' Send a simple mail via mutt  - see \url{http://www.mutt.org/}
#'
#' @family mail
#' @export
#' @param to a character vector of one or more valid email addresses
#' @param subject a character for the subject line (required)
#' @param message - a character vector of one or more lines for the message body (NA to skip)
#' @param attachment the fully qualified filename to attach (if any, NA to skip)
#' @param verbose  logical, if TRUE then echo the command
#' @return 0 for success and non-zero otherwise
muttmail <- function(to = "name@somewhere.org",
                     subject = "mutt mail",
                     message = paste("at", Sys.time(), "you have mutt mail"),
                     attachment = NA,
                     verbose = FALSE){

  if (!has_mutt()) stop("muttmail: mutt is not installed")
  address <- paste(to,collapse = " ")
  cmd <- paste("mutt", "-s", shQuote(subject[1]))
  if (!is.na(attachment)) cmd <- paste(cmd, "-a", attachment, "--")
  cmd <- paste(cmd, address)
  hasMsg <- (length(message) > 0) || !is.na(message[1])
  if (hasMsg) {
    msgFile <- tempfile()
    cat(message, sep = "\n", file = msgFile)
    cmd <- paste(cmd, "<", msgFile)
  }
  if (verbose) cat(cmd, "\n")
  ok <- system(cmd)
  # clean up
  if (hasMsg) unlink(msgFile)
  return(ok)
}

#' Send a simple mail via nail  - see \url{http://linux.die.net/man/1/nail}
#'
#' @family mail
#' @export
#' @param to a character vector of one or more valid email addresses
#' @param subject a character for the subject line (required)
#' @param message - a character vector of one or more lines for the message body (NA to skip)
#' @param attachment the fully qualified filename to attach (if any, NA to skip)
#' @param verbose  logical, if TRUE then echo the command
#' @return 0 for success and non-zero otherwise
nailmail <- function(to = "name@somewhere.org",
                     subject = "nail mail",
                     message = paste("at", Sys.time(), "you have nail mail"),
                     attachment = NA,
                     verbose = FALSE){
  if (!has_nail()) stop("nailmail: nail is not installed")
  address <- paste(to,collapse = " ")
  cmd <- paste("nail", "-s", shQuote(subject[1]))
  if (!is.na(attachment)) cmd <- paste(cmd, "-a", attachment)
  cmd <- paste(cmd, address)
  hasMsg <- (length(message) > 0) || !is.na(message[1])
  if (hasMsg) {
    msgFile <- tempfile()
    cat(message, sep = "\n", file = msgFile)
    cmd <- paste(cmd, "<", msgFile)
  }
  # speak?
  if (verbose) cat(cmd, "\n")
  ok <- system(cmd)
  # clean up
  if (hasMsg) unlink(msgFile)
  return(ok)
}


#' Send a mail message where \code{mail} binary is available
#'
#' @export
#' @param to character, vector of one or more email addresses
#' @param subject character subject line
#' @param message character, vector of one or more lines of text
#' @param attachment NULL or charcater, file path to optional attachment
#' @param verbose  logical, if TRUE then echo the command
#' @return integer success code (0) or some non-zero if an error
#' @examples
#' \dontrun{
#' cat(LETTERS, sep = "\n", file = "letters.txt")
#' sendmail(to = "foo@bar.com",
#'          subject = "Facts",
#'          message = c("Fact: dogs start out as puppies.", "Not sure about cats."),
#'          attachment = "letters.txt")
#'}
mailmail <- function(to = 'btupper@bigelow.org',
                     subject = "mail from charlier",
                     message = "It's so wonderful to get mail, ain't it?",
                     attachment = NULL,
                     verbose = FALSE){

  mailapp <- Sys.which("mail")
  if ( mailapp == "") stop("mail application not available")

  # https://tecadmin.net/ways-to-send-email-from-linux-command-line/
  # mail -a [attachment] -s [subject] <to> < [message]

  # store the message in a temporary file
  msgfile <- tempfile()
  cat(message, sep = "\n", file = msgfile)

  cmd <- sprintf("-s %s %s < %s", shQuote(subject), paste(to, collapse = ","), msgfile)
  if (!is.null(attachment)){
    cmd <- sprintf("-a %s %s", attachment, cmd)
  }

  if(verbose){
    cat(mailapp, cmd, "\n")
  }
  ok <- system2(mailapp, args = cmd)

  unlink(msgfile)

  return(ok)
}


