\name{dlgInput}
\alias{dlgInput}
\alias{dlgInput.gui}
\alias{dlgInput.textCLI}
\alias{dlgInput.nativeGUI}

\title{ A simple input box }
\description{
  Enter one (textual) value with this input box.
}

\usage{
dlgInput(message = "Enter a value", default = "", \dots, gui = .GUI)

## These should not be called directly
\method{dlgInput}{gui}(message = "Enter a value", default = "", \dots, gui = .GUI)
\method{dlgInput}{textCLI}(message = "Enter a value", default = "", \dots, gui = .GUI)
\method{dlgInput}{nativeGUI}(message = "Enter a value", default = "", \dots, gui = .GUI)
}

\arguments{
  \item{message}{ the message to display in the dialog box. Use \code{\\n} for
    line break, or provide a vector of character strings, one for each line. }
  \item{default}{ the default value in the text box. Single string or
    \code{NULL}. }
  \item{\dots}{ pass further arguments to methods. }
  \item{gui}{ the 'gui' object concerned by this dialog box. }
}

\value{
  The modified 'gui' object is returned invisibly. The text entered by the user
  at the input box, or an empty string if the dialog box was cancelled can be
  obtained from \code{gui$res} (see example).
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{dlgMessage}} } %, \code{\link{dlgText}} }

\examples{
## Ask something...
user <- dlgInput("Who are you?", Sys.info()["user"])$res
if (!length(user)) { # The user clicked the 'cancel' button
    cat("OK, you prefer to stay anonymous!\n")
} else {
    cat("Hello", user, "\n")
}
}

\keyword{ misc }

\concept{ GUI API dialog boxes }
