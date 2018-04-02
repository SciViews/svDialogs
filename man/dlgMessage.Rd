\name{dlgMessage}
\alias{dlgMessage}
\alias{dlgMessage.gui}
\alias{dlgMessage.textCLI}
\alias{dlgMessage.nativeGUI}
\alias{msgBox}
\alias{okCancelBox}

\title{ Display a message box }
\description{
  A message box with icon, text, and one to three buttons.
}

\usage{
dlgMessage(message,  type = c("ok", "okcancel", "yesno", "yesnocancel"),
    \dots, gui = .GUI)
msgBox(message)
okCancelBox(message)

## These should not be called directly
\method{dlgMessage}{gui}(message,  type = c("ok", "okcancel", "yesno", "yesnocancel"),
    \dots, gui = .GUI)
\method{dlgMessage}{textCLI}(message,  type = c("ok", "okcancel", "yesno", "yesnocancel"),
    \dots, gui = .GUI)
\method{dlgMessage}{nativeGUI}(message,  type = c("ok", "okcancel", "yesno", "yesnocancel"),
    \dots, gui = .GUI)
}

\arguments{
  \item{message}{ the message to display in the dialog box. Use \code{\\n} for
    line break, or provide a vector of character strings, one for each line. }
  \item{type}{ the type of dialog box: 'ok', 'okcancel', 'yesno' or
    'yesnocancel'. }
  \item{\dots}{ pass further arguments to methods. }
  \item{gui}{ the 'gui' object concerned by this dialog box. }
}

\value{
  The modified 'gui' object is returned invisibly. A string with the name of
  the button ("ok", "cancel", "yes" or "no") that the user pressed can be
  obtained from \code{gui$res} (see example).
  \code{msgBox()} just returns the name of the button (ok), while okCancelBox()
  returns \code{TRUE} if ok was clicked or \code{FALSE} if cancel was clicked.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{dlgInput}} }

\examples{
## A simple information box
dlgMessage("Hello world!")$res

## Ask to continue
dlgMessage(c("This is a long task!", "Continue?"), "okcancel")$res

## Ask a question
dlgMessage("Do you like apples?", "yesno")$res

## Idem, but one can interrupt too
res <- dlgMessage("Do you like oranges?", "yesnocancel")$res
if (res == "cancel") cat("Ah, ah! You refuse to answer!\n")

## Simpler version with msgBox and okCancelBox
msgBox("Information message") # Use this to interrupt script and inform user
if (okCancelBox("Continue?")) cat("we continue\n") else cat("stop it!\n")
}

\keyword{ misc }

\concept{ GUI API dialog boxes }
