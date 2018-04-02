\name{dlgDir}
\alias{dlgDir}
\alias{dlgDir.gui}
\alias{dlgDir.textCLI}
\alias{dlgDir.nativeGUI}

\title{ A select directory dialog box }
\description{
  Allows to easily select a directory.
}

\usage{
dlgDir(default = getwd(), title, \dots, gui = .GUI)

## These should not be called directly
\method{dlgDir}{gui}(default = getwd(), title, \dots, gui = .GUI)
\method{dlgDir}{textCLI}(default = getwd(), title, \dots, gui = .GUI)
\method{dlgDir}{nativeGUI}(default = getwd(), title, \dots, gui = .GUI)
}

\arguments{
  \item{default}{ the directory to start with. }
  \item{title}{ a title to display on top of the dialog box. }
  \item{\dots}{ pass further arguments to methods. }
  \item{gui}{ the 'gui' object concerned by this dialog box. }
}

\value{
  The modified 'gui' object is returned invisibly. The choosen directory, or an
  empty string if the cancel button was clicked is found in \code{gui$res} (see
  example).
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{dlgOpen}},  \code{\link{dlgSave}} }

\examples{
### A quick default directory changer
setwd(dlgDir(default = getwd())$res)
}

\keyword{ misc }

\concept{ GUI API dialog boxes }
