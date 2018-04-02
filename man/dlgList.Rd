\name{dlgList}
\alias{dlgList}
\alias{dlgList.gui}
\alias{dlgList.textCLI}
\alias{dlgList.nativeGUI}

\title{ A list selection dialog box }
\description{
  Select one or several items in a list.
}

\usage{
dlgList(choices, preselect = NULL, multiple = FALSE, title = NULL,
    \dots, gui = .GUI)
   
## These should not be called directly
\method{dlgList}{gui}(choices, preselect = NULL, multiple = FALSE, title = NULL,
    \dots, gui = .GUI)
\method{dlgList}{textCLI}(choices, preselect = NULL, multiple = FALSE, title = NULL,
    \dots, gui = .GUI)
\method{dlgList}{nativeGUI}(choices, preselect = NULL, multiple = FALSE, title = NULL,
    \dots, gui = .GUI)
}

\arguments{
  \item{choices}{ the list of items. It is coerced to character strings. }
  \item{preselect}{ a list of preselections, or \code{NULL} (then, the first
    element is selected in the list). Preselections not in choices are
    tolerated (and disregarded, of course). }
  \item{multiple}{ is it a multiple selection dialog box? }
  \item{title}{ the title of the dialog box, or \code{NULL}. }
  \item{\dots}{ pass further arguments to methods. }
  \item{gui}{ the 'gui' object concerned by this dialog box. }
}

\value{
  The modified 'gui' object is returned invisibly. A list with selected items,
  or a character vector of length 0 if the dialog box was cancelled is available
  from \code{gui$res} (see examples).
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

%\seealso{ \code{\link{dlgItemSel}}, \code{\link{dlgVarSel}} }

\examples{
## Select a month
res <- dlgList(month.name, multiple = TRUE)$res
if (!length(res)) {
  cat("You cancelled the choice\n")
} else {
  cat("You selected:\n")
  print(res)
}
}

\keyword{ misc }

\concept{ GUI API dialog boxes }
