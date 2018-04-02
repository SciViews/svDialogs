\name{dlgOpen}
\alias{dlgOpen}
\alias{dlgOpen.gui}
\alias{dlgOpen.textCLI}
\alias{dlgOpen.nativeGUI}
\alias{dlgFilters}

\title{ A select file(s) to open dialog box }
\description{
  Allows to easily select one or more existings files.
}

\usage{
dlgOpen(default, title, multiple = FALSE, filters = dlgFilters["All", ],
    \dots, gui = .GUI)
dlgFilters

## These should not be called directly
\method{dlgOpen}{gui}(default, title, multiple = FALSE, filters = dlgFilters["All", ],
    \dots, gui = .GUI)
\method{dlgOpen}{textCLI}(default, title, multiple = FALSE, filters = dlgFilters["All", ],
    \dots, gui = .GUI)
\method{dlgOpen}{nativeGUI}(default, title, multiple = FALSE, filters = dlgFilters["All", ],
    \dots, gui = .GUI)
}

\arguments{
  \item{default}{ the default file to start with (use \code{/dir/*} or
    \code{/dir/*.*} to start in a given directory). }
  \item{title}{ a title to display on top of the dialog box. }
  \item{multiple}{ is a multiple selection of files allowed? }
  \item{filters}{ a specification of file filters as a nx2 matrix, or a
    character string with even number of items. First items is the label, second
    one is the filter. See \code{dlgFilters for examples}. This is currently
    ignored on Mac OS X, since such kind of filter is defined differently there. }
  \item{\dots}{ pass further arguments to methods. }
  \item{gui}{ the 'gui' object concerned by this dialog box. }
}

\value{
  The modified 'gui' object is returned invisibly. The choosen file(s), or an
  empty string if the cancel button was clicked is found in \code{gui$res} (see
  example).
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{dlgSave}},  \code{\link{dlgDir}} }

\examples{
## Choose one R file
dlgOpen(title = "Select one R file", filters = dlgFilters[c("R", "All"), ])$res
## Choose several files
dlgOpen(multiple = TRUE)$res
}

\keyword{ misc }

\concept{ GUI API dialog boxes }
