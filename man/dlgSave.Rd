\name{dlgSave}
\alias{dlgSave}
\alias{dlgSave.gui}
\alias{dlgSave.textCLI}
\alias{dlgSave.nativeGUI}

\title{ A select file to save dialog box }
\description{
  Allows to easily select one filename to save data.
}

\usage{
dlgSave(default, title, filters = dlgFilters["All", ], \dots, gui = .GUI)

## These should not be called directly
\method{dlgSave}{gui}(default, title, filters = dlgFilters["All", ], \dots, gui = .GUI)
\method{dlgSave}{textCLI}(default, title, filters = dlgFilters["All", ], \dots, gui = .GUI)
\method{dlgSave}{nativeGUI}(default, title, filters = dlgFilters["All", ], \dots, gui = .GUI)
}

\arguments{
  \item{default}{ the default file to start with (use \code{/dir/*} or
    \code{/dir/*.*} to start in a given directory, but without predefined name). }
  \item{title}{ a title to display on top of the dialog box. }
  \item{filters}{ a specification of file filters as a nx2 matrix, or a
    character string with even number of items. First items is the label, second
    one is the filter. See \code{dlgFilters for examples}. This is currently
    ignored on Mac OS X, since such kind of filter is defined differently there. }
  \item{\dots}{ pass further arguments to methods. }
  \item{gui}{ the 'gui' object concerned by this dialog box. }
}

\value{
  The modified 'gui' object is returned invisibly. The choosen file, or an
  empty string (if the cancel button was clicked or confirmation was
  cancelled) is placed in \code{gui$res} (see example). For existing files,
  confirmation is always asked!
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{dlgSave}},  \code{\link{dlgFilters}}, \code{\link{dlgDir}} }

\examples{
## Choose one R filename to save some R script into it
dlgSave(title = "Save R script to", filters = dlgFilters[c("R", "All"), ])$res
}

\keyword{ misc }

\concept{ GUI API dialog boxes }
