\name{dlgForm}
\alias{dlgForm}
\alias{dlgForm.gui}
\alias{dlgForm.textCLI}
\alias{dlgForm.nativeGUI}

\title{ A flexible form dialog box }
\description{
  Fill a form in a dialog box.
}

\usage{
dlgForm(form, title = "Fill the form", message = NULL, columns = 1,
    strip.type = TRUE, \dots, gui = .GUI)

## These should not be called directly
\method{dlgForm}{gui}(form, title = "Fill the form", message = NULL, columns = 1,
    strip.type = TRUE, \dots, gui = .GUI)
\method{dlgForm}{textCLI}(form, title = "Fill the form", message = NULL, columns = 1,
    strip.type = TRUE, \dots, gui = .GUI)
\method{dlgForm}{nativeGUI}(form, title = "Fill the form", message = NULL, columns = 1,
    strip.type = TRUE, \dots, gui = .GUI)
}

\arguments{
  \item{form}{ named list of default values, or list of possible items. Names
    are the labels of corresponding entries in the dialog box, followed by an
    indicator of the type of entry to place in the dialog box (see details). }
  \item{title}{ the tile of the dialog box. }
  \item{message}{ an optional message to display in the dialog box. Use
    \code{\\n} for line break, or provide a vector of character strings, one for
    each line. }
  \item{columns}{ arrange the entries on this number of columns (by row). }
  \item{strip.type}{ do we strip the type from the names in results. }
  \item{\dots}{ pass further arguments to methods. }
  \item{gui}{ the 'gui' object concerned by this dialog box. }
}

\details{
  The form content is defined by a named list. Items are default values, or a
  list of possible items, e.g., for the combobox. Names are labels displayed
  in front of each field in the form. Follow them by a code that represents the
  type of entry you want to use:
  
  \code{:TXT} for simple (default) textual box,
  \code{:H} for hidden text (password),
  \code{:RO} for read-only text,
  \code{:NUM} for null of positive integers with up/down arrows,
  \code{:CHK} for checkbox: \code{TRUE} or \code{FALSE},
  \code{:CB} for read-only combobox,
  \code{:CBE} for editable combobox,
  \code{:FL} to select one existing file,
  \code{:MFL} to select multiple existing files,
  \code{:SFL} to select or create one file,
  \code{:DIR} to select a directory,
  \code{:CDIR} to select or create a directory,
  \code{:FN} to select font and font size, 
  \code{:DT} to enter a date,
  \code{:CLR} to enter a RGB color,
  \code{:BTN} to create a button that execute some code,
  \code{:LBL} to add a label.
  
  For the moment, the form dialog box is only supported on Linux. You have to
  install yad to get access to it. On Ubuntu, you do so by:
  \preformatted{
sudo add-apt-repository ppa:webupd8team/y-ppa-manager
sudo apt-get update
sudo apt-get install yad  
  }
}

\value{
  The modified 'gui' object is returned invisibly. The items entered by the
  user, or an empty list if the dialog box was cancelled can be
  obtained from \code{gui$res} (see example).
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{dlgInput}} } %, \code{\link{dlgText}} }

\examples{
## Ask a series of items at once in a dialog box
Form <- list(
    "Name:TXT" = "John Smith",
    "Age:NUM" = 25,
    "Sex:CB" = c("male", "female"),
    "Married:CHK"=FALSE
)
dlgForm(Form, "My form")$res

}

\keyword{ misc }

\concept{ GUI API dialog boxes }
