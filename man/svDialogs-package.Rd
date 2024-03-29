% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svDialogs-package.R
\docType{package}
\name{svDialogs-package}
\alias{svDialogs}
\alias{svDialogs-package}
\title{svDialogs: 'SciViews' - Standard Dialog Boxes for Windows, MacOS and Linuxes}
\description{
Quickly construct standard dialog boxes for your GUI, including 
  message boxes, input boxes, list, file or directory selection, ... In case R
  cannot display GUI dialog boxes, a simpler command line version of these
  interactive elements is also provided as fallback solution.
}
\details{
The dialog box function dispatch to specific dialog boxes depending on the OS
and whether a GUI can be displayed or not.
}
\note{
Under Linux, you should install \strong{'yad'} (preferably), or
\strong{'zenity'}. If you use 'zenity', you cannot build more complex \code{\link[=dlg_form]{dlg_form()}}
dialog boxes, and some versions trigger a warning like: "Gtk-Message:
GtkDialog mapped without a transient parent. This is discouraged." You do not
get these inconveniences if you use 'yad' (default choice in case both
programs are installed simultaneously).
}
\section{Important functions}{

\itemize{
\item \code{\link[=dlg_message]{dlg_message()}} display a message box,
\item \code{\link[=dlg_input]{dlg_input()}} prompt for textual input,
\item \code{\link[=dlg_list]{dlg_list()}} select one or more items in a list,
\item \code{\link[=dlg_open]{dlg_open()}} open one or more existing file(s),
\item \code{\link[=dlg_save]{dlg_save()}} prompt for a file to save to (and ask confirmation if the file
already exists),
\item \code{\link[=dlg_dir]{dlg_dir()}} select a directory,
\item \code{\link[=dlg_form]{dlg_form()}} display a complex dialog box with textual areas, password,
checkboxes, comboboxes, font and size, color, etc.
}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/SciViews/svDialogs}
  \item \url{https://www.sciviews.org/svDialogs/}
  \item Report bugs at \url{https://github.com/SciViews/svDialogs/issues}
}

}
\author{
\strong{Maintainer}: Philippe Grosjean \email{phgrosjean@sciviews.org} (\href{https://orcid.org/0000-0002-2694-9471}{ORCID})

Other contributors:
\itemize{
  \item Paul Hibbins \email{paulhibbing@gmail.com} [contributor]
}

}
\keyword{internal}
