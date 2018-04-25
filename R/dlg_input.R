#' Modal dialog to input a string or a value.
#'
#' Prompt for some data in a modal dialog box.
#'
#' @param message The message to display in the dialog box. Use `\\n` for
#' line break, or provide a vector of character strings, one for each line.
#' @param default The default value in the text box. Single string or `NULL`.
#' @param ... Pass further arguments to methods.
#' @param gui The 'gui' object concerned by this dialog box.
#' @return The modified 'gui' object is returned invisibly. The text entered by
#' the user at the input box, or an empty string if the dialog box was cancelled
#' can be obtained from `gui$res` (see example).
#' @export
#' @name dlg_input
#' @seealso [dlg_list()], [dlg_form()], [dlg_message()]
#' @keywords misc
#' @concept Modal dialog box
#' @examples
#' \dontrun{
#' # Ask something...
#' user <- dlg_input("Who are you?", Sys.info()["user"])$res
#' if (!length(user)) {# The user clicked the 'cancel' button
#'   cat("OK, you prefer to stay anonymous!\n")
#' } else {
#'   cat("Hello", user, "\n")
#' }
#' }
dlg_input <- function(message = "Enter a value", default = "", ...,
gui = .GUI) {
  # Define the S3 method
  if (!gui$startUI("dlg_input", call = match.call(), default = default,
    msg = "Displaying a modal input dialog box",
    msg.no.ask = "A modal input dialog box was by-passed"))
    return(invisible(gui))

  # Check and rework main arguments and place them in gui$args
  if (!length(message))
    message <- "Enter a value"
  message <- paste(as.character(message), collapse = "\n")
  if (is.null(default)) {
    default <- ""
  } else {
    default <- as.character(default)[1]
  }
  gui$setUI(args = list(message = message, default = default))

  # ... and dispatch to the method
  UseMethod("dlgInput", gui)
}

#' @export
#' @rdname dlg_input
dlgInput <- dlg_input # Backward compatibility

#' @export
#' @rdname dlg_input
dlgInput.gui <- function(message = "Enter a value", default = "", ...,
gui = .GUI) {
  # Used to break the chain of NextMethod(), searching for a usable method
  # in the current context
  msg <- paste("No workable method available to display",
    "an input dialog box using:")
  msg <- paste(msg, paste(guiWidgets(gui), collapse = ", "))
  gui$setUI(status = "error", msg = msg, widgets = "none")
  stop(msg)
}

#' @export
#' @rdname dlg_input
dlgInput.textCLI <- function(message = "Enter a value", default = "", ...,
gui = .GUI) {
  # The pure textual version used a fallback in case no GUI could be used
  gui$setUI(widgets = "textCLI")
  # Ask for the input with readline()
  res <- readline(paste0(gui$args$message,
    " [", gui$args$default, "] or 00 to Cancel: "))
  if (res == "")
    res <- gui$args$default
  if (res == "00")
    res <- character(0)
  gui$setUI(res = res, status = NULL)
  invisible(gui)
}

#' @export
#' @rdname dlg_input
dlgInput.nativeGUI <- function(message = "Enter a value", default = "", ...,
gui = .GUI) {
  # The native version of the input box
  gui$setUI(widgets = "nativeGUI")
  # A simple text input box using native window
  # Return either a string, or character(0) if 'Cancel' clicked
  if (.is_rstudio()) syst <- "RStudio" else syst <- Sys.info()["sysname"]
  res <- switch(syst,
    RStudio = .rstudio_dlg_input(gui$args$message, gui$args$default),
    Windows = .win_dlg_input(gui$args$message, gui$args$default),
    Darwin = .mac_dlg_input(gui$args$message, gui$args$default),
    .unix_dlg_input(gui$args$message, gui$args$default, ...)
  )

  # Do we need to further dispatch?
  if (is.null(res)) {
    NextMethod("dlgInput", gui)
  } else {
    gui$setUI(res = res, status = NULL)
    invisible(gui)
  }
}

# RStudio version (need at least version 1.1.67)
.rstudio_dlg_input <- function(message, default) {
  if (rstudioapi::getVersion() < '1.1.67')
    return(NULL)
  res <- rstudioapi::showPrompt(title = "R prompt", message = message,
    default = default)
  if (is.null(res)) {
    character(0)
  } else{
    res
  }
}

# Windows version
.win_dlg_input <- function(message, default) {
  res <- winDialogString(message = message, default = default)
  if (is.null(res)) {
    character(0)
  } else {
    res
  }
}

# MacOS version
.mac_dlg_input <- function(message, default) {
  # Display a modal message with native Mac dialog box
  #if (.Platform$GUI == "AQUA") app <- "(name of application \"R\")" else
  # This works from Mac OS X 10.5 Leopard:
  if (.Platform$GUI == "AQUA") {
    app <- "(name of application id \"Rgui\")"
  } else if (.is_jgr()) {
    app <- "\"JGR\""
  } else {
    app <- "\"Terminal\""
  }
  # Avoid displaying warning message when the user clicks on 'Cancel'
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  cmd <- paste0("-e 'tell application ", app,
    " to display dialog \"", message, "\" default answer \"", default,
    "\" with title \"Question\" buttons {\"Cancel\",\"OK\"} cancel button 1",
    " default button 2'")
  # I cannot use system(intern = TRUE) with this in R.app/R64.app
  # (deadlock situation?), but I can in R run in a terminal. system2() also
  # works, but this preclue of using svDialogs on R < 2.12.0.
  # The hack is thus to redirect output to a file, then, to read the content
  # of that file and to desctroy it
  tfile <- tempfile()
  on.exit(unlink(tfile))
  res <- try(system(paste("osascript", cmd, ">", tfile), wait = TRUE,
    intern = FALSE, ignore.stderr = TRUE), silent = TRUE)
  if (inherits(res, "try-error") || !length(res))
    return(character(0))
  if (res > 0)
    return(character(0)) # User cancelled input
  res <- readLines(tfile)
  res <- sub("^.*text returned:", "", res)
  res <- sub(", button returned:.*$", "", res)
  # This is for an alternate return string on El Capitain
  res <- sub("^.*text returned:", "", res)
	paste(res, collapse = " ")
}

# Linux/Unix version
.unix_dlg_input <- function(message, default, zenity = FALSE) {
  message <- .escape_quotes(message)
  default <- .escape_quotes(default)
  exec <- .get_yad_or_zenity(zenity)
  if (exec == "")
    return(NULL) # Try next method
  is_yad <- attr(exec, "is_yad")
  exec <- as.character(exec)
  # Avoid displaying warning message in case user clicks on Cancel
  owarn <- getOption("warn")
  on.exit(options(warn = owarn))
  options(warn = -1)
  # Use zenity to display the prompt box
  msg <- paste0("'", exec, "' --entry --title=\"Question\" --text=\"", message,
    "\" --entry-text=\"", default, "\"")
  if (is_yad)
    msg <- paste(msg, "--on-top --skip-taskbar")
  res <- system(msg, intern = TRUE)
  attr(res, "status") <- NULL
  res
}
