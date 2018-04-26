#' Display a modal message box.
#'
#' A message box with icon, text, and one to three buttons.
#'
#' @param message The message to display in the dialog box. Use `\\n` for line
#' break, or provide a vector of character strings, one for each line.
#' @param type The type of dialog box: `'ok'`, `'okcancel'`, `'yesno'` or
#' `'yesnocancel'`.
#' @param ... Pass further arguments to methods.
#' @param gui The 'gui' object concerned by this dialog box.
#' @return The modified 'gui' object is returned invisibly. A string with the
#' name of the button (`"ok"`, `"cancel"`, `"yes"` or `"no"`) that the user
#' pressed can be obtained from `gui$res` (see example).
#' `msg_box()` just returns the name of the button (`"ok"`), while
#' `ok_cancel_box()` returns `TRUE` if "ok" was clicked or `FALSE` if "cancel"
#' was clicked.
#' @note On 'RStudio' or with 'zenity' under Linux, only two buttons are
#' available. So, when using `type = "yesnocancel"`, two successive dialog boxes
#' are displayed: one with the message and `'yes'`/`'no'` buttons, and a second
#' one asking to continue, and if the user clicks `'no'`, the function returns
#' `"cancel"`. This is clearly sub-optimal. So, for a clean experience on all
#' supported platforms, try to avoid `'yesnocancel'` as much as possible.
#' @export
#' @name dlg_message
#' @seealso [dlg_list()], [dlg_input()]
#' @keywords misc
#' @concept Modal dialog box
#' @examples
#' \dontrun{
#' # A simple information box
#' dlg_message("Hello world!")$res
#'
#' # Ask to continue
#' dlg_message(c("This is a long task!", "Continue?"), "okcancel")$res
#'
#' # Ask a question
#' dlg_message("Do you like apples?", "yesno")$res
#'
#' # Idem, but one can interrupt too
#' res <- dlg_message("Do you like oranges?", "yesnocancel")$res
#' if (res == "cancel")
#'   cat("Ah, ah! You refuse to answer!\n")
#'
#' # Simpler version with msgBox and okCancelBox
#' msg_box("Information message") # Use this to interrupt script and inform user
#' if (ok_cancel_box("Continue?")) cat("we continue\n") else cat("stop it!\n")
#' }
dlg_message <- function(message, type = c("ok", "okcancel", "yesno",
"yesnocancel"), ..., gui = .GUI) {
  # Define the S3 method
  if (!gui$startUI("dlg_message", call = match.call(), default = "ok",
    msg = "Displaying a modal message dialog box",
    msg.no.ask = "A modal message dialog box was by-passed"))
    return(invisible(gui))

  # Check and rework main arguments and place them in gui$args
  if (missing(message))
    message <- "[Your message here...]"
  message <- paste(as.character(message), collapse = "\n")
  type <- match.arg(type)
  gui$setUI(args = list(message = message, type = type))

  # ... and dispatch to the method
  UseMethod("dlgMessage", gui)
}

#' @export
#' @rdname dlg_message
dlgMessage <- dlg_message # Backward compatibility

#' @export
#' @rdname dlg_message
msg_box <- function(message) {
  # Simplified versions of dlg_message()
  dlg_message(message = message)$res
}

#' @export
#' @rdname dlg_message
msgBox <- msg_box # Backward compatibility

#' @export
#' @rdname dlg_message
ok_cancel_box <- function(message) {
  dlg_message(message = message, type = "okcancel")$res == "ok"
}

#' @export
#' @rdname dlg_message
okCancelBox <- ok_cancel_box # Backward compatibility

#' @export
#' @rdname dlg_message
dlgMessage.gui <- function(message, type = c("ok", "okcancel", "yesno",
"yesnocancel"), ..., gui = .GUI) {
  # Used to break the chain of NextMethod(), searching for a usable method
  # in the current context
  msg <- paste("No workable method available to display",
    "a message dialog box using:")
  msg <- paste(msg, paste(guiWidgets(gui), collapse = ", "))
  gui$setUI(status = "error", msg = msg, widgets = "none")
  stop(msg)
}

#' @export
#' @rdname dlg_message
dlgMessage.textCLI <- function(message, type = c("ok", "okcancel", "yesno",
"yesnocancel"), ..., gui = .GUI) {
  # The pure textual version used a fallback in case no GUI could be used
  gui$setUI(widgets = "textCLI")
  # Display the message and wait for user action
  if (gui$args$type == "ok") {
    readline(paste0(gui$args$message, "\n(hit ENTER to continue) "))
    res <- "ok"
  } else {
    # Use a non-graphical select.list() for the others
    choices <- switch(gui$args$type,
      okcancel = c("ok", "cancel"),
      yesno = c("yes", "no"),
      yesnocancel = c("yes", "no", "cancel")
    )
    res <- select.list(choices, title = gui$args$message, graphics = FALSE)
    if (res == "" && type != "yesno") res <- "cancel"
    if (res == "") res <- "no" # Selection of 0 with yes/no => no
  }
  gui$setUI(res = res, status = NULL)
  invisible(gui)
}

#' @export
#' @rdname dlg_message
dlgMessage.nativeGUI <- function(message, type = c("ok", "okcancel", "yesno",
"yesnocancel"), ..., gui = .GUI) {
  # The native version of the message box
  gui$setUI(widgets = "nativeGUI")
  # A simple message box
  # type can be 'ok' (info), 'okcancel', 'yesno', 'yesnocancel' (question)
  # This dialog box is always modal
  # Returns invisibly a character with the button that was pressed
  if (.is_rstudio()) syst <- "RStudio" else syst <- Sys.info()["sysname"]
  res <- switch(syst,
    RStudio = .rstudio_dlg_message(gui$args$message, gui$args$type),
    Windows = .win_dlg_message(gui$args$message, gui$args$type),
    Darwin = .mac_dlg_message(gui$args$message, gui$args$type),
    .unix_dlg_message(gui$args$message, gui$args$type, ...)
  )

  # Do we need to further dispatch?
  if (is.null(res)) {
    NextMethod("dlgMessage", gui)
  } else {
    gui$setUI(res = res, status = NULL)
    invisible(gui)
	}
}

# RStudio version (need at least version 1.1.67)
# No yesnocancel box => ask in two stages (ugly, but what to do?)
.rstudio_dlg_message <- function(message, type = c("ok", "okcancel", "yesno",
"yesnocancel")) {
  if (rstudioapi::getVersion() < '1.1.67')
    return(NULL)
  type <- match.arg(type)
  if (type == "ok") {
    alarm()
    rstudioapi::showDialog(title = "R Message", message = message,
      url = "")
    return(invisible("ok"))
  } else if (type == "yesnocancel") {
    type <- "yesno"
    confirm <- TRUE
  } else confirm <- FALSE
  # Now, we have only "okcancel" or "yesno"
  if (type == "okcancel") {
    res <- rstudioapi::showQuestion(title = "R Question", message = message,
      ok = "OK", cancel = "Cancel")
    if (res) res <- "ok" else res <- "cancel"
  } else {
    res <- rstudioapi::showQuestion(title = "R Question", message = message,
      ok = "Yes", cancel = "No")
    if (res) res <- "yes" else res <- "no"
  }
  # Do we ask to continue (if was yesnocancel)?
  if (confirm) {
    res2 <- rstudioapi::showQuestion(title = "R Question",
      message = "Continue?", ok = "Yes", cancel = "No")
    if (!res2) res <- "cancel"
  }
  res
}

# Windows version
.win_dlg_message <- function(message, type = c("ok", "okcancel", "yesno",
"yesnocancel")) {
  res <- winDialog(type = type, message = message)
  # Rework result to match the result from the other functions
  if (type == "ok") {
    invisible("ok")
  } else {
    tolower(res)
  }
}

# MacOS version
.mac_dlg_message <- function(message, type= c("ok", "okcancel", "yesno",
"yesnocancel")) {
  message <- .replace_quotes(message)
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
  type <- match.arg(type)
  buttons <- switch(type,
    ok = "\"OK\"",
    okcancel = "\"Cancel\",\"OK\"",
    yesno = "\"No\",\"Yes\"",
    yesnocancel = ",\"Cancel\",\"No\",\"Yes\"",
    stop("type can only be 'ok', 'okcancel', 'yesno', 'yesnocancel'"))

  if (type == "ok") {
    beep <- " -e 'beep'"
    icon <- "caution"
    title <- "\"Information\""
    more <- " default button 1"
  } else {
    beep <- ""
    icon <- "note"
    title <- "\"Question\""
    if (type == "yesnocancel")
      more <- " default button 3 cancel button 1" else
    if (type == "yesno") more <- " default button 2" else
      more <- " default button 2 cancel button 1"
  }
  # TODO: Escape single and double quotes in message
  cmd <- paste0("exit `osascript", beep, " -e 'tell application ", app,
    " to set dlg to display dialog \"", message, "\" with title ", title,
    more, " with icon ", icon, " buttons {", buttons,
    "}' -e 'if button returned of dlg is \"No\" then 2' 2> /dev/null`")
  res <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = TRUE)
  # Decrypt result
  if (type == "ok") {
    if (res > 0) {
      return(NULL)
    } else {
      return(invisible("ok"))
    }
  }
  if (res == 2)
    return("no")
  if (res == 1)
    return("cancel")
  if (type == "okcancel") {
    return("ok")
  } else {
    return("yes")
  }
}

# Linux/Unix version
.unix_dlg_message <- function(message, type = c("ok", "okcancel", "yesno",
"yesnocancel"), zenity = FALSE) {
  message <- .escape_quotes(message)
  exec <- .get_yad_or_zenity(zenity)
  if (exec == "")
    return(NULL) # Try next method
  is_yad <- attr(exec, "is_yad")
  exec <- as.character(exec)

  type <- match.arg(type)
  if (type == "ok") {
    alarm()
    if (is_yad) {
      msg <- paste("'", exec, "' --image=gtk-dialog-info --text=\"", message,
        "\" --title=\"Information\" --button=OK:0 --on-top --skip-taskbar",
        sep = "")
    } else {# zenity
      msg <- paste("'", exec, "' --info --text=\"", message,
        "\" --title=\"Information\"", sep = "")
    }
    system(msg)
    return("ok")
  } else if (is_yad) {
    msg <- switch(type,
      yesno = paste0("'", exec, "' --image=gtk-dialog-question --text=\"",
        message, "\" --button=No:1 --button=Yes:0 --title=\"Question\"",
        "--on-top --skip-taskbar"),
      okcancel = paste0("'", exec, "' --image=gtk-dialog-question --text=\"",
        message, "\" --button=Cancel:1 --button=OK:0 --title=\"Question\"",
        "--on-top --skip-taskbar"),
      yesnocancel = paste0("'", exec, "' --image=gtk-dialog-question --text=\"",
        message, "\" --button=Cancel:2 --button=No:1 --button=Yes:0",
        " --title=\"Question\" --on-top --skip-taskbar"),
      stop("unknown type"))
    results <- switch(type,
      yesno = c("yes", "no"),
      okcancel = c("ok", "cancel"),
      yesnocancel = c("yes", "no", "cancel"),
      stop("unknown type"))
    res <- system(msg)
    if (res > length(results) - 1)
      res <- length(results) - 1 # Use last item by default
    res <- results[res + 1]
  } else {# This is zenity
    if (type == "yesnocancel") {
      type <- "yesno"
      confirm <- TRUE
    } else confirm <- FALSE
    # Now, we have only "okcancel" or "yesno"
    if (type == "okcancel") {
      msg <- paste0("'", exec, "' --question --text=\"", message,
        "\" --ok-label=\"OK\" --cancel-label=\"Cancel\" --title=\"Question\"")
      results <- c("ok", "cancel")
    } else {
      msg <- paste0("'", exec, "' --question --text=\"", message,
        "\" --ok-label=\"Yes\" --cancel-label=\"No\" --title=\"Question\"")
      results <- c("yes", "no")
    }
    res <- system(msg)
    if (res > 1) res <- 1
    res <- results[res + 1]
    # Do we ask to continue (if was yesnocancel)?
    if (confirm) {
      conf <- system(paste0("'", exec, "' --question --text=\"Continue?\"",
        " --ok-label=\"OK\" --cancel-label=\"Cancel\" --title=\"Confirm\""))
      if (conf == 1)
        res <- "cancel"
    }
  }
  res
}
