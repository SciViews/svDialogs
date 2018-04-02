#' Dispaly a modal message box.
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
  if (!gui$startUI("dlgMessage", call = match.call(), default = "ok",
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
  msg <- paste("No workable method available to display a message dialog box using:",
    paste(guiWidgets(gui), collapse = ", "))
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
  res <- switch(Sys.info()["sysname"],
    Windows = .win_dlg_message(gui$args$message, gui$args$type),
    Darwin = .mac_dlg_message(gui$args$message, gui$args$type),
    .unix_dlg_message(gui$args$message, gui$args$type)
  )

  # Do we need to further dispatch?
  if (is.null(res)) {
    NextMethod("dlgMessage", gui)
  } else {
    gui$setUI(res = res, status = NULL)
    invisible(gui)
	}
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
    stop("type can only be 'ok'n 'okcancel', 'yesno', 'yesnocancel'"))

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
"yesnocancel")) {
  # TODO: escape single and double quotes in message
  # zenity must be installed on this machine!
  if (Sys.which("zenity") == "")
    return(NULL)
  type <- match.arg(type)
  if (type == "ok") {
    alarm()
    msg <- paste("zenity --info --text=\"", message,
      "\" --title=\"Information\"", sep = "")
    res <- system(msg)
    if (res > 0) {
      return(NULL)
    } else {
      return(invisible("ok"))
    }
  } else if (type == "yesnocancel") {
    type <- "yesno"
    confirm <- TRUE
  } else confirm <- FALSE
  # Now, we have only "okcancel" or "yesno"
  if (type == "okcancel") {
    msg <- paste0("zenity --question --text=\"", message,
      "\" --ok-label=\"OK\" --cancel-label=\"Cancel\" --title=\"Question\"")
      results <- c("ok", "cancel")
  } else {
    msg <- paste0("zenity --question --text=\"", message,
      "\" --ok-label=\"Yes\" --cancel-label=\"No\" --title=\"Question\"")
    results <- c("yes", "no")
  }
  res <- system(msg)
  if (res > 1) {
    return(NULL)
  } else {
    res <- results[res + 1]
  }
  # Do we ask to continue (if was yesnocancel)?
  if (confirm) {
    conf <- system(paste("zenity --question --text=\"Continue?\"",
      "--ok-label=\"OK\" --cancel-label=\"Cancel\" --title=\"Confirm\""))
    if (conf == 1)
      return("cancel")
  }
  res
}
