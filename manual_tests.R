# These are manual tests for svDialogs modal boxes.
# They cannot be automated (yet) and should be run on Windows, MacOS & Linux
# using various interfaces (RGui, R.app, JGR, RStudio Deskto & Server, terminal)
# once in a while, and before submitting a new version of the package to CRAN.
# Copyright (c) 2018, Ph. Grosjean <phgrosjean@sciviews.org>
# License: see the license for the repo at https://github.com/SciViews/svDialogs

# Tested 2018-04-24 on R 3.4.4 MacOS High Sierra R.app & Terminal &
#                      RStudio Desktop 1.1.447
# Tested 2018-04-24 on R 3.4.4 Windows 10 RGui.exe & RTerm.exe &
#                      RStudio Desktop 1.1.447
#        Default directory is not always honored in dlg_dir()! See ?choose.dir
# Tested 2018-04-24 on R 3.4.4 svBox2018b preview terminal &
#                      RStudio Server 1.1.447
#        + ssh session with and without X11 (-Y argument)

library(svGUI)
library(svDialogs)
# We use the default .GUI with nativeGUI, but also, a text-only version:
gui_add("CLI", widgets = "textCLI", ask = TRUE)


# dlg_dir() -----------------------------------------------------------------
dlg_dir()$res
dlg_dir(default = tempdir(), title = "Select a temporary directory")$res
dlg_dir(title = "Click CANCEL to check it")$res
dlg_dir(title = "Choose dir (textual version)", gui = CLI)$res
dlg_dir(title = "Type 0 to cancel it", gui = CLI)$res
dlg_dir(zenity = TRUE)$res


# dlg_open() ----------------------------------------------------------------
dlg_open()$res
dlg_open(default = system.file("DESCRIPTION"),
  title = "Select an R system file", multiple = FALSE)$res
dlg_open(title = "Select an R or S script file",
  filters = dlg_filters["R", ])$res
dlg_open(title = "Select an R script file",
  filters = c("R script file (*.R)", "*.R"))$res
dlg_open(title = "Click CANCEL to check it")$res
dlg_open(title = "Choose a file (textual version)", gui = CLI)$res
dlg_open(title = "Type 0 to cancel it", gui = CLI)$res
# Multiple selection **does not** work yet on RStudio!!!
dlg_open(title = "Select multiple files", multiple = TRUE)$res
dlg_open(title = "Select multiple files", multiple = TRUE, zenity = TRUE)$res
dlg_open(title = "Select multiple files (text)", multiple = TRUE, gui = CLI)$res
dlg_open(zenity = TRUE)$res


# dlg_save() ----------------------------------------------------------------
dlg_save()$res
dlg_save(default = tempfile(), title = "Create a temporary file")$res
dlg_save(title = "Click CANCEL to check it")$res
dlg_save(default = system.file("DESCRIPTION"),
  title = "Select an existing file, then say OK on confirmation")$res
dlg_save(default = system.file("DESCRIPTION"),
  title = "Select an existing file, then click Cancel on confirmation")$res
dlg_save(title = "Choose a file (textual version)", gui = CLI)$res
dlg_save(title = "Type 0 to cancel it", gui = CLI)$res
dlg_save(default = system.file("DESCRIPTION"),
  title = "Choose an existing file, then confirm yes (text)", gui = CLI)$res
dlg_save(default = system.file("DESCRIPTION"),
  title = "Choose an existing file, then cancel the confirmation box (text)",
  gui = CLI)$res
dlg_save(zenity = TRUE)$res


# dlg_message() -------------------------------------------------------------
dlg_message()$res
dlg_message(message = "My message", type = "ok")$res
dlg_message(message = "My message (hit OK)", type = "okcancel")$res
dlg_message(message = "My message (hit Cancel)", type = "okcancel")$res
dlg_message(message = "My message (close the window)", type = "okcancel")$res
dlg_message(message = "My message (hit Yes)", type = "yesno")$res
dlg_message(message = "My message (hit No)", type = "yesno")$res
dlg_message(message = "My message (close the window)", type = "yesno")$res
dlg_message(message = "My message (hit Yes)", type = "yesnocancel")$res
dlg_message(message = "My message (hit No)", type = "yesnocancel")$res
dlg_message(message = "My message (hit Cancel)", type = "yesnocancel")$res
dlg_message(message = "My message (close the window)", type = "yesnocancel")$res
dlg_message(message = "My message", type = "ok", gui = CLI)$res
dlg_message(message = "My message", type = "okcancel", gui = CLI)$res
dlg_message(message = "My message", type = "yesno", gui = CLI)$res
dlg_message(message = "My message", type = "yesnocancel", gui = CLI)$res
dlg_message(zenity = TRUE)$res
dlg_message(message = "My message", type = "yesnocancel", zenity = TRUE)$res
# zenity does not have three-button boxes, so, we need to work in two passes
msg_box("A message (click OK)")
msg_box("A message (close the window)")
ok_cancel_box("A message (click OK)")
ok_cancel_box("A message (click Cancel)")
ok_cancel_box("A message (close the window)")


# dlg_input() ---------------------------------------------------------------
dlg_input()$res
dlg_input(message = "Enter nothing, click OK")$res
dlg_input(message = "Enter nothing, click Cancel")$res
dlg_input(message = "Enter something, click Cancel")$res
dlg_input(message = "There is a default value (apple)", default = "apple")$res
dlg_input(zenity = TRUE)$res
dlg_input(message = "Enter something, click Enter", gui = CLI)$res
dlg_input(message = "Enter nothing, click Enter", gui = CLI)$res
dlg_input(message = "Use default click Enter", default = "apple", gui = CLI)$res
dlg_input(message = "Cancel this", gui = CLI)$res


# dlg_list() ---------------------------------------------------------------
dlg_list(month.name)$res
dlg_list(month.name, title = "June preselected", preselect = month.name[6])$res
dlg_list(month.name, title = "Click Cancel")$res
dlg_list(month.name, title = "Close the window")$res
dlg_list(month.name, title = "Close the window", multiple = TRUE)$res
dlg_list(month.name, title = "Select multiple items", multiple = TRUE)$res
dlg_list(month.name, title = "Preselection (Mar, Jun, Nov)",
  preselect = month.name[c(3, 6, 11)], multiple = TRUE)$res
dlg_list(month.name, gui = CLI)$res
dlg_list(month.name, multiple = TRUE, gui = CLI)$res
dlg_list(month.name, title = "Enter 0 to cancel", gui = CLI)$res
dlg_list(month.name, title = "Jun & Nov preselected",
  preselect = month.name[c(6, 11)], multiple = TRUE, gui = CLI)$res
dlg_list(month.name, title = "zenity version", zenity = TRUE)$res
dlg_list(month.name, title = "zenity version", multiple = TRUE, zenity = TRUE)$res

# dlg_form() ----------------------------------------------------------------
# Only for graphical version under Linux, currently
form <- list(
  "Name:TXT" = "John Smith",
  "Age:NUM" = 25,
  "Sex:CB" = c("male", "female"),
  "Married:CHK" = FALSE
)
dlg_form(form, title = "My form", message = "An example free form")$res
