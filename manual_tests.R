# These are manual tests for svDialogs modal boxes.
# They cannot be automated (yet) and should be run on Windows, MacOS & Linux
# using various interfaces (RGui, R.app, JGR, RStudio Deskto & Server, terminal)
# once in a while, and before submitting a new version of the package to CRAN.
# Copyright (c) 2018, Ph. Grosjean <phgrosjean@sciviews.org>
# License: see the license for the repo at https://github.com/SciViews/svDialogs

library(svGUI)
library(svDialogs)
# We use the default .GUI with nativeGUI, but also, a text-only version:
gui_add("CLI", widgets = "textCLI", ask = TRUE)


# dlg_dir() -----------------------------------------------------------------
# Tested 2018-04-03 on R 3.4.4 MacOS High Sierra R.app & Terminal & RStudio Desktop 1.1.442
# Tested 2018-04-03 on R 3.4.4 Windows 10 RGui.exe & RTerm.exe & RStudio Desktop 1.1.442
#        Default directory is not always honored! See ?choose.dir
# Tested 2018-04-03 on R 3.4.4 svBox2018b preview terminal & RStudio Server 1.1.442
dlg_dir()$res
dlg_dir(default = tempdir(), title = "Select a temporary directory")$res
dlg_dir(title = "Click CANCEL to check it")$res
dlg_dir(title = "Choose dir (textual version)", gui = CLI)$res
dlg_dir(title = "Type 0 to cancel it", gui = CLI)$res


# dlg_open() ----------------------------------------------------------------
#.rs.api.selectFile(caption = "Select File", label = "Select", path = , filter = NULL, existing = TRUE)

# dlg_save() ----------------------------------------------------------------


# dlg_message() -------------------------------------------------------------
#.rs.api.showDialog(title = "Title", message = "Msg", url = "")
#.rs.api.showQuestion(title = "Title", message = "Message", ok = "", cancel = "")


# dlg_input() ---------------------------------------------------------------
#.rs.api.showPrompt(title = "Title", message = "Message", default = "Default")


# dlg_list() ---------------------------------------------------------------


# dlg_form() ----------------------------------------------------------------
