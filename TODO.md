# svDialogs To Do list

-   In GitHub actions, reimplement the travis after_success: Rscript -e 'covr::codecov()'

-   The \n and \t in menu commands result into n or t, not \n or \t in xvkbd cmd.

-   Create as many tests (testthat) as possible, given most of the code is supposed to be used interactively, and thus, is not (easily) testable automatically.

-   A complete vignette, with rationate, explanation of mechanism to dispatch dialog boxes, example of input dialog box on all supported platforms, and further explanation for all supported dialog boxes.

-   Reexpose useful svGUI functions.

-   Find a solution for `dlg_open(multiple = TRUE)` on RStudio (not supported yet).

-   Find a better solution for textual `dlg_open(multiple = TRUE)`: allow for globbing and for folder indication followed by a list selector of files in that directory that match filters.

-   Find a better solution for `dlg_message(type = "yesnocancel")` on RStudio (since I can only display two button, we first ask 'yes'/'no', then ask to confirm). This is the same with zenity, but since yad is advised on Linux, we don't care much.

-   Menus for MacOS, RStudio, and RTerm.exe under Windows.

-   `dlg_form()` for MacOS, Windows and RStudio, as well as, a better text version.

-   For MacOS one can also use:

    -   osascript -e 'tell application "Terminal" to choose color default color {12, 56, 78}' \# Warning: 0-65535!

-   rstudioapi provides `askForPassword()` =\> implement it, including other versions (use a yad form in Linux).

-   Translation into different languages.
