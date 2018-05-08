# On Ubuntu 16.04.4 (R 3.4.3 and RStudio 1.1.423), commands with rstudio = FALSE
# (except dlg_save) give a trivial warning:
# Gtk-Message: GtkDialog mapped without a transient parent. This is discouraged.

# dlg_dir -----------------------------------------------------------------

dlg_dir(title = "Sample Dir")$res
dlg_dir(title = "Sample Dir", rstudio = FALSE)$res
dlg_dir(title = "Sample Dir", rstudio = FALSE, zenity = TRUE)$res

# dlg_input ---------------------------------------------------------------

dlg_input("Sample Input", "Sample Default")$res
dlg_input("Sample Input", "Sample Default", rstudio = FALSE)$res
dlg_input("Sample Input", "Sample Default", rstudio = FALSE, zenity = TRUE)$res

# dlg_list ----------------------------------------------------------------

dlg_list(c(A = "Choice 1", B = "Choice 2"),
         title = "Sample List")$res
dlg_list(c(A = "Choice 1", B = "Choice 2"),
         title = "Sample List",
         rstudio = FALSE)$res
dlg_list(c(A = "Choice 1", B = "Choice 2"),
         title = "Sample List",
         rstudio = FALSE,
         zenity = TRUE)$res

# dlg_message -------------------------------------------------------------

dlg_message("Sample Message", "yesnocancel")$res
dlg_message("Sample Message", "yesnocancel", rstudio = FALSE)$res
dlg_message("Sample Message",
            "yesnocancel",
            rstudio = FALSE,
            zenity = TRUE)$res

# Two benefits here, if available in the system dialog:
# 1. cancel button comes back
# 2. ability to respond by pressing 'y' or 'n' without clicking also
#     comes back.

# dlg_open ----------------------------------------------------------------

dlg_open(title = "Sample Open")$res
dlg_open(title = "Sample Open", rstudio = FALSE)$res
dlg_open(title = "Sample Open", rstudio = FALSE, zenity = TRUE)$res

# dlg_save ----------------------------------------------------------------

dlg_save(title = "Sample Save")$res
dlg_save(title = "Sample Save", rstudio = FALSE)$res
dlg_save(title = "Sample Save", rstudio = FALSE, zenity = TRUE)$res

## MENU (No Changes)

