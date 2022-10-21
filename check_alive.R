# Small script to launch the app as a subprocess to check if the app launches
#  If the app is alive after 5 seconds, we're golden. Otherwise, error out.

shinyproc <- processx::process$new("Rscript", c("-e", "shiny::runApp()"))
Sys.sleep(5)
stopifnot(shinyproc$is_alive())
shinyproc$kill()
