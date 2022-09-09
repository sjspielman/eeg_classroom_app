library(shiny)
library(shinyWidgets)
library(eegUtils)
source("utils.R")

shinyServer(function(input, output) {
  observeEvent(input$go, {

    # Read in and reference the EDF data from input$edf_file ----------
    #edf_data <- reactive({
    #  infile <- input$edf_file
    #  if (is.null(infile)) {
    #    # User has not uploaded a file yet
    #    return(NULL)
    #  }
    #  # Or, read in
    #  read_edf_file(infile$datapath)
    #})
    edf_data <- reactive({
      read_edf_file()
    })

    ### Reactive variables for conditionalPanel interaction in ui.R
    # output$data_exists <- reactive(!is.null(edf_data()))
    # outputOptions(output, "data_exists", suspendWhenHidden = FALSE)



    # Make a PSD plot ----------
    # Separate reactive for faster response time in app
    psd_data <- reactive({
      prep_psd_data(edf_data())
    })

   output$psd_plot <- renderPlot({
     make_psd_plot(psd_data(), input$psd_plot_channels, input$psd_frequency_range)
   })

  })
})