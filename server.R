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
    #output$data_exists <- reactive(!is.null(edf_data()))
    #outputOptions(output, "data_exists", suspendWhenHidden = FALSE)

    # Prep PSD data -------------
    psd_data <- reactive({
      prep_psd_data(edf_data())
    })


    # Make and export PSD plot ----------
    output$psd_plot <- renderPlot({
      make_psd_plot(psd_data(), input$psd_plot_channels, input$psd_frequency_range)
    })
    output$psd_plot_export <- downloadHandler(
      filename = function() {
        paste("psd_plot-", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        plot <- make_psd_plot(psd_data(),
                              input$psd_plot_channels,
                              input$psd_frequency_range)
        ggplot2::ggsave(file, plot, width = 6, height = 4)
      }
    )
    output$psd_plot_ui <- renderUI({
      tagList(
        plotOutput("psd_plot"),
        downloadButton("psd_plot_export")
      )
    })



    # Make and export topo plot ------------------------
    output$alpha_power_topoplot <- renderPlot({
      make_power_topoplot(psd_data(),
                          topo_bins = input$alpha_power_topoplot_bins,
                          freq_min = alpha_freq_min,
                          freq_max = alpha_freq_max)
    })
    output$alpha_power_topoplot_export <- downloadHandler(
      filename = function() {
        paste("alpha_power_topoplot-", Sys.Date(), ".pdf", sep="")
      },
      content = function(file) {
        plot <- make_power_topoplot(psd_data(),
                                    topo_bins = input$alpha_power_topoplot_bins,
                                    freq_min = alpha_freq_min,
                                    freq_max = alpha_freq_max)
        ggplot2::ggsave(file, plot, width = 6, height = 4)
      }
    )
    output$alpha_power_topoplot_ui <- renderUI({
      tagList(
        plotOutput("alpha_power_topoplot"),
        downloadButton("alpha_power_topoplot_export")
      )
    })


  }) # observeEvent
})