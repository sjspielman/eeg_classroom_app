#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(eegUtils)
source("utils.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("EEG in the classroom!"),

    # Sidebar layout ----------------
    sidebarLayout(
        # sidebar panel ----------------
        sidebarPanel(

          # fileInput edf_file -------------
          fileInput("edf_file",
                    "Upload EDF file:",
                    accept = ".edf"),

          # PSD plot selection --------------------
          h4("Power spectral density plot"),
          shinyWidgets::checkboxGroupButtons(
            "psd_plot_channels",
            "Which electrode(s) (i.e. channels) should be plotted? All are selected by default.",
            choices = expected_channels,
            selected = expected_channels,
            individual = TRUE
          ),
          sliderInput(
            "psd_frequency_range",
            "Select a range of frequencies to show on the x-axis.",
            min = 1, max = 60, step = 1,
            value = c(1, 40)
          ),

          br(),br(),
          h4("Alpha power topoplot"),
          sliderInput(
            "alpha_power_topoplot_bins",
            "How fine-grained should the contour be? Higher number for more contour lines.",
            min = 2, max = 30, step = 1,
            value = 6
          ),

          # go button -----------------
          actionButton("go", "Let's go!")

        ), # sidebarpanel

        mainPanel(
          # psd plot  ------------
          uiOutput("psd_plot_ui"),

          br(),br(),

          # alpha power topoplot -----------
          uiOutput("alpha_power_topoplot_ui")


        ) #mainpanel
    )# sidebar layout
))
