## ui.R ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  # library(listviewer)
  library(stringr)
  library(dplyr)
  library(haven)
  library(plotly)
  library(forcats)
  library(lassopmm)
  library(DT)
  library(purrr)
  library(tidyr)
  library(janitor)
  library(mice)
})

options(shiny.maxRequestSize = 100 * 1024^2)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Header ------------------------------------------------------------------
header <-
  dashboardHeader(title = "lassopmm web tool")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Sidebar -----------------------------------------------------------------

sidebar <-
  dashboardSidebar(disable = TRUE)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Body --------------------------------------------------------------------

# Data row
data_model_row <-
  fluidRow(
    column(
      width = 12,
      box(
        title = "Source data and model specification",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = "100%",
        fluidRow(
          ## Upload column ---------------------
          column(
            width = 3,
            box(
              # title = "Uploading source data",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE, # TRUE,
              # background = "light-blue",
              width = NULL,
              fileInput(
                "source_data",
                label = "Source data file (period 0)",
                multiple = FALSE,
                accept = NULL,
                width = "100%"
              ),
              fileInput(
                "target_data",
                label = "Target data file (period 1)",
                multiple = FALSE,
                accept = NULL,
                width = "100%"
              )
            ),
            textOutput("simulation_check"),
            textOutput("simulation_check_2")
          ),

          # Specification column
          column(
            width = 6,
            box(
              # title = "Model specification",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE, # TRUE,
              # background = "light-blue",
              width = NULL,

              fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectizeInput(
                    inputId = "dep_var",
                    label = "Dependent variable",
                    choices = NULL,
                    multiple = FALSE,
                    selected = NULL
                  )
                ),
                shiny::column(
                  width = 6,
                  shiny::selectizeInput(
                    inputId = "weight_var",
                    label = "Weighting variable (optional)",
                    choices = NULL,
                    multiple = FALSE,
                    selected = NULL
                  )
                )
              ),

              fluidRow(
                shiny::column(
                  width = 12,
                  shiny::selectizeInput(
                    inputId = "indep_var",
                    label = "Independent variables",
                    choices = NULL,
                    multiple = TRUE,
                    width = "100%"
                  )
                )
              ),

              fluidRow(
                shiny::column(
                  width = 6,
                  shiny::selectizeInput(
                    inputId = "strata_var",
                    label = "Strata variables (optional)",
                    choices = NULL,
                    multiple = TRUE,
                    selected = NULL
                  )
                ),
                shiny::column(
                  width = 6,
                  shiny::selectizeInput(
                    inputId = "cluster_var",
                    label = "Cluster variables (optional)",
                    choices = NULL,
                    multiple = TRUE,
                    selected = NULL
                  )
                )
              ),

              fluidRow(
                shiny::column(
                  width = 12,
                  shiny::selectizeInput(
                    inputId = "add_var",
                    label = "Additional variables",
                    choices = NULL,
                    multiple = TRUE,
                    width = "100%"
                  )
                )
              ),

              fluidRow(
                column(
                  width = 12,
                  actionButton(
                    inputId = "run_lassopmm",
                    label = "Run 'lassopmm' analysis",
                    icon = icon("bell"),
                    # class = "btn-success",
                    width = "100%"
                  )
                )
              )
            )
          ),

          # Additional parameters column
          column(
            width = 3,
            box(
              # title = "Additional parameters",
              status = "primary",
              solidHeader = TRUE,
              collapsible = FALSE, # TRUE,
              # background = "light-blue",
              width = NULL,

              shiny::numericInput(
                inputId = "n_boot",
                label = "Bootstrap iterations number",
                min = 0,
                value = 5
              ),

              shiny::numericInput(
                inputId = "n_near",
                label = "Nearest match number",
                value = 5
              ),

              shiny::numericInput(
                inputId = "n_folds",
                label = "Cross-validation folds number",
                value = 10
              ),

              shiny::numericInput(
                inputId = "force_lambda",
                label = "Fixed lambda",
                value = NULL
              )
            )
          )
        )
      )
    )
  )
# Analysis results row
analysis_results_row <-
  fluidRow(
    column(
      width = 12,
      box(
        status = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        width = "100%",

        fluidRow(
          column(
            width = 3,

            ## Poverty parameters
            box(
              title = "Poverty parameters",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = "100%",

              radioButtons(
                inputId = "transform_dep_var",
                label = "Transform dependent variable form source dataset with:",
                choices = c(
                  "Take an exponent" = "exp",
                  "No tranformation" = "no",
                  "Take a logarithm" = "log"
                ),
                selected = "exp"
              ),

              shiny::selectizeInput(
                inputId = "compare_var",
                label = "Variable from target data to calculate povery mobility",
                choices = NULL,
                multiple = FALSE,
                selected = NULL
              ),

              radioButtons(
                inputId = "transform_compare_var",
                label = "Transform variable form target dataset with:",
                choices = c(
                  "Take an exponent" = "exp",
                  "No tranformation" = "no",
                  "Take a logarithm" = "log"
                ),
                selected = "no"
              ),

              numericInput(
                inputId = "pov_line_1",
                label = "Extreme poverty line",
                min = 0,
                value = NULL,
                width = "100%"
              ),

              numericInput(
                inputId = "pov_line_2",
                label = "Moderate poverty line",
                min = 0,
                value = NULL,
                width = "100%"
              ),

              actionButton(
                inputId = "run_mobility_calc",
                label = "Run mobility calculations",
                width = "100%"
              )
            )
          ),

          column(
            width = 9,

            # Results: First diagnostic box ---------------------
            box(
              title = "Income variables comparison and ooverty summary",
              solidHeader = TRUE,
              status = "info",
              width = "100%",
              collapsible = TRUE,
              collapsed = FALSE,
              fluidRow(
                column(
                  width = 6,
                  h5("Povery statistics based on the 'source data' (period 0)"),
                  plotlyOutput(outputId = "source_income_plot"),
                  DT::DTOutput(outputId = "source_poverty_table")
                ),
                column(
                  width = 6,
                  h5("Povery statistics based on the 'target data' (period 1)"),
                  plotlyOutput(outputId = "target_income_plot"),
                  DT::DTOutput(outputId = "target_poverty_table")
                )
              )
            ),

            # Results: Mobility summary ------------------------------------
            box(
              title = "Mobility",
              solidHeader = TRUE,
              status = "success",
              width = "100%",
              collapsible = TRUE,
              collapsed = FALSE,
              fluidRow(
                column(
                  width = 12,
                  DT::DTOutput("mobility_table", height = "250px")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  DT::DTOutput("mobility_full_table")
                )
              )


            ),

            # Results: Data download ------------------------------------
            box(
              title = "Data explorer and download",
              solidHeader = TRUE,
              status = "success",
              width = "100%",
              collapsible = TRUE,
              collapsed = FALSE,

              fluidRow(
                column(
                  width = 6,
                  downloadButton(
                    outputId = "download_dta",
                    label = "Download all data in '.dta' (Stata) format")
                ),
                column(
                  width = 6,
                  downloadButton(
                    outputId = "download_csv",
                    label = "Download all data in '.csv' format")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  DT::DTOutput("full_data_table")
                )
              )
            )
          )
        )
      )
    )
  )


body <-
  dashboardBody(
    data_model_row,
    # fluidRow(
    #   box(
    #     width = 12, collapsible = TRUE, collapsed = TRUE,
    #     title = "Inputs",
    #     textOutput("inputs_data2"),
    #     jsoneditOutput("inputs_data")
    #   )
    # ),
    analysis_results_row
  )

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# UI ----------------------------------------------------------------------

dashboardPage(
  header,
  sidebar,
  body
)
