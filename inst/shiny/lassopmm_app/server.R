shinyServer(function(input, output, session) {

  # Loading source data -------------------
  source_data <- reactive({
    req(input$source_data)
    is_dta <-
      stringr::str_to_lower(input$source_data$datapath) %>%
      stringr::str_detect("\\.dta$")
    is_csv <-
      stringr::str_to_lower(input$source_data$datapath) %>%
      stringr::str_detect("\\.csv$")
    if (is_dta) {
      return_data <-
        haven::read_dta(input$source_data$datapath) %>%
        haven::zap_labels() %>%
        haven::zap_formats()
    } else if (is_csv) {
      return_data <- readr::read_csv(input$source_data$datapath)
    } else {
      return_data <- NULL
    }
    return_data
  })


  # Loading target data ---------------------
  target_data <- reactive({
    req(input$target_data)
    is_dta <-
      stringr::str_to_lower(input$target_data$datapath) %>%
      stringr::str_detect("\\.dta$")
    is_csv <-
      stringr::str_to_lower(input$target_data$datapath) %>%
      stringr::str_detect("\\.csv$")
    if (is_dta) {
      return_data <-
        haven::read_dta(input$target_data$datapath) %>%
        haven::zap_labels() %>%
        haven::zap_formats()
    } else if (is_csv) {
      return_data <- readr::read_csv(input$target_data$datapath)
    } else {
      return_data <- NULL
    }
    return_data
  })

  # Variable lists and dependent-independent variables ------------

  variable_choices_source <-
    reactive({
      req(source_data())
      names(source_data())
    })

  variable_choices_target <-
    reactive({
      req(target_data())
      names(target_data())
    })

  # dependent var
  observe({
    dep_var_guess <- variable_choices_source()[str_detect(variable_choices_source(), "lipcf")]

    if (length(dep_var_guess) == 0) {
      dep_var_guess <- NULL
    }

    if (length(dep_var_guess) > 1) {
      dep_var_guess <- dep_var_guess[[1]]
    }

    updateSelectizeInput(
      session,
      inputId = "dep_var",
      choices = variable_choices_source(),
      selected = dep_var_guess,
      server = FALSE
    )
  })

  # Weight var
  observe({
    current_dep_var <- input$dep_var
    possible_choices <-
      Filter(
        function(x) !x %in% c(current_dep_var),
        variable_choices_source()
      )
    weight_var_guess <- possible_choices[str_detect(possible_choices, "pondera")]

    if (length(weight_var_guess) == 0) {
      weight_var_guess <- NULL
    }

    if (length(weight_var_guess) > 1) {
      weight_var_guess <- weight_var_guess[[1]]
    }

    updateSelectizeInput(
      session,
      inputId = "weight_var",
      choices = possible_choices,
      selected = weight_var_guess,
      server = FALSE
    )
  })

  # independent var
  observe({
    current_dep_var <- input$dep_var
    current_weight_var <- input$weight_var
    possible_choices <-
      Filter(
        function(x) !x %in% c(current_dep_var, current_weight_var),
        variable_choices_source()
      )

    updateSelectizeInput(
      session,
      inputId = "strata_var",
      choices = possible_choices,
      selected = NULL,
      server = FALSE
    )

    updateSelectizeInput(
      session,
      inputId = "cluster_var",
      choices = possible_choices,
      selected = NULL,
      server = FALSE
    )
  })

  # Independent variable ------------------
  observe({
    current_dep_var <- input$dep_var
    current_weight_var <- input$weight_var
    current_strata_var <- input$strata_var
    current_cluster_var <- input$cluster_var

    possible_choices <-
      Filter(
        function(x) !x %in% c(
          current_dep_var, current_weight_var,
          current_strata_var, current_cluster_var
        ),
        variable_choices_source()
      )

    suggested_choices <-
      Filter(
        function(x) !x %in% c("id", "ID", "ids", ".id", ".ids", ".ID"),
        possible_choices
      )

    updateSelectizeInput(
      session,
      inputId = "indep_var",
      choices = possible_choices,
      selected = suggested_choices,
      server = FALSE
    )
  })

  # Other variable
  observe({
    updateSelectizeInput(
      session,
      inputId = "add_var",
      choices = variable_choices_source(),
      selected = NULL,
      server = FALSE
    )
  })

  # Updating variables' list for calculating poverty
  observe({
    req(target_data())
    target_pove_var_guess <- variable_choices_target()[str_detect(variable_choices_target(), "ipcf")]

    if (length(target_pove_var_guess) == 0) {
      target_pove_var_guess <- NULL
    }

    if (length(target_pove_var_guess) > 1) {
      target_pove_var_guess <- target_pove_var_guess[[1]]
    }

    updateSelectizeInput(
      session,
      inputId = "compare_var",
      choices = variable_choices_target(),
      selected = target_pove_var_guess,
      server = FALSE
    )
  })


  # Simulation validators --------------------------
  output$simulation_check <-
    renderText({
      target_supported <-
        stringr::str_to_lower(input$target_data$datapath) %>%
          stringr::str_detect("\\.dta$") ||
          stringr::str_to_lower(input$target_data$datapath) %>%
            stringr::str_detect("\\.csv$")
      source_supported <-
        stringr::str_to_lower(input$source_data$datapath) %>%
          stringr::str_detect("\\.dta$") ||
          stringr::str_to_lower(input$source_data$datapath) %>%
            stringr::str_detect("\\.csv$")

      validate(
        need(isTruthy(input$source_data), "Source data is not specified."),

        need(
          if (isTruthy(input$source_data)) {
            source_supported
          } else {
            TRUE
          },
          "Source data has unsupported format."
        ),
        need(isTruthy(input$target_data), "Target data is not specified."),

        need(
          if (isTruthy(input$target_data)) {
            target_supported
          } else {
            TRUE
          },
          "Target data has unsupported format."
        ),
        need(
          {
            if (!isTruthy(input$source_data)) {
              TRUE
            } else if (source_supported) {
              isTruthy(input$dep_var)
            } else {
              TRUE
            }
          },
          "Please select dependent variable."
        ),
        need(
          {
            if (!isTruthy(input$source_data)) {
              TRUE
            } else if (source_supported) {
              isTruthy(input$weight_var)
            } else {
              TRUE
            }
          },
          "Please select weight variable."
        ),
        need(
          {
            if (!isTruthy(input$source_data)) {
              TRUE
            } else if (source_supported) {
              isTruthy(input$indep_var)
            } else {
              TRUE
            }
          },
          "Please select independent variables."
        )
      )
    })

  output$simulation_check_2 <-
    renderText({
      req(target_data())
      validate(
        need(
          {
            all(c(input$weight_var, input$indep_var) %in% variable_choices_target())
          },
          "Not all independent and weight variables from the source data are
          present in the target data. Please provide a valid target data file."
        ),
        need(
          {
            isTruthy(simulation_results())
          },
          "Dont forget to press 'Run 'lassopmm' analysis'"
        )
      )
    })

  # Lassopmm reactive ---------------------------------
  simulation_results <- reactiveVal()

  observeEvent({
    input$run_lassopmm
  }, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Running 'lassopmm'", value = 0)

    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / input$n_folds
      }
      progress$set(value = value, detail = detail)
    }

    lassopmm::lassopmm(
      source = source_data(),
      target = target_data(),
      dep_var = input$dep_var,
      indep_var = input$indep_var,
      weight_var = input$weight_var,
      extra_var = input$add_var,
      strata_vars = input$strata_var,
      cluster_vars = input$cluster_var,
      n_near = input$n_near,
      n_boot = input$n_boot,
      force_lambda = input$force_lambda,
      n_folds = input$n_folds,
      updateProgress = updateProgress
    ) %>%
      simulation_results()
  })
#
#
#   # Technical input analysis ----------------
#
#   output$inputs_data <-
#     renderJsonedit({
#       jsonedit(
#         list(
#           `input$source_data` = input$source_data,
#           source_data = source_data(),
#           `input$target_data` = input$target_data,
#           target_data = target_data()
#           # dep_var = input$dep_var,
#           # weight_var = input$weight_var,
#           # indep_var = input$indep_var,
#           # strata_var = input$strata_var,
#           # cluster_var = input$cluster_var,
#           # run_lassopmm = input$run_lassopmm
#         ),
#         mode =
#           "tree", "change" = htmlwidgets::JS("function(){\n                                    console.log( event.currentTarget.parentNode.editor.get() )\n    }")
#       )
#     })
#
#   output$inputs_data2 <-
#     renderPrint({
#       print(
#         list(
#           # `input$source_data` = input$source_data,
#           # source_data = source_data(),
#           # `input$target_data` = input$target_data,
#           # target_data = target_data(),
#           dep_var = input$dep_var,
#           weight_var = input$weight_var,
#           indep_var = input$indep_var,
#           strata_var = input$strata_var,
#           cluster_var = input$cluster_var,
#           run_lassopmm = input$run_lassopmm,
#           n_boot = input$n_boot,
#           n_near = input$n_near,
#           n_folds = input$n_folds,
#           force_lambda = input$force_lambda,
#           pov_line_1 = input$pov_line_1,
#           pov_line_2 = input$pov_line_2,
#           exp_trans = input$exp_trans,
#           run_poverty_calc = input$run_poverty_calc
#         )
#       )
#     })

  # Source poverty data diagnostics -----------------------
  source_pov_data <-
    reactive({
      source_supported <-
        stringr::str_to_lower(input$source_data$datapath) %>%
        stringr::str_detect("\\.dta$") ||
        stringr::str_to_lower(input$source_data$datapath) %>%
        stringr::str_detect("\\.csv$")

      validate(
        need(isTruthy(input$source_data), "Source data is not specified."),
        need(
          if (isTruthy(input$source_data)) {
            source_supported
          } else {
            TRUE
          },
          "Source data has unsupported format."
        )
        )
      req(input$weight_var)
      req(input$dep_var)
      wt_var <- input$weight_var
      dp_var <- input$dep_var
      transf <- input$transform_dep_var
      source_data() %>%
        select(wt_var, dp_var) %>%
        {
          out <- (.)
          if (transf == "exp") {
            out <- mutate_at(out, vars(dp_var), list(~ exp(.)))
          }
          if (transf == "log") {
            out <- mutate_at(out, vars(dp_var), list(~ log(.)))
          }
          out
        }
    })

  output$source_income_plot <- renderPlotly({
    histogram <-
      source_pov_data() %>%
      ggplot() +
      aes_string(x = input$dep_var) +
      geom_histogram(bins = 75, alpha = 0.6) +
      xlab(input$dep_var) +
      ylab("") +
      theme_bw()

    if (isTruthy(input$pov_line_1)) {
      histogram <-
        histogram +
        geom_vline(xintercept = input$pov_line_1, colour = "red")
    }

    if (isTruthy(input$pov_line_2)) {
      histogram <-
        histogram +
        geom_vline(xintercept = input$pov_line_2, colour = "black")
    }
    histogram <-
      histogram %>%
      ggplotly()

    boxplot <-
      source_pov_data() %>%
      plot_ly(
        x = ~ get(input$dep_var), type = "box",
        boxpoints = "outliers", jitter = 0.3
      ) %>%
      layout(
        yaxis = list(title = ""),
        xaxis = list(title = input$dep_var)
      )
    suppressWarnings(
      subplot(boxplot, histogram, nrows = 2, shareX = TRUE) %>%
        layout(showlegend = FALSE)
    )
  })


  output$source_poverty_table <- DT::renderDT({
    req(input$pov_line_1)

    pov_1 <- input$pov_line_1
    if (!isTruthy(input$pov_line_2)) {
      pov_2 <- NULL
    } else {
      pov_2 <- input$pov_line_2
    }

    pov_stats <-
      source_pov_data() %>%
      detect_poverty(eval(parse(text = input$dep_var)), "actual_pov", pov_1, pov_2) %>%
      mutate_at(vars(contains("actual_pov")), list(~ ifelse(. == 1, eval(
        parse(text = input$weight_var)
      ), 0))) %>%
      summarise_at(vars(contains("actual_pov")), list(~ sum(.))) %>%
      gather(var, val, 1:length(.)) %>%
      mutate(
        val = val / sum(val, na.rm = T) * 100,
        three_pov_lines = any(str_detect(var, "^2")),
        var =
          case_when(
            str_detect(var, "^1") ~ "1. % below extreme poverty line",
            str_detect(var, "^2") &
              three_pov_lines ~ "2. % between extreme moderate poverty lines",
            str_detect(var, "^3") &
              three_pov_lines ~ "3. % above moderate poverty line",
            str_detect(var, "^3") &
              !three_pov_lines ~ "3. % above extreme poverty line"
          )
      ) %>%
      select(-three_pov_lines)
    names(pov_stats) <- c(" ", "% of population")
    pov_stats
  },
  rownames = F,
  options =
    list(
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE
    )
  )


  # Target poverty data diagnostics -----------------------
  target_pov_data <-
    reactive({

      target_supported <-
        stringr::str_to_lower(input$target_data$datapath) %>%
        stringr::str_detect("\\.dta$") ||
        stringr::str_to_lower(input$target_data$datapath) %>%
        stringr::str_detect("\\.csv$")

      validate(
        need(isTruthy(input$target_data), "Target data is not specified."),
        need(
          if (isTruthy(input$target_data)) {
            target_supported
          } else {
            TRUE
          },
          "Target data has unsupported format."
        ))

      req(input$weight_var)
      req(input$compare_var)
      wt_var <- input$weight_var
      dp_var <- input$compare_var
      transf <- input$transform_compare_var
      target_data() %>%
        select(wt_var, dp_var) %>%
        {
          out <- (.)
          if (transf == "exp") {
            out <- mutate_at(out, vars(dp_var), list(~ exp(.)))
          }
          if (transf == "log") {
            out <- mutate_at(out, vars(dp_var), list(~ log(.)))
          }
          out
        }
    })

  output$target_income_plot <- renderPlotly({
    histogram <-
      target_pov_data() %>%
      ggplot() +
      aes_string(x = input$compare_var) +
      geom_histogram(bins = 75, alpha = 0.6) +
      xlab(input$compare_var) +
      ylab("") +
      theme_bw()
    if (isTruthy(input$pov_line_1)) {
      histogram <-
        histogram +
        geom_vline(xintercept = input$pov_line_1, colour = "red")
    }

    if (isTruthy(input$pov_line_2)) {
      histogram <-
        histogram +
        geom_vline(xintercept = input$pov_line_2, colour = "black")
    }
    histogram <-
      histogram %>%
      ggplotly()
    boxplot <-
      target_pov_data() %>%
      plot_ly(x = ~ get(input$compare_var), type = "box", boxpoints = "outliers", jitter = 0.3) %>%
      layout(
        yaxis = list(title = ""),
        xaxis = list(title = input$compare_var)
      )
    suppressWarnings(
      subplot(boxplot, histogram, nrows = 2, shareX = TRUE) %>%
        layout(showlegend = FALSE)
    )
  })



  output$target_poverty_table <- DT::renderDT({
    req(input$pov_line_1)

    pov_1 <- input$pov_line_1
    if (!isTruthy(input$pov_line_2)) pov_2 <- NULL else pov_2 <- input$pov_line_2

    pov_stats <-
      target_pov_data() %>%
      detect_poverty(eval(parse(text = input$compare_var)), "actual_pov", pov_1, pov_2) %>%
      mutate_at(vars(contains("actual_pov")), list(~ ifelse(. == 1, eval(parse(text = input$weight_var)), 0))) %>%
      summarise_at(vars(contains("actual_pov")), list(~ sum(.))) %>%
      gather(var, val, 1:length(.)) %>%
      mutate(
        val = val / sum(val, na.rm = T) * 100,
        three_pov_lines = any(str_detect(var, "^2")),
        var =
          case_when(
            str_detect(var, "^1") ~ "1. % below extreme poverty line",
            str_detect(var, "^2") & three_pov_lines ~ "2. % between extreme moderate poverty lines",
            str_detect(var, "^3") & three_pov_lines ~ "3. % above moderate poverty line",
            str_detect(var, "^3") & !three_pov_lines ~ "3. % above extreme poverty line"
          )
      ) %>%
      select(-three_pov_lines)
    names(pov_stats) <- c(" ", "% of population")
    pov_stats
  },
  rownames = F,
  options =
    list(
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE
    )
  )

  # MI data for export and analysis ------------------------------
  simulation_results_clean <- reactive({
    req(simulation_results())
    dependent_var <- input$dep_var
    imputed_var <- stringr::str_c(dependent_var, "_source")
    compare_var <- input$compare_var
    weight_var <- input$weight_var
    transf_dep <- input$transform_dep_var
    transf_comp <- input$transform_compare_var

    simulation_results() %>%
      select(.id, .imp, compare_var, imputed_var, weight_var, everything()) %>%
      {
        out <- (.)
        if (transf_dep == "exp") {
          out <- mutate_at(out, vars(imputed_var), list(~ exp(.)))
        }
        if (transf_dep == "log") {
          out <- mutate_at(out, vars(imputed_var), list(~ log(.)))
        }
        out
      } %>%
      {
        out <- (.)
        if (transf_comp == "exp") {
          out <- mutate_at(out, vars(compare_var), list(~ exp(.)))
        }
        if (transf_comp == "log") {
          out <- mutate_at(out, vars(compare_var), list(~ log(.)))
        }
        out
      }
  })

  # Poverty calculations ------------------------------------------
  poverty_results <- reactive({
    req(simulation_results_clean())
    req(input$pov_line_1)

    pl_1 <- input$pov_line_1
    if (!isTruthy(input$pov_line_2)) {
      pl_2 <- NULL
    } else {
      pl_2 <- input$pov_line_2
    }

    dependent_var <- input$dep_var
    imputed_var <- stringr::str_c(dependent_var, "_source")
    compare_var <- input$compare_var
    weight_var <- input$weight_var

    simulation_results_clean() %>%
      select(.id, .imp, compare_var, imputed_var, weight_var) %>%
      detect_poverty(eval(parse(text = imputed_var)), "source", pl_1, pl_2) %>%
      detect_poverty(eval(parse(text = compare_var)), "target", pl_1, pl_2) %>%
      left_join(
        (.) %>%
          select(matches("\\d.{1,}_source$"), matches("\\d.{1,}_target$")) %>%
          get_all_combinations(mob_, clean_name = F, connector = "__")
      )
  })

  # Combility multiple imputations statistics -------------------

  mobility_results <- reactiveVal()
  observeEvent({
    input$run_mobility_calc
  }, {
    validate(need({
      isTruthy(input$pov_line_1)
    }, "Please specify at least 'Extreme poverty line' to continue"))

    req(poverty_results())

    dependent_var <- input$dep_var
    imputed_var <- stringr::str_c(dependent_var, "_source")
    compare_var <- input$compare_var
    weight_var <- input$weight_var

    compare_vars <-
      poverty_results() %>%
      names(.) %>%
      magrittr::extract(stringr::str_detect(., "mob__")) %>%
      sort()

    poverty_results() %>%
      mutate_at(vars(compare_vars), list(~ . * 100)) %>%
      get_mi_means_table(compare_vars, "pondera") %>%
      select(-ubar, -t) %>%
      arrange(variable) %>%
      mobility_results()
  })

  # Mobility results ----------------------
  mobility_results_short <- reactive({
    req(mobility_results())

    mobility_results() %>%
      select(variable, estimate, `2.5 %`, `97.5 %`, std.error) %>%
      mutate_if(any_vars(is.numeric(.)), list(~ round(., 2))) %>%
      tidyr::separate(variable, c("type", "from_var", "to_var"), sep = "__") %>%
      mutate_at(vars("from_var", "to_var"), list(~ str_remove_all(., "_.{1,}$"))) %>%
      mutate(value = glue::glue("{estimate} % [SE {std.error}; CI ({`2.5 %`};{`97.5 %`})]")) %>%
      mutate(three_pov_lines = any(str_detect(from_var, "^2"))) %>%
      mutate_at(
        vars("from_var", "to_var"),
        list(~
        case_when(
          str_detect(., "^1") ~ "1. Below extreme poverty line ",
          str_detect(., "^2") & three_pov_lines ~ "2. Between extreme and moderate poverty line",
          str_detect(., "^3") & three_pov_lines ~ "3. Above moderate poverty line",
          str_detect(., "^3") & !three_pov_lines ~ "3. Above extreme poverty line"
        ))
      ) %>%
      mutate_at(vars("from_var"), list(~ str_c(., " (source)"))) %>%
      mutate_at(vars("to_var"), list(~ str_c(., " (target)"))) %>%
      select(value, from_var, to_var) %>%
      spread(to_var, value)
  })

  mob_names <- reactive({
    req(mobility_results_short())
    c("", names(mobility_results_short())[-1])
  })

  output$mobility_table <-
    DT::renderDataTable({
      validate(
        need(
          isTruthy(simulation_results()),
          "Don't forget to press 'Run 'lassopmm' analysis'"
        ),
        need(
          if (isTruthy(simulation_results())) {
            isTruthy(input$run_mobility_calc)
          } else {
            FALSE
          },
          "Don't forget to press 'Run mobility calculations'"
        )
      )

      DT::datatable(
        mobility_results_short(),
        caption = "Estimates of mobility from source to target",
        rownames = FALSE,
        colnames = mob_names(),
        options =
          list(
            paging = FALSE,
            ordering = FALSE,
            searching = FALSE
          )
      )
    })

  output$mobility_full_table <-
    DT::renderDataTable({
      DT::datatable(
        data = {
          req(mobility_results())
          mobility_results()
        },
        rownames = FALSE,
        extensions = c("Buttons", "Scroller"),
        options = list(
          pageLength = 25,
          dom = c("Bfrtip"),
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE,
          buttons = list(
            list(extend = "copy", text = "Copy table"),
            list(
              extend = "excel",
              text = "Download in an Excel file",
              title = "Detailed mobility statistics"
            )
          )
        )
      )
    })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      readr::write_csv(simulation_results_clean(), con)
    }
  )

  output$download_dta <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".dta", sep = "")
    },
    content = function(con) {
      haven::write_dta(data = simulation_results_clean() %>%
                         janitor::clean_names(),
                       path = con)
    }
  )

  output$full_data_table <-
    DT::renderDataTable( #
      DT::datatable( #
        data = {
          req(simulation_results_clean())
          simulation_results_clean()
        }, #
        rownames = FALSE,
        extensions = c("Scroller"),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 600,
          scroller = TRUE
        )
      )
    )
})
