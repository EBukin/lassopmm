shinyServer(function(input, output, session) {

  # Loading source data -------------------
  source_data <- reactive({
    source_supported <-
      stringr::str_to_lower(input$source_data$datapath) %>%
        stringr::str_detect("\\.dta$") ||
        stringr::str_to_lower(input$source_data$datapath) %>%
          stringr::str_detect("\\.csv$")

    validate(
      need(isTruthy(input$source_data), "- Source data is not specified."),
      need(
        if (isTruthy(input$source_data)) {
          source_supported
        } else {
          TRUE
        },
        "- Source data has unsupported format. Supported data formats are '.dta' and '.csv'."
      )
    )
    # req(input$source_data)
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
    target_supported <-
      stringr::str_to_lower(input$target_data$datapath) %>%
        stringr::str_detect("\\.dta$") ||
        stringr::str_to_lower(input$target_data$datapath) %>%
          stringr::str_detect("\\.csv$")

    validate(
      need(isTruthy(input$target_data), "- Target data is not specified."),
      need(
        if (isTruthy(input$target_data)) {
          target_supported
        } else {
          TRUE
        },
        "- Target data has unsupported format. Supported data formats are '.dta' and '.csv'."
      )
    )

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
      dep_var_guess <- ""
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
      weight_var_guess <- ""
    }

    if (length(weight_var_guess) > 1) {
      weight_var_guess <- weight_var_guess[[1]]
    }

    isolate(current_weight <- input$weight_var)

    if (is.null(current_weight) || current_weight == "") {
      updateSelectizeInput(
        session,
        inputId = "weight_var",
        choices = possible_choices,
        selected = weight_var_guess,
        server = FALSE
      )
    } else if (current_weight == current_dep_var) {
      updateSelectizeInput(
        session,
        inputId = "weight_var",
        choices = possible_choices,
        selected = "",
        server = FALSE
      )
    } else {
      updateSelectizeInput(
        session,
        inputId = "weight_var",
        choices = possible_choices,
        selected = current_weight,
        server = FALSE
      )
    }
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
      selected = "",
      server = FALSE
    )

    updateSelectizeInput(
      session,
      inputId = "cluster_var",
      choices = possible_choices,
      selected = "",
      server = FALSE
    )
  })

  # Independent variable ------------------
  observe({
    current_dep_var <- input$dep_var
    current_weight_var <- input$weight_var
    current_strata_var <- input$strata_var
    current_cluster_var <- input$cluster_var

    isolate(current_indep <- input$indep_var)

    possible_choices <-
      Filter(
        function(x) !x %in% c(
            current_dep_var, current_weight_var,
            current_strata_var, current_cluster_var
          ),
        variable_choices_source()
      )

    if (all(is.null(current_indep)) || current_indep[[1]] == "") {
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
    } else {
      suggested_choices <-
        Filter(
          function(x) !x %in% c("id", "ID", "ids", ".id", ".ids", ".ID"),
          current_indep
        )
      updateSelectizeInput(
        session,
        inputId = "indep_var",
        choices = possible_choices,
        selected = suggested_choices,
        server = FALSE
      )
    }
  })

  # Other variable
  observe({
    updateSelectizeInput(
      session,
      inputId = "add_var",
      choices = variable_choices_source(),
      selected = "",
      server = FALSE
    )
  })

  # Updating variables' list for calculating poverty
  observe({
    req(target_data())
    target_pove_var_guess <- variable_choices_target()[str_detect(variable_choices_target(), "ipcf")]

    if (length(target_pove_var_guess) == 0) {
      target_pove_var_guess <- ""
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
      req(source_data())
      validate(
        need({
          isTruthy(input$dep_var)
        }, " - Please select dependent variable."),
        need({
          isTruthy(input$weight_var)
        }, "- Don't forget to select weight variable if needed."),
        need({
          isTruthy(input$indep_var)
        }, "- Please select independent variables.")
      )
    })

  # Validate same variables
  output$simulation_check_2 <-
    renderText({
      req(target_data())
      req(source_data())

      vars_selected <- c(input$indep_var)
      vars_existing <- variable_choices_target()
      missing_vars <- vars_selected[!vars_selected %in% vars_existing]
      vars_message <-
        str_c(
          "- Variables '",
          str_c(missing_vars, collapse = "', '"),
          "' (from the list of independent variables) are missing in the 'target' data. \n",
          "- Revise the list of independent variables or upload correct 'target' data."
        )

      validate(need({
        all(vars_selected %in% vars_existing)
      }, vars_message))
    })

  # Checking if there are non-numeric variables --------------------------
  output$simulation_check_3 <-
    renderText({
      req(input$source_data)
      req(input$dep_var)
      req(input$indep_var)
      variable_types <-
        map(source_data(), ~ typeof(.x[1])) %>%
        unlist() %>%
        magrittr::extract(names(.) %in% c(input$dep_var, input$indep_var, input$weight_var)) %>%
        magrittr::extract(!(.) %in% c("integer", "double"))
      message <-
        str_c(
          "- Variable(s) '",
          str_c(names(variable_types), collapse = "', "),
          "' is (are) not integer or numeric. \n",
          "- Only numeric and integer variables are alowed for dependent, independent and weight variables. ",
          # "Analysis will result in error if unsupported variables' types are supplied. \n",
          "- Unsupported variable(s) are: ",
          str_c(str_c("[", names(variable_types), ": ", variable_types, "]"), collapse = ", ")
        )
      validate(
        need({
          length(variable_types) == 0
        }, message)
      )
    })

  # Missing data Validation -----------------------------------------
  output$simulation_check_4 <-
    renderText({
      req(input$source_data)
      req(input$dep_var)
      req(input$indep_var)
      n_na_rows <-
        source_data() %>%
        select(c(
          var_check(input$dep_var), var_check(input$indep_var),
          var_check(input$weight_var), var_check(input$strata_var),
          var_check(input$cluster_var)
        ))
      if (ncol(n_na_rows) > 0) {
        is_missing <- function(x) any(is.na(x))
        cols_with_na <-
          n_na_rows %>%
          select_if(is_missing) %>%
          names()
        n_na_rows <-
          n_na_rows %>%
          filter_all(any_vars(is.na(.))) %>%
          nrow()


        validate(
          need(
            !n_na_rows > 0,
            str_c(
              "- 'Source data' contains ", n_na_rows, " rows, where one of the ",
              "dependent, independent or weight variables contains empty value (NA). ",
              "As such 'empty' values are not allowed in lassopmm analysis, ",
              "they will be dropped. \n",
              "- Variables that contain missing data are: '",
              str_c(cols_with_na, collapse = "', '"),
              "'. \n- Consider revising 'Source data' or independent, dependent and weight variables"
            )
          )
        )
      }
    })

  # Valid Press Simulation -------------------------------------
  output$simulation_check_5 <-
    renderText({
      req(input$source_data)
      req(input$target_data)
      req(input$indep_var)
      req(input$dep_var)
      validate(need(
        {
          isTruthy(input$run_lassopmm)
        },
        "- Don't forget to press 'Run 'lassopmm' analysis'"
      ))
    })

  # Lassopmm reactive ---------------------------------
  observeEvent(input$run_lassopmm, {
    problem <- FALSE
    message <- ""
    if (!isTruthy(input$source_data)) {
      problem <- TRUE
      message <- "'Source data' is missing!\n"
    } else {
      source_supported <-
        stringr::str_to_lower(input$source_data$datapath) %>%
          stringr::str_detect("\\.dta$") ||
          stringr::str_to_lower(input$source_data$datapath) %>%
            stringr::str_detect("\\.csv$")

      if (!source_supported) {
        problem <- TRUE
        message <- str_c(message, "'Source data' has unsupported format. Supported data formats are '.dta' and '.csv'.\n")
      } else {
        if (!isTruthy(input$dep_var)) {
          problem <- TRUE
          message <- str_c(message, "Please select dependent variable.\n")
        }
        if (!isTruthy(input$indep_var)) {
          problem <- TRUE
          message <- str_c(message, "Please select independent variable.\n")
        }
      }
    }

    if (!isTruthy(input$target_data)) {
      problem <- TRUE
      message <- str_c(message, "'Target data' is missing!")
    } else {
      target_supported <-
        stringr::str_to_lower(input$target_data$datapath) %>%
          stringr::str_detect("\\.dta$") ||
          stringr::str_to_lower(input$target_data$datapath) %>%
            stringr::str_detect("\\.csv$")
      if (!target_supported) {
        problem <- TRUE
        message <- str_c(message, "'Target data' has unsupported format. Supported data formats are '.dta' and '.csv'.\n")
      }
    }

    if (isTruthy(input$source_data) &&
      isTruthy(input$target_data) &&
      target_supported &&
      source_supported) {
      vars_selected <- c(input$indep_var)
      vars_existing <- variable_choices_target()
      missing_vars <- vars_selected[!vars_selected %in% vars_existing]
      vars_message <-
        str_c(
          "Variables '",
          str_c(missing_vars, collapse = "', '"),
          "' (from the list of independent variables) are missing in the 'target' data. ",
          "Revise the list of independent variables or upload correct 'target' data."
        )

      if (!all(vars_selected %in% vars_existing)) {
        problem <- TRUE
        message <- str_c(message, vars_message)
      }
    }

    if (problem) {
      shinyalert(
        "Specification error!",
        message,
        type = "error",
        closeOnClickOutside = TRUE,
        timer = 5000
      )
    }
  })

  # Simulation ----------------------
  simulation_results <- reactiveVal()
  observeEvent({
    input$run_lassopmm
  }, {
    req(source_data())
    req(target_data())
    req(input$dep_var)
    req(input$indep_var)

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

    # Check if there are any NAs in data and filtering them out
    filtered_source <-
      source_data() %>%
      filter_at(
        vars(c(input$dep_var, var_check(input$indep_var), var_check(input$weight_var))),
        all_vars(!is.na(.))
      )

    set.seed(input$seed_n)

    lasso_result <- try(lassopmm::lassopmm(
      source = filtered_source,
      target = target_data(),
      dep_var = var_check(input$dep_var),
      indep_var = var_check(input$indep_var),
      weight_var = var_check(input$weight_var),
      extra_var = var_check(input$add_var),
      strata_vars = var_check(input$strata_var),
      cluster_vars = var_check(input$cluster_var),
      n_near = input$n_near,
      n_boot = input$n_boot,
      force_lambda = input$force_lambda,
      n_folds = input$n_folds,
      updateProgress = updateProgress
    ),
    silent = TRUE
    )

    got_error <-
      ifelse(class(lasso_result)[[1]][[1]] == "try-error", TRUE, FALSE)

    if (got_error) {
      error_message <-
        paste0(
          "Unfortunately, 'lassopmm' yielded with an error. ",
          "Try to modify list of independent variables. ",
          "The error message is: ",
          attr(lasso_result, "condition")[[1]]
        )
      error_message2 <-
        paste0(
          "Unfortunately, 'lassopmm' yielded with an error.\n ",
          "Try to modify list of independent variables. \n",
          "The error message is: \n",
          attr(lasso_result, "condition")[[1]]
        )
      shinyalert(
        "Lassopmm error!",
        error_message2,
        type = "error",
        closeOnClickOutside = TRUE
      )
    } else {
      error_message <- ""
    }

    validate(need(!got_error, error_message))

    simulation_results(lasso_result)
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
      req(source_data())
      req(input$dep_var)
      wt_var <- var_check(input$weight_var)
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
    req(source_pov_data())
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
      try(
        histogram %>%
          ggplotly()
      )

    got_error <- ifelse(class(histogram)[[1]][[1]] == "try-error", TRUE, FALSE)

    validate(
      need(
        !got_error,
        str_c(
          "Unfortunately, plot produced an error. \n",
          "Try to change transformation type used for the varialbes in the source data."
        )
      )
    )

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

    weight_var <- var_check(input$weight_var)
    if (is.null(weight_var)) weight_var <- "1"

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
        parse(text = var_check(weight_var))
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
      req(target_data())
      req(input$compare_var)
      wt_var <- var_check(input$weight_var)
      compare_var <- input$compare_var
      transf <- input$transform_compare_var
      target_data() %>%
        select(wt_var, compare_var) %>%
        {
          out <- (.)
          if (transf == "exp") {
            out <- mutate_at(out, vars(compare_var), list(~ exp(.)))
          }
          if (transf == "log") {
            out <- mutate_at(out, vars(compare_var), list(~ log(.)))
          }
          out
        }
    })

  output$target_income_plot <- renderPlotly({
    req(target_pov_data())

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
      try(
        histogram %>%
          ggplotly(),
        silent = TRUE
      )

    got_error <- ifelse(class(histogram)[[1]][[1]] == "try-error", TRUE, FALSE)

    validate(
      need(
        !got_error,
        str_c(
          "Unfortunately, plot produced an error. \n",
          "Try to change transformation type used for the varialbes in the traget data."
        )
      )
    )

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

    weight_var <- var_check(input$weight_var)
    if (is.null(weight_var)) weight_var <- "1"

    pov_1 <- input$pov_line_1
    if (!isTruthy(input$pov_line_2)) pov_2 <- NULL else pov_2 <- input$pov_line_2

    pov_stats <-
      target_pov_data() %>%
      detect_poverty(eval(parse(text = input$compare_var)), "actual_pov", pov_1, pov_2) %>%
      mutate_at(vars(contains("actual_pov")), list(~ ifelse(. == 1, eval(parse(text = weight_var)), 0))) %>%
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
    compare_var <- var_check(input$compare_var)
    weight_var <- var_check(input$weight_var)
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
    weight_var <- var_check(input$weight_var)

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
    validate(
      need({
        isTruthy(input$pov_line_1)
      }, "Please specify at least 'Extreme poverty line' to continue")
    )

    req(poverty_results())

    dependent_var <- input$dep_var
    imputed_var <- stringr::str_c(dependent_var, "_source")
    compare_var <- var_check(input$compare_var)
    weight_var <- var_check(input$weight_var)

    compare_vars <-
      poverty_results() %>%
      names(.) %>%
      magrittr::extract(stringr::str_detect(., "mob__")) %>%
      sort()

    poverty_results() %>%
      mutate_at(vars(compare_vars), list(~ . * 100)) %>%
      get_mi_means_table(compare_vars, weight_var) %>%
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
          "- Don't forget to press 'Run 'lassopmm' analysis'"
        ),
        need({
          isTruthy(input$pov_line_1)
        }, "- Specify at least 'Extreme poverty line' to continue"),
        need(
          if (isTruthy(simulation_results())) {
            isTruthy(input$run_mobility_calc)
          } else {
            FALSE
          },
          "- Don't forget to press 'Run mobility calculations'"
        )
      )

      DT::datatable(
        mobility_results_short(),
        caption = "Estimates of mobility from source to target",
        rownames = FALSE,
        colnames = mob_names(),
        extensions = c("Buttons"),
        options = list(
          dom = c("Bfrtip"),
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE,
          buttons = list(
            list(extend = "copy", text = "Copy table"),
            list(
              extend = "excel",
              text = "Download in an Excel file",
              title = "Mobility statistics"
            )
          )
        )
      )
    })

  output$growth_incidence <- renderPlotly({
    validate(need(
      {
        isTruthy(input$run_lassopmm)
      },
      "- Don't forget to press 'Run 'lassopmm' analysis'"
    ))
    req(simulation_results_clean())
    req(input$compare_var)
    n_tiles <- 5

    dependent_var <- input$dep_var
    imputed_var <- stringr::str_c(dependent_var, "_source")
    compare_var <- var_check(input$compare_var)
    weight_var <- var_check(input$weight_var)

    subs_data <-
      simulation_results_clean() %>%
      select(.id, .imp, compare_var, imputed_var, weight_var) %>%
      mutate(
        target_compare = eval(parse(text = compare_var)),
        source_imputed = eval(parse(text = imputed_var))
      )

    if (is.null(weight_var)) {
      subs_data <-
        subs_data %>%
        mutate(weights = 1)
    } else {
      subs_data <-
        subs_data %>%
        mutate(weights = eval(parse(text = weight_var)))
    }
    year_1 <- input$source_data_year
    year_2 <- input$target_data_year

    year_dif <- abs(year_1 - year_2)
    year_dif <- ifelse(year_dif == 0, 1, year_dif)

    subs_data <-
      subs_data %>%
      left_join(
        (.) %>%
          filter(.imp == 0) %>%
          mutate(
            quintiles =
              statar::xtile(target_compare, n_tiles, wt = weights)
          ) %>%
          select(.id, quintiles),
        ".id"
      ) %>%
      mutate(GIC = (source_imputed / target_compare)^(1/year_dif) - 1) %>%
      select(
        .id, .imp, target_compare, weights,
        source_imputed, quintiles, GIC
      )

    mi_stat <- try(suppressWarnings(get_mi_mean_by_group(subs_data, "GIC", "quintiles", "weights")))
    got_error <- ifelse(class(mi_stat)[[1]][[1]] == "try-error", TRUE, FALSE)

    validate(
      need(
        !got_error,
        str_c(
          "Unfortunately, this plot produced an error. \n",
          "Try to change transformation type used for the varialbes in the source or target data."
        )
      )
    )

    change_data <-
      mi_stat %>%
      select(variable, quintiles, estimate, `2.5 %`, `97.5 %`) %>%
      mutate(plus = `97.5 %` - estimate, minus = estimate - `2.5 %`,
             quintiles = as.integer(quintiles)) %>%
      mutate_at(vars(estimate, `2.5 %`, `97.5 %`, plus, minus), list(~round(., 3))) %>%
      mutate(text =  str_c(estimate, " [95% CI: ", `2.5 %`, "; ", `97.5 %`,"]"))

    change_data%>%
      plot_ly(
        x = ~quintiles,
        y = ~estimate,
        type = 'scatter',
        text = ~text,
        name = 'Growth incidence curve',
        mode = 'lines+markers',
        hoverinfo = 'text',
        error_y = ~list(
          array = plus,
          arrayminus = minus
        )) %>%
      layout(
        title = "Growth incidence curve",
        xaxis = list(
          title = "Income 'tiles'"
        ),
        yaxis = list(
          title = "Compund annual growth rate"
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
          searching = FALSE,
          dom = c("Bfrtip"),
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = FALSE,
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
      haven::write_dta(
        data = simulation_results_clean() %>%
          janitor::clean_names(),
        path = con
      )
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
