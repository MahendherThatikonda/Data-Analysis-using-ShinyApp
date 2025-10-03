shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  
  getTrControl_XGBTree <- reactive({
    y <- getTrainData()[,"Response"]
    n <- 25  # number of resamples
    tune_length <- 400  # enough seeds for xgbTree or large grids
    
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- sample.int(100000, tune_length)
    }
    seeds[[n + 1]] <- sample.int(100000, 1)
    
    trainControl(
      method = "boot",
      number = n,
      repeats = NA,
      allowParallel = TRUE,
      search = "grid",
      index = caret::createResample(y = y, times = n),
      savePredictions = "final",
      seeds = seeds,
      trim = TRUE
    )
  })
  
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "")  ## change the value parameter to your best method
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.null(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel(aes(label = label)) +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      method <- "null"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    method <- "null"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output null_Recipe (table) ----
  output$null_Recipe <- renderTable({
    method <- "null"
    mod <- models[[method]]
    req(mod)
    terms <- mod$recipe$term_info
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms
  })  


  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observe GO event ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),  
                        #      tuneGrid = expand.grid( ## CODE FOR RIDGE MODEL RMSE OF 277
                         #       alpha = 0,
                          #      lambda = 10^seq(-3, 1, length = 100)  # wide range of lambdas
                           #   ),
                              
    #                          tuneGrid = expand.grid( ## Code for Lasso Model
  #  alpha = 1,
  #    lambda = seq(0.0001, 0.1, length = 50)
  #     ),
#  tuneLength =5,
#  tuneGrid = expand.grid(
#    alpha = 1,  # LASSO
#    lambda = c(
#      seq(0.0001, 0.01, by = 0.001),     # finer control at low lambda
#      seq(0.015, 0.1, by = 0.005),       # mid-range
#      seq(0.15, 1, by = 0.05)            # higher range
#    )
#  ),
tuneGrid = expand.grid(
  alpha = seq(0, 1, by = 0.1),
  lambda = seq(0.001, 10, length = 50)  ),
    na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output method summary text ----
  output$glmnet_MethodSummary <- renderText({
    method <- "glmnet"
    description(method)
  })
  
  # output resampling metrics table ----
  output$glmnet_Metrics <- renderTable({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output hyperparameter tuning chart ----
  output$glmnet_ModelTune <- renderPlot({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })

  # output an html formatted recipe "print" ----
  output$glmnet_RecipePrint <- renderUI({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
    
  
  # output Recipe-output table ----
  output$glmnet_RecipeOutput <- renderTable({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  
  
  # output training summary print ----
  output$glmnet_TrainSummary <- renderPrint({
    method <- "glmnet"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })

  # output coefficient print ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observe GO event ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneGrid = expand.grid(ncomp = 8:12), na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      method <- "pls"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output method summary text ----
  output$pls_MethodSummary <- renderText({
    method <- "pls"
    description(method)
  })

  # output resampling metrics table ----
  output$pls_Metrics <- renderTable({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output hyperparameter tuning chart ----
  output$pls_ModelTune <- renderPlot({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })     
  
  # output an html formatted recipe "print" ----
  output$pls_RecipePrint <- renderUI({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })

  # output the recipe-output table ----
  output$pls_RecipeOutput <- renderTable({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  

  # output the training summary print ----
  output$pls_TrainSummary <- renderPrint({
    method <- "pls"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # output coefficients table ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observe the GO event -----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneGrid = expand.grid(cp = seq(0.000, 0.005, by = 0.0005))
, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      method <- "rpart"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output the method summary text ----
  output$rpart_MethodSummary <- renderText({
    method <- "rpart"
    description(method)
  })
  
  # output the resampling metrics table ----
  output$rpart_Metrics <- renderTable({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output recipe-outputs table ----
  output$rpart_RecipeOutput <- renderTable({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  

  # output hyperparameter tuning chart ----
  output$rpart_ModelTune <- renderPlot({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  # output a model tree-chart ----
  output$rpart_ModelTree <- renderPlot({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    rpart.plot::rpart.plot(mod$finalModel, roundint = FALSE)
  })     
  
  # output an html formatted recipe print ----
  output$rpart_RecipePrint <- renderUI({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
    
  })

  # output a training summary print ----
  output$rpart_TrainSummary <- renderPrint({
    method <- "rpart"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # Add further methods here.  You have the pls, glmnet and rpart templates to paste here - each has different layout 
  # and plotting characteristics so choose a good one and/or change the code more substantially
  
  
  # Now we added Ridge Regression
  
  # METHOD * Ridge ---------------------------------------------------------------------------------------------------------------------------
  library(elasticnet)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  #library(rpart.plot)

  # reactive getRidgeRecipe  
  # reactive getRpartRecipe ----
  getridgeRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$ridge_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observe the GO event -----
  observeEvent(
    input$ridge_Go,
    {
      method <- "ridge"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        
        # prep and bake recipe
        #ridge_recipe <- prep(getridgeRecipe(), verbose = FALSE)
        #train_data <- bake(ridge_recipe, new_data = getTrainData())  # apply to training data
        
        
        model <- caret::train(getridgeRecipe(), data = getTrainData(), method = "glmnet", metric = "RMSE", trControl = getTrControl(),
                              tuneGrid = expand.grid(
                                alpha = 0,  # <-- this forces glmnet to do Ridge
#                                lambda = seq(0.001, 0.1, length.out = 5) 
lambda = 10^seq(-4,1,length.out=20)
                              ),na.action=na.pass)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$ridge_Load,
    {
      method  <- "ridge"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$ridge_Delete,
    {
      method <- "ridge"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output the method summary text ----
  output$ridge_MethodSummary <- renderText({
    method <- "ridge"
    description(method)
    print(models[["ridge"]])
  })
  
  # output the resampling metrics table ----
  output$ridge_Metrics <- renderTable({
    method <- "ridge"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output recipe-outputs table ----
  output$ridge_RecipeOutput <- renderTable({
    method <- "ridge"
    mod <- models[[method]]
    req(mod)
    
    # Defensive check
    if (is.null(mod$recipe) || nrow(mod$recipe$term_info) == 0) {
      return(data.frame(Message = "No recipe output available."))
    }
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  
  
  # output hyperparameter tuning chart ----
  output$ridge_ModelTune <- renderPlot({
    method <- "ridge"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  # output a model tree-chart ----
#  output$ridge_ModelTree <- renderPlot({
#    method <- "ridge"
#    mod <- models[[method]]
#    req(mod)
#    rpart.plot::rpart.plot(mod$finalModel, roundint = FALSE)
#  })     
  
  # output an html formatted recipe print ----
  output$ridge_RecipePrint <- renderUI({
    method <- "ridge"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
    
  })
  
  # output a training summary print ----
  output$ridge_TrainSummary <- renderPrint({
    method <- "ridge"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
  ##Neural Network
  
  library(nnet)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  #library(rpart.plot)
  
  # reactive getRidgeRecipe  
  # reactive getRpartRecipe ----
  getNNRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$NN_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observe the GO event -----
  observeEvent(
    input$NN_Go,
    {
      method <- "nnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        
        # prep and bake recipe
        #ridge_recipe <- prep(getridgeRecipe(), verbose = FALSE)
        #train_data <- bake(ridge_recipe, new_data = getTrainData())  # apply to training data
        
        
        model <- caret::train(getNNRecipe(), 
                              data = getTrainData(), 
                              method = "nnet", 
                              metric = "RMSE",
                              trControl = getTrControl(),
                              tuneGrid = expand.grid(
                                size = seq(25, 35, by = 2), decay = c(1e-4, 5e-4, 1e-3, 5e-3, 1e-2, 5e-2) # regularization
                              ),linout=TRUE,trace=FALSE,na.action=na.pass)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$NN_Load,
    {
      method  <- "nnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$NN_Delete,
    {
      method <- "nnet"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output the method summary text ----
  output$NN_MethodSummary <- renderText({
    method <- "nnet"
    description(method)
    print(models[["nnet"]])
  })
  
  # output the resampling metrics table ----
  output$NN_Metrics <- renderTable({
    method <- "nnet"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output recipe-outputs table ----
  output$NN_RecipeOutput <- renderTable({
    method <- "nnet"
    mod <- models[[method]]
    req(mod)
    
    # Defensive check
    if (is.null(mod$recipe) || nrow(mod$recipe$term_info) == 0) {
      return(data.frame(Message = "No recipe output available."))
    }
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  
  
  # output hyperparameter tuning chart ----
  output$NN_ModelTune <- renderPlot({
    method <- "nnet"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  # output a model tree-chart ----
  #  output$ridge_ModelTree <- renderPlot({
  #    method <- "ridge"
  #    mod <- models[[method]]
  #    req(mod)
  #    rpart.plot::rpart.plot(mod$finalModel, roundint = FALSE)
  #  })     
  
  # output an html formatted recipe print ----
  output$NN_RecipePrint <- renderUI({
    method <- "nnet"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
    
  })
  
  # output a training summary print ----
  output$NN_TrainSummary <- renderPrint({
    method <- "nnet"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
  ## SVMKernelMethod

  install.packages("kernlab")
  library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  #library(rpart.plot)
  
  # reactive getRidgeRecipe  
  # reactive getRpartRecipe ----
  getsvmrRecipe <- reactive({
    req(input$svmRadial_Preprocess) # added by Mahi
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmRadial_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observe the GO event -----
  observeEvent(
    input$svmr_Go,
    {
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        
        # prep and bake recipe
        #ridge_recipe <- prep(getridgeRecipe(), verbose = FALSE)
        #train_data <- bake(ridge_recipe, new_data = getTrainData())  # apply to training data
        
        
        model <- caret::train(getsvmrRecipe(), 
                              data = getTrainData(), 
                              method = "svmRadial", 
                              metric = "RMSE",
                              trControl = getTrControl(),
                              tuneGrid = expand.grid(
                                C = c(2, 3, 4, 5, 6),                 # Focused around 4
                                sigma = c(0.007, 0.009, 0.01, 0.012, 0.015)
                              ),
#                              tuneGrid = expand.grid(
#                                C = 1, sigma = 0.01     # RBF kernel width (optional, auto-tuned by default)
        #                      ),
        na.action=na.pass)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(
    input$svmr_Load,
    {
      method  <- "svmRadial"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmr_Delete,
    {
      method <- "svmRadial"
      models[[method]] <- NULL
      gc()
    }
  )
  
  # output the method summary text ----
  output$svmr_MethodSummary <- renderText({
    method <- "svmRadial"
    description(method)
    print(models[["svmRadial"]])
  })
  
  # output the resampling metrics table ----
  output$svmr_Metrics <- renderTable({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    mod$results[ which.min(mod$results[, "RMSE"]), ]
  })
  
  # output recipe-outputs table ----
  output$svmr_RecipeOutput <- renderTable({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    
    # Defensive check
    if (is.null(mod$recipe) || nrow(mod$recipe$term_info) == 0) {
      return(data.frame(Message = "No recipe output available."))
    }
    terms <- as.data.frame(mod$recipe$term_info)
    n <- dim(terms)[1]
    types <- vector(mode="character", length=n)
    for (row in 1:n) {
      types[row] <- paste(collapse = " ", unlist(terms$type[row]))
    }
    terms$type <- types
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })  
  
  # output hyperparameter tuning chart ----
  output$svmr_ModelTune <- renderPlot({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  # output a model tree-chart ----
  #  output$ridge_ModelTree <- renderPlot({
  #    method <- "ridge"
  #    mod <- models[[method]]
  #    req(mod)
  #    rpart.plot::rpart.plot(mod$finalModel, roundint = FALSE)
  #  })     
  
  # output an html formatted recipe print ----
  output$svmr_RecipePrint <- renderUI({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE) %>%
      gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
    
  })
  
  # output a training summary print ----
  output$lsvmr_TrainSummary <- renderPrint({
    method <- "svmRadial"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
#---------  ## XGB Boost Method-----------------------------------------------------
#  install.packages("xgboost")
  library(xgboost)
  
  getXgbRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$xgb_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  observeEvent(
    input$xgb_Go,
    {
      method <- "xgbTree"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getXgbRecipe(),
                              data = getTrainData(),
                              method = method,
                              metric = "RMSE",
                              trControl = getTrControl_XGBTree(),
                              tuneLength = 400,
                              na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
  )
  
  observeEvent(input$xgb_Load, {
    method <- "xgbTree"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  })
  
  observeEvent(input$xgb_Delete, {
    method <- "xgbTree"
    models[[method]] <- NULL
    gc()
  })
  
  output$xgb_MethodSummary <- renderText({
    method <- "xgbTree"
    description(method)
  })
  
  output$xgb_Metrics <- renderTable({
    method <- "xgbTree"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$xgb_ModelTune <- renderPlot({
    method <- "xgbTree"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$xgb_RecipePrint <- renderUI({
    method <- "xgbTree"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse= "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$xgb_RecipeOutput <- renderTable({
    method <- "xgbTree"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- nrow(terms)
    types <- sapply(terms$type, function(x) paste(unlist(x), collapse = " "))
    terms$type <- types
    terms %>%
      dplyr::filter(role == "predictor") %>%
      dplyr::select(type, source) %>%
      dplyr::group_by(type, source) %>%
      dplyr::summarise(count = n())
  })
  
  output$xgb_TrainSummary <- renderPrint({
    method <- "xgbTree"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
  
#----------------------Random Forest------------------------------------
  
  # METHOD * Random Forest (ranger)
  #library(ranger)
  
  getRFRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rf_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  
  observeEvent(input$rf_Go, {
    method <- "rf"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    obj <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(
        getRFRecipe(),
        data = getTrainData(),
        method = "rf",   # <- plain Random Forest via caret
        metric = "RMSE",
        trControl = getTrControl(),
        tuneGrid = expand.grid(mtry = c(5, 7, 9, 11, 13, 15)),
#        tuneGrid = expand.grid(mtry = c(5, 7, 9, 10, 11, 13, 15, 17, 20)),
        importance = TRUE,
    #    ntree=500,
        ntree = 1000,
        na.action = na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(obj)
    })
  })
  
  
  observeEvent(input$rf_Load, {
    method <- "rf"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  })
  
  observeEvent(input$rf_Delete, {
    method <- "rf"
    models[[method]] <- NULL
    gc()
  })
  
  output$rf_MethodSummary <- renderText({
    method <- "rf"
    description(method)
  })
  
  output$rf_Metrics <- renderTable({
    method <- "rf"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$rf_ModelTune <- renderPlot({
    method <- "rf"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$rf_RecipePrint <- renderUI({
    method <- "rf"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  output$rf_RecipeOutput <- renderTable({
    method <- "rf"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- nrow(terms)
    types <- sapply(terms$type, function(x) paste(unlist(x), collapse = " "))
    terms$type <- types
    terms %>%
      dplyr::filter(role == "predictor") %>%
      dplyr::select(type, source) %>%
      dplyr::group_by(type, source) %>%
      dplyr::summarise(count = n())
  })
  
  output$rf_TrainSummary <- renderPrint({
    method <- "rf"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
#-------------------SVM LINEAR---------------------------------------------
  
  # Reactive Recipe
  getsvml2Recipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svml2_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  
  # Train model
  observeEvent(input$svml2_Go, {
    method <- "svmLinear2"
    models[[method]] <- NULL
    showNotification(id = method, paste("Training", method, "model..."), session = session, duration = NULL)
    obj <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(getsvml2Recipe(),
                            data = getTrainData(),
                            method = "svmLinear2",
                            metric = "RMSE",
                            trControl = getTrControl(),
                            # tuneLength = 10,
                            tuneGrid = expand.grid(
                              cost = seq(0.1, 0.4, by = 0.05)
                            ),
                            na.action = na.pass)
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    }, finally = {
      removeNotification(id = method)
      stopMode(obj)
    })
  })
  
  
  # Load model
  observeEvent(input$svml2_Load, {
    method <- "svmLinear2"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  })
  
  # Delete model
  observeEvent(input$svml2_Delete, {
    method <- "svmLinear2"
    models[[method]] <- NULL
    gc()
  })
  
  # Output text
  output$svml2_MethodSummary <- renderText({
    method <- "svmLinear2"
    description(method)
  })
  
  output$svml2_Metrics <- renderTable({
    method <- "svmLinear2"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$svml2_ModelTune <- renderPlot({
    method <- "svmLinear2"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$svml2_RecipePrint <- renderUI({
    method <- "svmLinear2"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(tags$head(tags$style(css)), tags$pre(HTML(html)))
  })
  
  output$svml2_RecipeOutput <- renderTable({
    method <- "svmLinear2"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    terms$type <- sapply(terms$type, function(x) paste(unlist(x), collapse = " "))
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })
  
  output$svml2_TrainSummary <- renderPrint({
    method <- "svmLinear2"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
  
  # METHOD * ctree2 ---------------------------------------------------------------------------------------------------------------------------
  library(partykit)  # Needed for ctree2
  
  # Recipe
  getctree2Recipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$ctree2_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # Train model
  observeEvent(input$ctree2_Go, {
    method <- "ctree2"
    models[[method]] <- NULL
    showNotification(id = method, paste("Training", method, "model..."), session = session, duration = NULL)
    obj <- startMode(input$Parallel)
    
    tryCatch({
      
      tuneGrid  <-  expand.grid(
        mincriterion = seq(0.90, 0.99, by = 0.01),
        maxdepth = c(3, 5, 7, 9)
      )
      
      model <- caret::train(
        getctree2Recipe(),
        data = getTrainData(),
        method = "ctree2",
        metric = "RMSE",
        tuneGrid = tuneGrid,
        trControl = getTrControl(),  # or a basic trainControl if you like
        na.action = na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    }, finally = {
      removeNotification(id = method)
      stopMode(obj)
    })
  })
  
  
  # Load/Delete/Output blocks
  observeEvent(input$ctree2_Load, {
    method <- "ctree2"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  })
  
  observeEvent(input$ctree2_Delete, {
    method <- "ctree2"
    models[[method]] <- NULL
    gc()
  })
  
  output$ctree2_MethodSummary <- renderText({
    method <- "ctree2"
    description(method)
  })
  
  output$ctree2_Metrics <- renderTable({
    method <- "ctree2"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$ctree2_ModelTune <- renderPlot({
    method <- "ctree2"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$ctree2_RecipePrint <- renderUI({
    method <- "ctree2"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(tags$head(tags$style(css)), tags$pre(HTML(html)))
  })
  
  output$ctree2_RecipeOutput <- renderTable({
    method <- "ctree2"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    terms$type <- sapply(terms$type, function(x) paste(unlist(x), collapse = " "))
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })
  
  output$ctree2_TrainSummary <- renderPrint({
    method <- "ctree2"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
  #-------------Gradient Boosting Method----------------------------------------------
  # Remove: library(bst)
  library(gbm)  # GBM support
  
  # Initial recipe setup
  getgbmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gbm_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # Train model
  observeEvent(input$gbm_Go, {
    method <- "gbm"
    models[[method]] <- NULL
    showNotification(id = method, paste("Training", method, "model..."), session = session, duration = NULL)
    obj <- startMode(input$Parallel)
    tuneGrid <- expand.grid(
      n.trees = seq(1000, 3000, by = 500),
      interaction.depth = c(4, 5, 6, 7),
      shrinkage = c(0.01, 0.005, 0.001),
      n.minobsinnode = c(1, 2, 3, 4)
    )
    try({
      model <- caret::train(getgbmRecipe(),
                            data = getTrainData(),
                            method = "gbm",
                            metric = "RMSE",
                            tuneGrid = tuneGrid,
                           trControl = getTrControl(),
                           verbose=FALSE)
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    })
    removeNotification(id = method)
    stopMode(obj)
  })
  
  # Load model
  observeEvent(input$gbm_Load, {
    method <- "gbm"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  })
  
  # Delete model
  observeEvent(input$gbm_Delete, {
    method <- "gbm"
    models[[method]] <- NULL
    gc()
  })
  
  # Outputs
  output$gbm_MethodSummary <- renderText({
    method <- "gbm"
    description(method)
  })
  
  output$gbm_Metrics <- renderTable({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$gbm_ModelTune <- renderPlot({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    plot(mod, digits = 3)
  })
  
  output$gbm_RecipePrint <- renderUI({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(tags$head(tags$style(css)), tags$pre(HTML(html)))
  })
  
  output$gbm_RecipeOutput <- renderTable({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    terms$type <- sapply(terms$type, function(x) paste(unlist(x), collapse = " "))
    terms |>
      dplyr::filter(role == "predictor") |>
      dplyr::select(type, source) |>
      dplyr::group_by(type, source) |>
      dplyr::summarise(count = n())
  })
  
  output$gbm_TrainSummary <- renderPrint({
    method <- "gbm"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
#--------------------------------glmStepAIC Mehtod----------------------------------------
  # Recipe Function for glmStepAIC
  getGLMStepAICRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmStepAIC_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # Train Model
  observeEvent(input$glmStepAIC_Go, {
    method <- "glmStepAIC"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    obj <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(
        getGLMStepAICRecipe(),
        data = getTrainData(),
        method = method,
        metric = "RMSE",
        trControl = getTrControl(),
        na.action = na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(obj)
    })
  })
  
  # Load Model
  observeEvent(input$glmStepAIC_Load, {
    method <- "glmStepAIC"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  })
  
  # Delete Model
  observeEvent(input$glmStepAIC_Delete, {
    method <- "glmStepAIC"
    models[[method]] <- NULL
    gc()
  })
  
  # Method Summary
  output$glmStepAIC_MethodSummary <- renderText({
    method <- "glmStepAIC"
    description(method)
  })
  
  # Metrics Output
  output$glmStepAIC_Metrics <- renderTable({
    method <- "glmStepAIC"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  # No tuning plot for glmStepAIC
  output$glmStepAIC_ModelTune <- renderPlot({
    plot.new()
    text(0.5, 0.5, "No hyperparameter tuning for glmStepAIC", cex = 1.5)
  })
  
  # Recipe HTML
  output$glmStepAIC_RecipePrint <- renderUI({
    method <- "glmStepAIC"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep="<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(
      tags$head(tags$style(css)),
      tags$pre(HTML(html))
    )
  })
  
  # Recipe Output
  output$glmStepAIC_RecipeOutput <- renderTable({
    method <- "glmStepAIC"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    n <- nrow(terms)
    types <- sapply(terms$type, function(x) paste(unlist(x), collapse = " "))
    terms$type <- types
    terms %>%
      dplyr::filter(role == "predictor") %>%
      dplyr::select(type, source) %>%
      dplyr::group_by(type, source) %>%
      dplyr::summarise(count = n())
  })
  
  # Train Summary
  output$glmStepAIC_TrainSummary <- renderPrint({
    method <- "glmStepAIC"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  # Coefficient Output
  output$glmStepAIC_Coef <- renderTable({
    method <- "glmStepAIC"
    mod <- models[[method]]
    req(mod)
    coef_df <- as.data.frame(summary(mod$finalModel)$coefficients)
    coef_df
  })
  
#--------------------------Earth (MARS) Model--------------------
  
  library(Cubist)
  
  # Define the recipe generator for Cubist
  getCubistRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$cubist_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # Train the Cubist model
  observeEvent(input$cubist_Go, {
    method <- "cubist"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    obj <- startMode(input$Parallel)
    tryCatch({
      
      # Show recipe steps
      recipe_obj <- getCubistRecipe()  #created by Mahi
      cat("??? Recipe created:\n")
      print(recipe_obj)
      
      # Show baked data preview
      baked <- prep(recipe_obj) %>% bake(new_data = getTrainData()) #created by Mahi
      cat("??? Baked data preview:\n")
      print(head(baked))
      print(paste("??? Baked data has", ncol(baked), "columns"))
      model <- caret::train(
        getCubistRecipe(),
        data = getTrainData(),
        method = "cubist",
        metric = "RMSE",
        trControl = getTrControl(),
        tuneGrid = expand.grid(
#          committees = c(15, 20, 25, 30), RMSE AT 340
#          committees = c(30, 35, 40, 45, 50),
#          neighbors = c(7, 8, 9)
          committees = seq(10, 100, by = 10),
          neighbors = c(3, 5, 7, 9)
        ),
        na.action = na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    }, finally = {
      removeNotification(id = method)
      stopMode(obj)
    })
  })
  
  # Load the saved Cubist model
  observeEvent(input$cubist_Load, {
    method <- "cubist"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  })
  
  # Delete the Cubist model from memory
  observeEvent(input$cubist_Delete, {
    method <- "cubist"
    models[[method]] <- NULL
    gc()
  })
  
  # Output: Method description
  output$cubist_MethodSummary <- renderText({
    method <- "cubist"
    description(method)
  })
  
  # Output: Metrics table
  output$cubist_Metrics <- renderTable({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  # Output: Hyperparameter tuning plot
  output$cubist_ModelTune <- renderPlot({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  # Output: Recipe steps (formatted)
  output$cubist_RecipePrint <- renderUI({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(tags$head(tags$style(css)), tags$pre(HTML(html)))
  })
  
  # Output: Recipe variable summary
  output$cubist_RecipeOutput <- renderTable({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    terms$type <- sapply(terms$type, function(x) paste(unlist(x), collapse = " "))
    terms %>%
      dplyr::filter(role == "predictor") %>%
      dplyr::select(type, source) %>%
      dplyr::group_by(type, source) %>%
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  # Output: Training summary
  output$cubist_TrainSummary <- renderPrint({
    method <- "cubist"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
#------------------------------------Bagged CART Method----------------------
  
  getTreebagRecipe <- reactive({
    recipes::recipe(Response ~ ., data = getTrainData()) %>%
      dynamicSteps(input$treebag_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  observeEvent(input$treebag_Go, {
    method <- "treebag"
    models[[method]] <- NULL
    showNotification(id = method, paste("Training", method, "model..."), session = session, duration = NULL)
    obj <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(
        getTreebagRecipe(),
        data = getTrainData(),
        method = "treebag",
        trControl = getTrControl(),
        na.action = na.pass
      )
      saveToRds(model, method)
      models[[method]] <- model
    }, finally = {
      removeNotification(id = method)
      stopMode(obj)
    })
  })
  
  observeEvent(input$treebag_Load, {
    model <- loadRds("treebag", session)
    if (!is.null(model)) models[["treebag"]] <- model
  })
  
  observeEvent(input$treebag_Delete, {
    models[["treebag"]] <- NULL
    gc()
  })
  
  output$treebag_MethodSummary <- renderText({ description("treebag") })
  
  output$treebag_Metrics <- renderTable({
    mod <- models[["treebag"]]
    req(mod)
    mod$results[1, ]
  })
  
  output$treebag_RecipePrint <- renderUI({
    mod <- models[["treebag"]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(tags$head(tags$style(css)), tags$pre(HTML(html)))
  })
  
  output$treebag_RecipeOutput <- renderTable({
    mod <- models[["treebag"]]
    req(mod)
    as.data.frame(mod$recipe$term_info) %>%
      dplyr::filter(role == "predictor") %>%
      dplyr::select(type, source) %>%
      dplyr::group_by(type, source) %>%
      dplyr::summarise(count = dplyr::n())
  })
  
  output$treebag_TrainSummary <- renderPrint({
    mod <- models[["treebag"]]
    req(mod)
    print(mod)
  })
  
  
#----------------------ExtraTress Model-----------------------------
  
  getExtraTreesRecipe <- reactive({
    recipes::recipe(Response ~ ., data = getTrainData()) %>%
      dynamicSteps(input$extratrees_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  observeEvent(input$extratrees_Go, {
    method <- "extraTrees"
    models[[method]] <- NULL
    showNotification(id = method, paste("Training", method, "model..."), session = session, duration = NULL)
    obj <- startMode(input$Parallel)
    
    tryCatch({
      model <- caret::train(
        getExtraTreesRecipe(),
        data = getTrainData(),
        method = "extraTrees",
        tuneGrid = expand.grid(
          mtry = c(3, 5, 7),
          numRandomCuts = c(1, 2)
        ),
        na.action = function(x) x
      )
      saveToRds(model, method)
      models[[method]] <- model
    },error = function(e) {
      print(e)
      showNotification(paste("Model training failed:", e$message), type = "error")
    },  finally = {
      removeNotification(id = method)
      stopMode(obj)
    })
  })
  
  observeEvent(input$extratrees_Load, {
    model <- loadRds("extraTrees", session)
    if (!is.null(model)) models[["extraTrees"]] <- model
  })
  
  observeEvent(input$extratrees_Delete, {
    models[["extraTrees"]] <- NULL
    gc()
  })
  
  output$extratrees_MethodSummary <- renderText({ description("extraTrees") })
  
  output$extratrees_Metrics <- renderTable({
    mod <- models[["extraTrees"]]
    req(mod)
    mod$results[1, ]
  })
  
  output$extratrees_RecipePrint <- renderUI({
    mod <- models[["extraTrees"]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(tags$head(tags$style(css)), tags$pre(HTML(html)))
  })
  
  output$extratrees_RecipeOutput <- renderTable({
    mod <- models[["extraTrees"]]
    req(mod)
    as.data.frame(mod$recipe$term_info) %>%
      dplyr::filter(role == "predictor") %>%
      dplyr::select(type, source) %>%
      dplyr::group_by(type, source) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
  })
  
  output$extratrees_TrainSummary <- renderPrint({
    mod <- models[["extraTrees"]]
    req(mod)
    print(mod)
  })
  
#-----------------------------RANGER METHOD--------------------
  
  library(ranger)
  
  getRangerRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$ranger_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  observeEvent(input$ranger_Go, {
    method <- "ranger"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    obj <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(
        getRangerRecipe(),
        data = getTrainData(),
        method = "ranger",
        metric = "RMSE",
        trControl = getTrControl(),
        tuneGrid = expand.grid(
          #mtry = c(2, 5, 10),
          mtry = c(15,20,25),
          splitrule = "variance",
          min.node.size=10
          #min.node.size = c(1, 5)
        )
      #  ,
     #   na.action = na.omit  # Important: use na.omit instead of na.pass
      )
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    }, finally = {
      removeNotification(id = method)
      stopMode(obj)
    })
  })
  
  observeEvent(input$ranger_Load, {
    method <- "ranger"
    model <- loadRds(method, session)
    if (!is.null(model)) models[[method]] <- model
  })
  
  observeEvent(input$ranger_Delete, {
    method <- "ranger"
    models[[method]] <- NULL
    gc()
  })
  
  output$ranger_MethodSummary <- renderText({
    method <- "ranger"
    description(method)
  })
  
  output$ranger_Metrics <- renderTable({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    mod$results[which.min(mod$results[, "RMSE"]), ]
  })
  
  output$ranger_ModelTune <- renderPlot({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    plot(mod)
  })
  
  output$ranger_RecipePrint <- renderUI({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    html <- mod$recipe %>%
      print() %>%
      cli::cli_fmt() %>%
      cli::ansi_collapse(sep = "<br>", last = "<br>") %>%
      cli::ansi_html(escape_reserved = FALSE)
    css <- paste(format(ansi_html_style()), collapse = "\n")
    tagList(tags$head(tags$style(css)), tags$pre(HTML(html)))
  })
  
  output$ranger_RecipeOutput <- renderTable({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    terms <- as.data.frame(mod$recipe$term_info)
    terms$type <- sapply(terms$type, function(x) paste(unlist(x), collapse = " "))
    terms %>%
      dplyr::filter(role == "predictor") %>%
      dplyr::select(type, source) %>%
      dplyr::group_by(type, source) %>%
      dplyr::summarise(count = n(), .groups = "drop")
  })
  
  output$ranger_TrainSummary <- renderPrint({
    method <- "ranger"
    mod <- models[[method]]
    req(mod)
    print(mod)
  })
  
  
  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  
})
