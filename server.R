#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(naniar)
library(visdat)
library(dplyr)
library(ggplot2)
library(plotly)
library(alluvial)
library(ggalluvial)
library(caret)
library(corrgram)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(recipes)
library(aplpack)
#library(caret)
library(recipes)
library(randomForest)
# Define server logic required to draw a histogram
function(input, output, session) {
    
#  dat <- dat %>%
 #   mutate(ID = row_number())
 # dat$ID <- seq_len(nrow(dat)) 
  
  
    output$SummaryA1 <- renderPrint({
      str(dat)
    })
    
    output$histogramA1 <- renderPlot({
      selected_var <- input$var
      selected_bins <- as.numeric(input$bins) 
      hist(dat[[selected_var]],main = paste("Histogram of", selected_var), xlab = selected_var, col = "lightblue",breaks=selected_bins)
    })

    output$BoxplotA1 <- renderPlot({
      selected_var <- input$var
      selected_range <- input$range
      boxplot(dat[[selected_var]],main = paste("Boxplot of", selected_var), xlab = selected_var, col = "lightblue", range = selected_range)
    })
    
    output$BarplotA1 <- renderPlot({
      selected_varbar <- input$varbar
      barplot(table(dat[[selected_varbar]]),main = paste("BarPlot of", selected_varbar), xlab = selected_varbar, col = "lightblue")
    })
    
    output$corPlot <- renderPlot({
      
      selected_method <- input$cor_method
      # Select only numeric columns from the dataset
      numeric_vars <- dat[, sapply(dat, is.numeric)]
      
      # Calculate correlation matrix with pairwise handling of NAs
      cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs", method = selected_method)
      
      # Draw the correlation matrix using corrplot
      library(corrplot)
      corrplot(cor_matrix,
               method = "color",        # use colored squares
               type = "lower",          # show only lower triangle
               tl.col = "black",        # label color
               tl.cex = 0.8,            # label text size
               diag = FALSE)            # hide the diagonal
    })
    
    
    output$missingdataA1 <- renderPlot({
      gg_miss_upset(dat)
    })
    output$missingdataA2 <- renderPlot({
      vis_dat(dat)
    })
    output$missingdataA3 <- renderPlot({
      vis_miss(dat)
    })
    output$datacleaningA1 <- renderPlot({
      # This calculates the ratio of missingness of a vector.
      pMiss <- function(x){ sum(is.na(x))/length(x)*100 }
      threshold <- 50
      cRatio <- apply(X = dat, MARGIN = 2, FUN = pMiss) # run pMiss for each column of the data frame
      barplot(sort(cRatio, decreasing = TRUE), 
              las = 2, 
              col = "skyblue", 
              main = "Missing Data Percentage by Column", 
              ylab = "% Missing")
    })
    
    output$datacleaningA2 <- renderText({
      pMiss <- function(x) { sum(is.na(x)) / length(x) * 100 }
      threshold <- input$threshold
      cRatioa <- apply(X = dat, MARGIN = 1, FUN = pMiss)
      
      to_remove <- which(cRatioa > threshold)
      total_to_remove <- length(to_remove)
      preview_ids <- head(rownames(dat)[to_remove], 50)
      
      paste("Observations to remove: ", total_to_remove, 
             "\nFirst 50 row names:\n", paste(preview_ids, collapse = ", "))
    })
    
    getCleanData <- reactive({
      threshold <- input$threshold
      
      pMiss <- function(x) sum(is.na(x)) / length(x) * 100
      col_missing_pct <- sapply(dat, pMiss)
      
      dat_cleaned <- dat[, col_missing_pct <= threshold]
      return(dat_cleaned)
    })
    
    output$CleanData <- renderPlot({
      dat_cleaned <- getCleanData()
      
      pMiss <- function(x) sum(is.na(x)) / length(x) * 100
      cRatio <- apply(dat_cleaned, 2, pMiss)
      
      barplot(sort(cRatio, decreasing = TRUE),
              las = 2,
              col = "lightgreen",
              main = "Missing % After Removing High-Missing Columns",
              ylab = "% Missing")
    })
    output$investigationsA1 <- renderPlot({
      m <- is.na(dat) + 0
      View(m)
      cm <- colMeans(m)
      m <- m[, cm > 0 & cm < 1, drop = FALSE]
      print(cm)#remove none-missing or all-missing variables
      selected_method <- input$invest_corr_method
      corrgram::corrgram(cor(m, method = selected_method), order = "OLO", abs = TRUE)
      #title(main = "Variable missing value correlation",
      #      sub = "Notice whether variables are missing in sets")
    })
    
    output$predictionA1 <- renderPlot({
      
      # Ensure dat is available in the environment
  #    req(dat)
   #   dat <- dat %>%
    #    mutate(id = row_number())
      
      dat$missingness <- apply(X = is.na(dat), MARGIN = 1, FUN = sum)
      
      # step 3
      tree <- caret::train(missingness ~ ., 
                           data = dat, 
                           method = "rpart", 
                           na.action = na.rpart) # na.rpart means "rpart will deal with missing predictors intrinsically"
      # step 4
      rpart.plot(tree$finalModel, 
                 main = "Predicting the number of missing variables in an observation",
                 sub = "Check whether the outcome variable is an important variable",
                 roundint = TRUE, 
                 clip.facs = TRUE)
    })
    
    

    
    
    output$infomissingnessA1 <- renderPlot({
      #dat_shadow <- dat
      dat$POPULATION_shadow <- is.na(dat$POPULATION) + 0
      dat$AGE25_PROPTN_shadow <- is.na(dat$AGE25_PROPTN) + 0
      dat$AGE_MEDIAN_shadow <- is.na(dat$AGE_MEDIAN) + 0
      dat$AGE50_PROPTN_shadow <- is.na(dat$AGE50_PROPTN) + 0
      dat$GDP_shadow <- is.na(dat$GDP) + 0
      dat$INFANT_MORT_shadow <- is.na(dat$INFANT_MORT)+0
      dat$DOCS_shadow <- is.na(dat$DOCS)+0
      dat$VAX_RATE_shadow <- is.na(dat$VAX_RATE)+0
      dat$HEALTHCARE_COST_shadow <- is.na(dat$HEALTHCARE_COST)+0
      dat$DEATH_RATE_shadow <- is.na(dat$DEATH_RATE)+0
      dat$OBS_TYPE_shadow <- is.na(dat$OBS_TYPE)+0
  #    MplsStops$gender_shadow <- is.na(MplsStops$gender) + 0
      
      # Create a data frame of missing counts for selected variables
      missing_counts <- data.frame(
        Variable = c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN", "AGE50_PROPTN", "GDP","INFANT_MORT","DOCS","VAX_RATE","HEALTHCARE_COST","DEATH_RATE","OBS_TYPE"),
        Missing = colSums(dat[, c("POPULATION_shadow", "AGE25_PROPTN_shadow", 
                                  "AGE_MEDIAN_shadow", "AGE50_PROPTN_shadow", 
                                  "GDP_shadow","INFANT_MORT_shadow","DOCS_shadow","VAX_RATE_shadow","HEALTHCARE_COST_shadow","DEATH_RATE_shadow","OBS_TYPE_shadow")])
      )
      
      # Plot: Barplot of missing counts
      barplot(
        missing_counts$Missing,
        names.arg = missing_counts$Variable,
        col = "steelblue",
        main = "Informative Missingness (Shadow Variables)",
        ylab = "Number of Missing Values",
        las = 2
      )
    })
    
    # Model for POPULATION_shadow
 #   output$shadowModelPOP <- renderPlot({
  #    tree <- rpart(POPULATION_shadow ~ ., data = dat[, !grepl("shadow", names(dat)) | names(dat) == "POPULATION_shadow"], method = "class")
  #    rpart.plot(tree, main = "Tree for POPULATION missingness")
  #  })
#    output$infomiss_summ <- renderPrint({
      
#      summary(glm(POPULATION_shadow ~ GDP + AGE_MEDIAN, family = "binomial", data = dat))
#    })
    
    imputation_model <- reactive({
      set.seed(123)
      
      # ✅ Create dat_local — this is currently commented in your code!
    #  dat_local <- dat %>% mutate(ID = row_number())
      
      # Split data
      split_idx <- createDataPartition(dat$DEATH_RATE, p = 0.8, list = FALSE)
      train_data <- dat[split_idx, ]
      test_data <- dat[-split_idx, ]
      
      # 🔍 Only impute variables with missing values
      missing_cols <- names(train_data)[colSums(is.na(train_data)) > 0]
      # Recipe
      deathrate_recipe <- recipe(DEATH_RATE ~ ., data = train_data) %>%
        update_role(ID, new_role = "id") %>%
        update_role(OBS_TYPE, new_role = "split") %>%
        step_rm(has_role("split")) %>%
        step_impute_knn(all_of(missing_cols), neighbors = 5) %>%
        step_center(all_numeric_predictors()) %>%
        step_scale(all_numeric_predictors()) %>%
        step_dummy(all_nominal_predictors())
      
      # Prep and bake
      prep_recipe <- prep(deathrate_recipe, training = train_data)
      train_processed <- bake(prep_recipe, new_data = train_data)
      test_processed <- bake(prep_recipe, new_data = test_data)
      
      # Model
      ctrl <- trainControl(method = "cv", number = 5)
      model <- train(DEATH_RATE ~ ., data = train_processed, method = "glmnet", trControl = ctrl)
      
      # Predict
      predictions <- predict(model, newdata = test_processed)
      residuals <- test_processed$DEATH_RATE - predictions
      
      list(actual = test_processed$DEATH_RATE,
           pred = predictions,
           residuals = residuals,
           model = model)
    })
    
    
    # Imputed dataset reactive
    imputed_data <- reactive({
      set.seed(123)
      dat <- dat %>% mutate(ID = row_number())
      
      split_idx <- createDataPartition(dat$DEATH_RATE, p = 0.8, list = FALSE)
      train_data <- dat[split_idx, ]
      
      # 🔍 Only impute variables with missing values
      missing_cols <- names(train_data)[colSums(is.na(train_data)) > 0]
      
      impute_recipe <- recipe(DEATH_RATE ~ ., data = train_data) %>%
        update_role(ID, new_role = "id") %>%
        update_role(OBS_TYPE, new_role = "split") %>%
        step_rm(has_role("split")) %>%
        step_impute_knn(all_of(missing_cols), neighbors = 5) %>%
        step_center(all_numeric_predictors()) %>%
        step_scale(all_numeric_predictors())
      #%>%
      #  step_dummy(all_nominal_predictors())
      
      prep_recipe <- prep(impute_recipe, training = train_data)
      train_imputed <- bake(prep_recipe, new_data = train_data)
      
      train_imputed
    })
    
    output$imputed_data_table <- DT::renderDataTable({
      DT::datatable(imputed_data(), options = list(pageLength = 10, scrollX = TRUE))
    })

    output$imputationA1 <- renderPlot({
      set.seed(123)
      
      # Define it here too!
#      dat_local <- dat %>% mutate(ID = row_number())
      
      split_idx <- createDataPartition(dat$DEATH_RATE, p = 0.8, list = FALSE)
      train_data <- dat[split_idx, ]
      test_data <- dat[-split_idx, ]
      
      impute_recipe <- recipe(DEATH_RATE ~ ., data = train_data) %>%
        update_role(ID, new_role = "id") %>%
        update_role(OBS_TYPE, new_role = "split") %>%
        step_rm(has_role("split")) %>%
        step_impute_knn(all_predictors(), neighbors = 5) %>%
        step_center(all_numeric_predictors()) %>%
        step_scale(all_numeric_predictors())
      #%>%
       # step_dummy(all_nominal_predictors())
      
#      prep_recipe <- prep(impute_recipe, training = train_data)
#      train_imputed <- bake(prep_recipe, new_data = train_data)
#      test_imputed <- bake(prep_recipe, new_data = test_data)
      
##      train_imputed <- na.omit(train_imputed)
#      test_imputed <- na.omit(test_imputed)
      
      ctrl <- trainControl(method = "cv", number = 5)
      model <- train(impute_recipe, data = train_data, method = "glmnet", trControl = ctrl)
      
      preds <- predict(model, newdata = test_data)
      resids <- preds - test_imputed$DEATH_RATE
#View(dat_local)      
      plot(resids,
           main = "Residual Plot (KNN Imputation + glmnet)",
           ylab = "Residuals",
           xlab = "Index",
           col = "darkorange",
           pch = 16)
      abline(h = 0, col = "red", lwd = 2)
    })
    
    
    
    output$residualPlot <- renderPlot({
      result <- imputation_model()
      
      plot(result$residuals,
           ylab = "Residuals", xlab = "Observation Index",
           main = "Residuals from Model",
           col = "darkgreen", pch = 16)
      abline(h = 0, col = "red", lty = 2)
    })
    
    output$downloadImputed <- downloadHandler(
      filename = function() {
        paste0("imputed_dataset_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(imputed_data(), file, row.names = FALSE)
      }
    )
    
#    output$imputation_metrics <- renderPrint({
#      result <- imputation_model()
#      postResample(pred = result$pred, obs = result$actual)
#    })
    output$outliersA1 <- renderPlot({
      selected_bin <- as.numeric(input$bin)      
      selected_var <- input$var
#      hist(dat[[selected_var]],main = paste("Histogram of", selected_var), xlab = selected_var, col = "lightblue",breaks = selected_bin)
      
    ggplot(data = dat) + 
       geom_histogram(mapping = aes(x = .data[[selected_var]]), bins = selected_bin) + 
       labs(title = paste("Histogram of ",selected_var), x = selected_var, y = "Count")
    })
    
    output$outliersA2 <- renderPlot({
      selected_coef <- as.numeric(input$bin)
      selected_var <- input$var
      #      hist(dat[[selected_var]],main = paste("Histogram of", selected_var), xlab = selected_var, col = "lightblue",breaks = selected_bin)
      
#      coef <- 6.3
      ggplot(data = dat) +
        geom_boxplot(mapping = aes(x = .data[[selected_var]]), coef = selected_coef, outlier.colour = "red") +
        labs(title = paste("Uni-variable boxplots at IQR multiplier of", selected_coef), x = selected_coef) +
        theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    })
    
    output$outliersA3 <- renderPlot({
      selected_coef <- as.numeric(input$Factor)
      selected_var1 <- input$var1
      selected_var2 <- input$var2
      #      hist(dat[[selected_var]],main = paste("Histogram of", selected_var), xlab = selected_var, col = "lightblue",breaks = selected_bin)
      x_data <- dat[[selected_var1]]
      y_data <- dat[[selected_var2]]
      #      coef <- 6.3
#      factor <- 3
      aplpack::bagplot(x = x_data, y = y_data, factor = selected_coef, show.bagpoints = FALSE, xlab = selected_var1, ylab = selected_var2)
    })
    
    output$boxcoxA1 <- renderPrint({
      bc_recipe <- recipe(~ POPULATION + GDP + INFANT_MORT + HEALTHCARE_COST, data = dat) %>%
        step_BoxCox(POPULATION, GDP, INFANT_MORT, HEALTHCARE_COST) %>%
        prep(data = dat)  # "prep" trains the recipe (that holds the BC transform step)
      bc_recipe$steps[[1]]$lambdas
    })
    View(dat)
    output$yeojohnsonA1 <- renderPlot({
      yj_recipe <- recipe(~ CODE+ POLITICS+ HEALTHCARE_BASIS+ OBS_TYPE, data = dat) %>%
        step_YeoJohnson(CODE, POLITICS, HEALTHCARE_BASIS, OBS_TYPE) %>%
        prep(data = dat)  # "prep" trains the recipe (that holds the YJ transform step)
      
      yj_recipe$steps[[1]]$lambdas
      plot(density(biomass$sulfur), main = "Sulfur before Yeo-Johnson transform")
    })
    
    output$rawdataset <- DT::renderDataTable({
      DT::datatable(data = as.data.frame(dat))
    })
    
    
}
