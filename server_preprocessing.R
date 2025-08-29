#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(corrplot)
library(visdat)
library(naniar)
library(recipes)
library(caret)
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(DT)
# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$SummaryA1 <- renderPrint({
    str(dat)
  })
  
  output$histogramsum <- renderPlot({
    selected_var <- input$var
    selected_col <- input$colour 
    selected_bins <- as.numeric(input$bins)
    hist(dat[[selected_var]],main=paste("Histogram Of ",selected_var),xlab=selected_var, col=selected_col,breaks=selected_bins)
  })
  
  output$Boxplotsum <- renderPlot({
    selected_boxvar <- input$var_box
    selected_range <- input$range
    selected_col <- input$colour
    boxplot(dat[[selected_boxvar]],main=paste("Boxplot of", selected_boxvar),xlab=selected_boxvar,col=selected_col,range=selected_range)
  })
  
  output$Barplotsum <- renderPlot({
    selected_varbar <- input$var_bar
    selected_col <- input$colour
    var_table <- table(dat[[selected_varbar]], useNA = "ifany")
    
    names(var_table)[is.na(names(var_table))] <- "NA"
    barplot(var_table,main=paste("Barplot of ",selected_varbar),xlab=selected_varbar,col=selected_col)
  })
  
  output$Corrrplotsum <- renderPlot({
    selected_corr <- input$corr_method
    numeric_vars <- dat[,sapply(dat,is.numeric)]
    selected_method <- input$Method_cor
    selected_type <- input$type_cor
    
    cor_matrix <- cor(numeric_vars,use = "pairwise.complete.obs",method = selected_corr)
    
    corrplot(cor_matrix,method=selected_method,type = selected_type,tl.col = "black",tl.cex = 1,diag = FALSE )
  })
  
  output$missingdatasum <- renderPlot({
    selectedmiss_met <- input$missing_method
    if (selectedmiss_met=="gg_miss_upset"){
      gg_miss_upset(dat)
    } else if(selectedmiss_met=="vis_dat"){
      vis_dat(dat)
    }
    else{
      vis_miss(dat,cluster = TRUE)
    }
  })
  
  output$Rawdatasum <- renderDataTable({
    
    req(dat)
    
    
    filtered_data <- dat[input$obs_range[1]:input$obs_range[2], ]
    
    if (input$operator_filter != "All") {
      if (input$operator_filter == "NA") {
        filtered_data <- filtered_data[is.na(filtered_data$POLITICS), ]
      } else {
        filtered_data <- filtered_data[filtered_data$POLITICS == input$operator_filter, ]
      }
    }
    
    filtered_data <- filtered_data %>%
      dplyr::mutate(across(where(is.numeric), ~ round(., 2)))
    filtered_data[] <- lapply(filtered_data, function(x) {
      ifelse(is.na(x), "NA", as.character(x))
    })
    
    DT::datatable(
      data = filtered_data,
      options = list(
        pageLength = 50,
        autoWidth = TRUE,
        lengthMenu = c(10, 25, 50, 100)
        #   scrollX = TRUE
      ),
      rownames = FALSE
      #,na="NA"
    )
    
  })
  
  cleaned_data <- reactive({
    var_thre <- input$var_thresold
    obs_thre <-   input$obs_thresold
    # This calculates the ratio of missingness of a vector.
    pMiss <- function(x){ sum(is.na(x))/length(x)*100 }
    cRatio <- apply(X = dat, MARGIN = 2, FUN = pMiss) # run pMiss for each column of the data frame
    dat_cleaned <- dat[,cRatio<var_thre]
    rRatio <- apply(X = dat_cleaned, MARGIN = 1, FUN = pMiss)  # run pMiss for each row of the data frame
    dat_cleaned <- dat_cleaned[rRatio<obs_thre,]
    
    dat_cleaned
  })
  
  output$cleaned_data <- renderPrint({
    original_dims <- dim(dat)
    cleaned_dims <- dim(cleaned_data())
    
    cat("Original Dimensions: ", original_dims[1], "rows,", original_dims[2], "columns\n")
    cat("After Filtering: ", cleaned_dims[1], "rows,", cleaned_dims[2], "columns\n")
  })
  
  output$cleaned_data_review <- renderDataTable({
    DT::datatable(cleaned_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$visdat_after_cleaning <- renderPlot({
    vis_miss(cleaned_data())
  })
  
  output$visdat_before_cleaning <- renderPlot({
    vis_miss(dat)
  })
  
  output$corr_missingval <- renderPlot({
    m <- is.na(dat) + 0
    cm <- colMeans(m)
    m <- m[, cm > 0 & cm < 1, drop = FALSE]
    corr_method <-   input$cor_method
    cor_order <-   if (input$corr_order == "None") NULL else input$corr_order
    
    corrgram::corrgram(cor(m,method=corr_method), order = cor_order, abs = TRUE)
    title(main = "Variable missing value correlation",
          sub = "Notice whether variables are missing in sets")
  })
  
  output$informative_missng <- renderPlot({
    
    dat1 <- dat
    dat1$ID <- 1:nrow(dat1)
    dat1 <- dat1[order(dat1$ID), ]
    # step 2
    dat1$missingness <- apply(X = is.na(dat1), MARGIN = 1, FUN = sum)
    
    # step 3
    tree <- caret::train(missingness ~ ., 
                         data = dat1, 
                         method = "rpart", 
                         na.action = na.rpart) # na.rpart means "rpart will deal with missing predictors intrinsically"
    # step 4
    rpart.plot(tree$finalModel, 
               main = "Predicting the number of missing variables in an observation",
               sub = "Check whether the outcome variable is an important variable",
               roundint = TRUE, 
               clip.facs = TRUE)
  })
  
  
  imputed_recipe <- reactive({
   req(input$apply_imputation)
    data_cleaned <-   cleaned_data()
    train <-  data_cleaned[data_cleaned$OBS_TYPE == "Train",]
    test <-   data_cleaned[data_cleaned$OBS_TYPE == "Test",]
    
    
    rec <- recipe(DEATH_RATE ~ ., data = train) %>%
      update_role("OBS_TYPE", new_role = "split") %>%
      {
        if (input$imputeMethod == "knn") {
          step_impute_knn(., all_predictors(), neighbors = input$knnValue)
        } else if (input$imputeMethod == "mean") {
          step_impute_mean(., all_numeric_predictors()) %>%
            step_impute_mode(., all_nominal_predictors())
        } else {
          step_impute_median(., all_numeric_predictors()) %>%
            step_impute_mode(., all_nominal_predictors())
        }
      } %>%
      {
        if (input$center) step_center(., all_numeric_predictors()) else .
      } %>%
      {
        if (input$scale) step_scale(., all_numeric_predictors()) else .
      } %>%
      step_dummy(all_nominal_predictors())
    
    ctrl <- trainControl("cv", number = 10, savePredictions = "final")
    model <- train(rec, data = train, method = "glmnet", metric = "RMSE", trControl = ctrl)
    
    list(model = model, test = test, train = train)
  })
  
  
  output$imputation_model <- renderPlot({
    model_data <- imputed_recipe()
    pred <- predict(model_data$model, newdata = model_data$test)
    ggplot(data.frame(Act = model_data$test$DEATH_RATE, Pred = pred),
           aes(Act, Pred)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, col = "red") +
      coord_equal() +
      theme_minimal()
  })
  
  output$modelSummary <- renderPrint({
    model_data <- imputed_recipe()
    
    predictions <- predict(model_data$model, newdata = model_data$test)
    actuals <- model_data$test$DEATH_RATE
    
    rmse <- sqrt(mean((predictions - actuals)^2))
    
    cat("Test RMSE Value is", round(rmse, 4))
  })
  
  output$imputated_Plot <- renderPlot({
    model_data <- imputed_recipe()
    plot(model_data$model)
  })
  
  output$imputated_coefficients <- renderPrint({
    model_data <- imputed_recipe()
    
    coefficients <- coef(model_data$model$finalModel, model_data$model$bestTune$lambda)
    print(coefficients)
  })
 
  output$outlier_residual_Plot <- renderPlot({
    model_data <- imputed_recipe()
    
    train_residuals <- model_data$train$DEATH_RATE - predict(model_data$model, newdata = model_data$train)
    test_residuals  <- model_data$test$DEATH_RATE - predict(model_data$model, newdata = model_data$test)
    
    residuals_all <- c(train_residuals, test_residuals)
    data_type <- factor(c(rep("Train", length(train_residuals)), rep("Test", length(test_residuals))))
    
    residual_iqr <- IQR(residuals_all)
    lower_bound <- quantile(residuals_all, 0.25) - input$IQRmultiplier * residual_iqr
    upper_bound <- quantile(residuals_all, 0.75) + input$IQRmultiplier * residual_iqr
    outlier_indices <- which(residuals_all < lower_bound | residuals_all > upper_bound)
    
    boxplot(residuals_all ~ data_type,
            main = "Residuals by Data Type",
            col = c("blue", "green"),
            ylab = "Residuals")
    
    points(rep(1:2, times = c(length(train_residuals), length(test_residuals)))[outlier_indices],
           residuals_all[outlier_indices],
           col = "red", pch = 19)
  })
  

  output$outlier_Table <- renderDT({
    
    model_data <- imputed_recipe()
    
    predic_train <- predict(model_data$model, newdata = model_data$train)
    predic_test <- predict(model_data$model, newdata = model_data$test)

    res <- c(model_data$train$DEATH_RATE - predic_train, model_data$test$DEATH_RATE - predic_test)
    data_all <- rbind(model_data$train, model_data$test)
    
    iqr <- IQR(res)
    lwb <- quantile(res, 0.25) - input$IQRmultiplier * iqr
    upb <- quantile(res, 0.75) + input$IQRmultiplier * iqr
    outliers <- res < lwb | res > upb
    
    datatable(data_all[outliers, ])
  })
  
}
