# library(shiny)
# library(shinyBS) # Additional Bootstrap Controls (tooltips)
# library(DT)
# library(corrgram)
# library(visdat)
# library(shinycssloaders) # busy spinner
# library(recipes) # The recipes package is central to preprocessing
# library(doParallel) # We employ a form of parallelism that works for MAC/Windows/Ubuntu
# library(caret) # This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
# library(rlang)
# #library(rJava)
# library(devtools)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("BiocParallel", type="binary")
# #library(BiocManager)
# library(dplyr)
# library(cli)
#  if (!library("mixOmics", logical.return = TRUE)) {
#    BiocManager::install("mixOmics", update = FALSE, ask = FALSE)  # This has moved from CRAN to Bioconductor
#  }
#  library(mixOmics)
#  library(rlist)
#  library(ggplot2)
#  library(butcher)

#  if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
# 
#  options(repos = BiocManager::repositories())   # make sure Bioc repos present
# 
#  for (pkg in c("BiocParallel", "mixOmics")) {
#    if (!requireNamespace(pkg, quietly = TRUE))
#      BiocManager::install(pkg, ask = FALSE, update = TRUE)
# }
#  install.packages('seriation')
#  install.packages('rlang')
#  install.packages("devtools")
#  install.packages("lubridate")
#  install.packages("zoo")

# Step 1: Clean up any lingering lock folder
unlink("P:/R/win-library/4.4.0/00LOCK", recursive = TRUE)

install.packages("pkgbuild", dependencies = TRUE)  #installed by Mahi
install.packages("systemfonts")
install.packages("textshaping")
install.packages("elasticnet")
install.packages("svmRadial")
#install.packages("ellipsis") #installed by Mahi
#install.packages("kernlab") #installed by Mahi
install.packages("randomForest") #installed by Mahi
#install.packages("LiblineaR") #installed by Mahi
install.packages("party") #installed by Mahi
#install.packages("earth", type = "source") #installed by Mahi
install.packages("cubist") #installed by Mahi



library(zoo)
library(shiny)
library(shinyBS) # Additional Bootstrap Controls (tooltips)
library(DT)
library(corrgram)
library(visdat)
library(shinycssloaders) # busy spinner
library(recipes) # The recipes package is central to preprocessing
library(doParallel) # We employ a form of parallelism that works for MAC/Windows/Ubuntu
library(caret) # This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
library(rlang)
library(rJava)
library(devtools)
library(BiocManager)
library(dplyr)
library(pkgbuild) # installed by Mahi
library(ellipsis) # installed by Mahi
library(devtools) # installed by Mahi
library(randomForest) # installed by Mahi
library(LiblineaR)
library(cli)
library(bslib)
library(lubridate)
if (!library("mixOmics", logical.return = TRUE)) {
  BiocManager::install("mixOmics", update = FALSE, ask = FALSE)  # This has moved from CRAN to Bioconductor
}
library(mixOmics)
library(rlist)
library(ggplot2)
library(butcher)
library(httpuv)
library(ranger)
library(xgboost)
library(GGally)
library(fastICA)
library(dplyr)
library(e1071) #installed by Mahi
library(earth) #installed by Mahi
#library(partykit) #installed by Mahi
library(extraTrees) #installed by Mahi
library(rJava) #installed by Mahi

options(digits = 3)

#dummy ("zv", "impute_knn", "center", "scale", "dummy","interact")

#glmnet_initial <- c("naomit", "month", "dummy") # <-- These are arbitrary starting values. Set these to your best recommendation
glmnet_initial <- c("zv", "impute_knn", "center", "scale", "dummy","interact" ) # <-- These are arbitrary starting values. Set these to your best recommendation
pls_initial <- c( "zv","impute_knn","center","scale", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
rpart_initial <- c("impute_knn","dummy") # <-- These are arbitrary starting values. Set these to your best recommendation
ridge_initial <- c("zv", "impute_knn", "center", "scale", "dummy") # <-- These are arbitrary starting values. Set these to your best recommendation
NN_initial <- c("zv", "impute_knn", "center", "scale", "dummy") # <-- These are arbitrary starting values. Set these to your best recommendation
svmr_initial <- c("zv", "impute_knn", "center", "scale", "dummy") # <-- These are arbitrary starting values. Set these to your best recommendation
xgb_initial <- c("zv", "impute_knn", "center", "scale", "dummy")  # or any other good default
rf_initial <- c("zv", "impute_knn", "center", "scale", "dummy") # added by Mahi
svml_initial <- c("zv", "impute_knn", "center", "scale", "dummy") # added by Mahi
ctree2_initial <- c("zv", "impute_knn", "center", "scale", "dummy")  # or your preferred steps added by Mahi
#gbm_initial <- c("impute_knn", "center", "scale", "dummy", "zv") #Added by Mahi
gbm_initial <- c("zv", "impute_knn", "center", "scale", "dummy") #Added by Mahi
glmStepAIC_initial <- c("zv", "impute_knn", "center", "scale", "dummy") #Added by Mahi
#earth_initial <- c("zv", "impute_median", "center", "scale", "dummy") #Added by Mahi
cubist_initial <- c("zv", "impute_knn", "center", "scale", "dummy") #Added by Mahi
treebag_initial <- c("zv", "impute_knn", "center", "scale", "dummy") #Added by Mahi
extratrees_initial <- c("zv", "impute_knn", "center", "scale")  # Added by Mahi
ranger_initial <- c("zv", "impute_knn", "center", "scale", "dummy")  # Added by Mahi


# maintenance point ---------------------------------------------------------------------------------------------------------------------------
# add further preprocessing choices for the new methods here




startMode <- function(Parallel = TRUE) {
  if (Parallel) {
    outfile <- tempfile(pattern = "output")
    unlink(outfile)
    clus <- makeCluster(min(c(3,detectCores(all.tests = FALSE, logical = TRUE))), outfile = outfile)
    registerDoParallel(clus)
    list("cluster" = clus, "outfile" = outfile)
  } else {
    NULL
  }
}

stopMode <- function(obj) {
  if (!is.null(obj)) {
    stopCluster(obj$cluster)
    lines <- readLines(con = obj$outfile)
    lapply(paste0(lines, "\n"), FUN = cat)
    unlink(obj$outfile)
    registerDoSEQ()
  }
}

ppchoices <- c("impute_knn", "impute_bag", "impute_median", "impute_mode", "YeoJohnson", "naomit", 
               "pca", "pls", "ica", "center", "scale", "month", "dow", "dateDecimal", "nzv", "zv", "other", 
               "dummy", "poly", "interact", "indicate_na", "corr","boxcox")

# This function turns the method's selected preprocessing into a recipe that honours the same order. 
# You are allowed to add more recipe steps to this.
dynamicSteps <- function(recipe, preprocess) {
  if (is.null(preprocess)) {
    stop("The preprocess list is NULL - check that you are using the correct control identifier")
  }
  for (s in preprocess) {
    if (s == "impute_knn") {
      recipe <- step_impute_knn(recipe, all_numeric_predictors(), neighbors = 5)
      recipe <- step_impute_knn(recipe, all_nominal_predictors(), neighbors = 5)
#    if (s == "impute_knn") {
#      has_vars <- length(recipe$var_info$variable[recipe$var_info$role == "predictor" & recipe$var_info$type %in% c("numeric", "nominal")]) > 0
#      if (has_vars) {
#        recipe <- step_impute_knn(recipe, all_numeric_predictors(), all_nominal_predictors(), neighbors = 5)
#      }
    } else if (s == "impute_bag") {
      recipe <- step_impute_bag(recipe, all_numeric_predictors(), all_nominal_predictors(), trees = 25) # 25 is a reasonable guess
    } else if (s == "impute_median") {
      recipe <- step_impute_median(recipe, all_numeric_predictors())  # use with "impute_mode"
    } else if (s == "impute_mode") {
      recipe <- recipes::step_impute_mode(recipe, all_nominal_predictors())  # use with "impute_median"
    } else if (s == "YeoJohnson") {
      recipe <- recipes::step_YeoJohnson(recipe, all_numeric_predictors()) 
    } else if (s == "naomit") {
      recipe <- recipes::step_naomit(recipe, all_predictors(), skip = TRUE)  
    } else if (s == "pca") {
      recipe <- recipes::step_pca(recipe, all_numeric_predictors(), num_comp = 25) # 25 is a big enough guess
    } else if (s == "pls") {
      recipe <- recipes::step_pls(recipe, all_numeric_predictors(), outcome = "Response", num_comp = 25) # 25 is a big enough guess
    } else if (s == "ica") {
      recipe <- recipes::step_ica(recipe, all_numeric_predictors(), num_comp = 25) # 25 is a big enough guess
    } else if (s == "center") {
      recipe <- recipes::step_center(recipe, all_numeric_predictors()) # this needs to be after any reshaping
    } else if (s == "scale") {
      recipe <- recipes::step_scale(recipe, all_numeric_predictors())
    } else if (s == "month") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("month"), ordinal = FALSE)  # uses step_date to generate month-of-year
    } else if (s == "dow") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("dow"), ordinal = FALSE)   # uses step_date to generate day-of-week
    } else if (s == "dateDecimal") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("decimal"), ordinal = FALSE)   # uses step_date to generate decimal date
    } else if (s == "zv") {
      recipe <- recipes::step_zv(recipe, all_predictors())
    } else if (s == "nzv") {
      recipe <- recipes::step_nzv(recipe, all_predictors(), freq_cut = 95/5, unique_cut = 10)
    } else if (s == "other") {
      recipe <- recipes::step_other(recipe, all_nominal_predictors())
    } else if (s == "dummy") {
      recipe <- recipes::step_dummy(recipe, all_nominal_predictors(), one_hot = FALSE) # this needs to follow dealing with missing values
    } else if (s == "poly") {
     # recipe <- recipes::step_poly(recipe, all_numeric_predictors(), degree = 2)
      recipe <- recipes::step_poly(recipe, all_of(c("Coffee", "Exercise", "Alcohol")), degree = 2)
    } else if (s == "interact") {
      recipe <- recipes::step_interact(recipe, terms = ~ all_numeric_predictors():all_numeric_predictors())  # only numeric predictors allowed (this needs to follow dealing with categorical variables)
    } else if (s == "corr") {
      recipe <- recipes::step_corr(recipe, all_numeric_predictors(), threshold = 0.9)
    } else if (s == "indicate_na") {
      recipe <- recipes::step_indicate_na(recipe, all_predictors()) #shadow variables (this needs to precede dealing with NA)
    } else if (s == "boxcox") { # added a new pre-processing step by Mahi
      recipe <- recipes::step_BoxCox(recipe, all_numeric_predictors())
    } else if (s == "rm") {
      # intentionally blank
    } else {
      stop(paste("Attempting to use an unknown recipe step:", s))
    }
  }
  recipe
}

description <- function(name) {
  regexName <- paste0("^", name, "$") # force an regular expression exact match
  mlist <- caret::getModelInfo(model = regexName)[[name]]
  line1 <- paste0("Method \"", name, "\" is able to do ", paste(collapse = " and ", mlist$type), ".")
  line2 <- paste0("It uses parameters: ", paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3 <- paste0("Its characteristics are: ", paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line1, line2, line3)
}


# attempts to keep the model file size small by not saving the global environment with each model
saveToRds <- function(model, name) {
  try(
    # ensure that WEKA based models can be restored
    if (!is.null(model$finalModel$classifier)) {
      rJava::.jcache(model$finalModel$classifier)
    }, silent = TRUE
  )
  
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
 model2 <- butcher::axe_env(model, verbose = TRUE) # strip environments from the object to keep its size small. commented by Mahi for Now
#  model2 <- tryCatch({
    # Skip axe_env for cubist or problematic train.recipe objects
 #   if (!inherits(model, "train.recipe") || model$method != "cubist") {
#      butcher::axe_env(model, verbose = TRUE)
#    } else {
#      message("Skipping axe_env for method: ", model$method)
#      model
#    }
#  }, error = function(e) {
#    message("axe_env failed: ", e$message)
#    model  # fallback to original model
 # })
  
    #print(butcher::weigh(model2, threshold = 5, units = "MB"))
  saveRDS(model2, file)
}

loadRds <- function(name, session) {
  rdsfile <- file.path(".","SavedModels", paste0(name, ".rds"))
  if (!file.exists(rdsfile)) {
    showNotification("Model needs to be trained first", session = session, duration = 3)
    return(NULL)
  }
  showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
  model <- readRDS(file = rdsfile)
  
  # try to update the preprocessing steps with the ones that were used
  steps <- model$recipe$steps
  seld <- c()
  for (step in steps) {
    s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
    if (s == "date") {
      s <- step$features[1]
    }
    seld <- c(seld, s)
  }
  preprocessingInputId <- paste0(name, "_Preprocess")
  updateSelectizeInput(session = session, inputId = preprocessingInputId, choices = ppchoices, selected = seld)
  if (length(seld) > 0) {
    showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 5)
  }
  model
}

deleteRds <- function(name) {
  rdsfile <- file.path(".","SavedModels", paste0(name, ".rds"))
  if (file.exists(rdsfile)) {
    ok <- unlink(rdsfile, force = TRUE)
  } else {
    ok <- TRUE
  }
  ok
}