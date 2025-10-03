shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Your Name Here"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = paste("This will utilise all", detectCores(), "available CPUs during training")),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics")
                         ),
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_MethodSummary"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  h3("Hyperparameter Tuning:"),
                                  plotOutput(outputId = "glmnet_ModelTune"),
                                  hr(),
                                  h3("Recipe:"),
                                  htmlOutput(outputId = "glmnet_RecipePrint"),
                                  h3("Outputs"),
                                  tableOutput(outputId = "glmnet_RecipeOutput"),
                                  
                                  fluidRow(
                                    column(width=6,
                                           h3("Training Summary:"),
                                           verbatimTextOutput(outputId = "glmnet_TrainSummary")
                                    ),
                                    column(width=6,
                                           h3("Coefficients"),   # Not all method can produce coefficients
                                           wellPanel(
                                             tableOutput(outputId = "glmnet_Coef")
                                           )
                                    )
                                  )
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_MethodSummary"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  h3("Hyperparameter Tuning:"),
                                  plotOutput(outputId = "pls_ModelTune"),
                                  hr(),
                                  h3("Recipe:"),
                                  htmlOutput(outputId = "pls_RecipePrint"),
                                  h3("Outputs"),
                                  tableOutput(outputId = "pls_RecipeOutput"),
                                  fluidRow(
                                    column(width=6,
                                           h3("Training Summary:"),
                                           verbatimTextOutput(outputId = "pls_TrainSummary"),
                                    ),
                                    column(width=6,
                                           h3("Coefficients"),   # Not all method can produce coefficients
                                           wellPanel(
                                             tableOutput(outputId = "pls_Coef")
                                           )
                                    )
                                  )
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_MethodSummary"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  h3("Hyperparameter Tuning:"),
                                  plotOutput(outputId = "rpart_ModelTune"),
                                  hr(),
                                  h3("Model tree:"), #  <- this tree-plot is unique to the rpart method
                                  plotOutput(outputId = "rpart_ModelTree"),
                                  hr(),
                                  h3("Recipe:"),
                                  htmlOutput(outputId = "rpart_RecipePrint"),
                                  h3("Outputs"),
                                  tableOutput(outputId = "rpart_RecipeOutput"),
                                  fluidRow(
                                    column(width=6,
                                           h3("Training Summary:"),
                                           verbatimTextOutput(outputId = "rpart_TrainSummary")
                                    )
                                  )
                         ),
                         
                         tabPanel("Neural Network Model",
                                  verbatimTextOutput(outputId = "NN_MethodSummary"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "NN_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(NN_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = NN_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "NN_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "NN_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "NN_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "NN_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "NN_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "NN_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "NN_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "NN_Metrics"),
                                  hr(),
                                  h3("Hyperparameter Tuning:"),
                                  plotOutput(outputId = "NN_ModelTune"),
                                  hr(),
                         #         h3("Model tree:"), #  <- this tree-plot is unique to the rpart method
                        #          plotOutput(outputId = "ridge_ModelTree"),
                       #           hr(),
                                  h3("Recipe:"),
                                  htmlOutput(outputId = "NN_RecipePrint"),
                                  h3("Outputs"),
                                  tableOutput(outputId = "NN_RecipeOutput"),
                                  fluidRow(
                                    column(width=6,
                                           h3("Training Summary:"),
                                           verbatimTextOutput(outputId = "NN_TrainSummary")
                                    )
                                  )
                         ),
                       
                       tabPanel("SVM Radial",
                                verbatimTextOutput(outputId = "svmr_MethodSummary"),
                                fluidRow(
                                  column(width = 4,
                                         # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                         selectizeInput(inputId = "svmRadial_Preprocess",
                                                        label = "Pre-processing",
                                                        choices = unique(c(svmr_initial, ppchoices)),
                                                        multiple = TRUE,
                                                        selected = svmr_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                         bsTooltip(id = "svmRadial_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "svmr_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                         bsTooltip(id = "svmr_Go", title = "This will train or retrain your model (and save it)")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "svmr_Load", label = "Load", icon = icon("file-arrow-up")),
                                         bsTooltip(id = "svmr_Load", title = "This will reload your saved model")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "svmr_Delete", label = "Forget", icon = icon("trash-can")),
                                         bsTooltip(id = "svmr_Delete", title = "This will remove your model from memory")
                                  )
                                ),
                                hr(),
                                h3("Resampled performance:"),
                                tableOutput(outputId = "svmr_Metrics"),
                                hr(),
                                h3("Hyperparameter Tuning:"),
                                plotOutput(outputId = "svmr_ModelTune"),
                                hr(),
                                #         h3("Model tree:"), #  <- this tree-plot is unique to the rpart method
                                #          plotOutput(outputId = "ridge_ModelTree"),
                                #           hr(),
                                h3("Recipe:"),
                                htmlOutput(outputId = "svmr_RecipePrint"),
                                h3("Outputs"),
                                tableOutput(outputId = "svmr_RecipeOutput"),
                                fluidRow(
                                  column(width=6,
                                         h3("Training Summary:"),
                                         verbatimTextOutput(outputId = "svmr_TrainSummary")
                                  )
                                )
                       ),
                       
                       
                       #Code for XGB Tab:
                       
                       tabPanel("XGB Tree",
                                verbatimTextOutput(outputId = "xgb_MethodSummary"),
                                fluidRow(
                                  column(width = 4,
                                         selectizeInput(inputId = "xgb_Preprocess",
                                                        label = "Pre-processing",
                                                        choices = unique(c(xgb_initial, ppchoices)),
                                                        multiple = TRUE,
                                                        selected = xgb_initial),
                                         bsTooltip(id = "xgb_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "xgb_Go", label = "Train", icon = icon("play")),
                                         bsTooltip(id = "xgb_Go", title = "This will train or retrain your model (and save it)")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "xgb_Load", label = "Load", icon = icon("file-arrow-up")),
                                         bsTooltip(id = "xgb_Load", title = "This will reload your saved model")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "xgb_Delete", label = "Forget", icon = icon("trash-can")),
                                         bsTooltip(id = "xgb_Delete", title = "This will remove your model from memory")
                                  )
                                ),
                                hr(),
                                h3("Resampled performance:"),
                                tableOutput(outputId = "xgb_Metrics"),
                                hr(),
                                h3("Hyperparameter Tuning:"),
                                plotOutput(outputId = "xgb_ModelTune"),
                                hr(),
                                h3("Recipe:"),
                                htmlOutput(outputId = "xgb_RecipePrint"),
                                h3("Outputs"),
                                tableOutput(outputId = "xgb_RecipeOutput"),
                                fluidRow(
                                  column(width = 6,
                                         h3("Training Summary:"),
                                         verbatimTextOutput(outputId = "xgb_TrainSummary")
                                  )
                                )
                       ),
                       
                       ## Adding code for Random Forest
                       
                       tabPanel("Random Forest",
                                verbatimTextOutput(outputId = "rf_MethodSummary"),
                                fluidRow(
                                  column(width = 4,
                                         selectizeInput(inputId = "rf_Preprocess",
                                                        label = "Pre-processing",
                                                        choices = unique(c(rf_initial, ppchoices)),
                                                        multiple = TRUE,
                                                        selected = rf_initial),
                                         bsTooltip(id = "rf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "rf_Go", label = "Train", icon = icon("play")),
                                         bsTooltip(id = "rf_Go", title = "This will train or retrain your model (and save it)")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "rf_Load", label = "Load", icon = icon("file-arrow-up")),
                                         bsTooltip(id = "rf_Load", title = "This will reload your saved model")
                                  ),
                                  column(width = 1,
                                         actionButton(inputId = "rf_Delete", label = "Forget", icon = icon("trash-can")),
                                         bsTooltip(id = "rf_Delete", title = "This will remove your model from memory")
                                  )
                                ),
                                hr(),
                                h3("Resampled performance:"),
                                tableOutput(outputId = "rf_Metrics"),
                                hr(),
                                h3("Hyperparameter Tuning:"),
                                plotOutput(outputId = "rf_ModelTune"),
                                hr(),
                                h3("Recipe:"),
                                htmlOutput(outputId = "rf_RecipePrint"),
                                h3("Outputs"),
                                tableOutput(outputId = "rf_RecipeOutput"),
                                fluidRow(
                                  column(width = 6,
                                         h3("Training Summary:"),
                                         verbatimTextOutput(outputId = "rf_TrainSummary")
                                  )
                                )
                       ),
                       
                       
                       ## Added CODE FOR SVM LINEAR - 2
                       
                       tabPanel("SVM Linear2",
                                verbatimTextOutput(outputId = "svml2_MethodSummary"),
                                fluidRow(
                                  column(width = 4,
                                         selectizeInput("svml2_Preprocess", "Pre-processing",
                                                        choices = unique(c(svml_initial, ppchoices)),
                                                        multiple = TRUE,
                                                        selected = svml_initial),
                                         bsTooltip(id = "svml2_Preprocess", title = "Preprocessing steps", placement = "top")
                                  ),
                                  column(width = 1,
                                         actionButton("svml2_Go", "Train", icon = icon("play")),
                                         bsTooltip("svml2_Go", title = "Train and save model")
                                  ),
                                  column(width = 1,
                                         actionButton("svml2_Load", "Load", icon = icon("file-arrow-up")),
                                         bsTooltip("svml2_Load", title = "Load previously saved model")
                                  ),
                                  column(width = 1,
                                         actionButton("svml2_Delete", "Forget", icon = icon("trash-can")),
                                         bsTooltip("svml2_Delete", title = "Remove model from memory")
                                  )
                                ),
                                hr(),
                                h3("Resampled performance:"),
                                tableOutput("svml2_Metrics"),
                                hr(),
                                h3("Hyperparameter Tuning:"),
                                plotOutput("svml2_ModelTune"),
                                hr(),
                                h3("Recipe:"),
                                htmlOutput("svml2_RecipePrint"),
                                h3("Outputs"),
                                tableOutput("svml2_RecipeOutput"),
                                fluidRow(
                                  column(width = 6,
                                         h3("Training Summary:"),
                                         verbatimTextOutput("svml2_TrainSummary")
                                  )
                                )
                       ),
                       
                       
                       
                       
                ## Added Code for CTREE2
                
                tabPanel("ctree2",
                         verbatimTextOutput(outputId = "ctree2_MethodSummary"),
                         fluidRow(
                           column(width = 4,
                                  selectizeInput("ctree2_Preprocess", "Pre-processing",
                                                 choices = unique(c(ctree2_initial, ppchoices)),
                                                 multiple = TRUE,
                                                 selected = ctree2_initial),
                                  bsTooltip(id = "ctree2_Preprocess", title = "Preprocessing steps", placement = "top")
                           ),
                           column(width = 1,
                                  actionButton("ctree2_Go", "Train", icon = icon("play")),
                                  bsTooltip("ctree2_Go", title = "Train and save model")
                           ),
                           column(width = 1,
                                  actionButton("ctree2_Load", "Load", icon = icon("file-arrow-up")),
                                  bsTooltip("ctree2_Load", title = "Load previously saved model")
                           ),
                           column(width = 1,
                                  actionButton("ctree2_Delete", "Forget", icon = icon("trash-can")),
                                  bsTooltip("ctree2_Delete", title = "Remove model from memory")
                           )
                         ),
                         hr(),
                         h3("Resampled performance:"),
                         tableOutput("ctree2_Metrics"),
                         hr(),
                         h3("Hyperparameter Tuning:"),
                         plotOutput("ctree2_ModelTune"),
                         hr(),
                         h3("Recipe:"),
                         htmlOutput("ctree2_RecipePrint"),
                         h3("Outputs"),
                         tableOutput("ctree2_RecipeOutput"),
                         fluidRow(
                           column(width = 6,
                                  h3("Training Summary:"),
                                  verbatimTextOutput("ctree2_TrainSummary")
                           )
                         )
                ),
                
  #----------Gradient Boosting Machine lm MEHTOD----------
  tabPanel("Boosted Tree (GBM) Model",
           verbatimTextOutput(outputId = "gbm_MethodSummary"),
           fluidRow(
             column(width = 4,
                    selectizeInput("gbm_Preprocess", "Pre-processing",
                                   choices = unique(c(gbm_initial, ppchoices)),
                                   multiple = TRUE,
                                   selected = gbm_initial),
                    bsTooltip(id = "gbm_Preprocess", title = "Preprocessing steps", placement = "top")
             ),
             column(width = 1,
                    actionButton("gbm_Go", "Train", icon = icon("play")),
                    bsTooltip("gbm_Go", title = "Train and save model")
             ),
             column(width = 1,
                    actionButton("gbm_Load", "Load", icon = icon("file-arrow-up")),
                    bsTooltip("gbm_Load", title = "Load previously saved model")
             ),
             column(width = 1,
                    actionButton("gbm_Delete", "Forget", icon = icon("trash-can")),
                    bsTooltip("gbm_Delete", title = "Remove model from memory")
             )
           ),
           hr(),
           h3("Resampled performance:"),
           tableOutput("gbm_Metrics"),
           hr(),
           h3("Hyperparameter Tuning:"),
           plotOutput("gbm_ModelTune"),
           hr(),
           h3("Recipe:"),
           htmlOutput("gbm_RecipePrint"),
           h3("Outputs"),
           tableOutput("gbm_RecipeOutput"),
           fluidRow(
             column(width = 6,
                    h3("Training Summary:"),
                    verbatimTextOutput("gbm_TrainSummary")
             )
           )
  ),
  
  
#------------------------------glmstepAIC mehtod----------------------------



tabPanel("GLM StepAIC Model",
         verbatimTextOutput("glmStepAIC_MethodSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput("glmStepAIC_Preprocess", "Pre-processing",
                                 choices = unique(c(glmStepAIC_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = glmStepAIC_initial
                  )
           ),
           column(width = 1,
                  actionButton("glmStepAIC_Go", "Train", icon = icon("play"))
           ),
           column(width = 1,
                  actionButton("glmStepAIC_Load", "Load", icon = icon("file-arrow-up"))
           ),
           column(width = 1,
                  actionButton("glmStepAIC_Delete", "Forget", icon = icon("trash-can"))
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput("glmStepAIC_Metrics"),
         hr(),
         h3("Recipe:"),
         htmlOutput("glmStepAIC_RecipePrint"),
         h3("Outputs"),
         tableOutput("glmStepAIC_RecipeOutput"),
         fluidRow(
           column(width=6,
                  h3("Training Summary:"),
                  verbatimTextOutput("glmStepAIC_TrainSummary")
           ),
           column(width=6,
                  h3("Coefficients"),
                  wellPanel(tableOutput("glmStepAIC_Coef"))
           )
         )
),


#------------------------------Cubist Model------------------------------------------
tabPanel("Cubist Model",
         verbatimTextOutput(outputId = "cubist_MethodSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput("cubist_Preprocess", "Pre-processing",
                                 choices = unique(c(rf_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = c("zv", "center", "scale", "impute_median", "dummy")
                  ),
                  bsTooltip(id = "cubist_Preprocess", title = "Preprocessing steps", placement = "top")
           ),
           column(width = 1,
                  actionButton("cubist_Go", "Train", icon = icon("play")),
                  bsTooltip("cubist_Go", title = "Train and save model")
           ),
           column(width = 1,
                  actionButton("cubist_Load", "Load", icon = icon("file-arrow-up")),
                  bsTooltip("cubist_Load", title = "Load previously saved model")
           ),
           column(width = 1,
                  actionButton("cubist_Delete", "Forget", icon = icon("trash-can")),
                  bsTooltip("cubist_Delete", title = "Remove model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput("cubist_Metrics"),
         hr(),
         h3("Hyperparameter Tuning:"),
         plotOutput("cubist_ModelTune"),
         hr(),
         h3("Recipe:"),
         htmlOutput("cubist_RecipePrint"),
         h3("Outputs"),
         tableOutput("cubist_RecipeOutput"),
         fluidRow(
           column(width = 6,
                  h3("Training Summary:"),
                  verbatimTextOutput("cubist_TrainSummary")
           )
         )
),


#--------------------------Bagged CART Mehtod----------


tabPanel("Bagged CART (Treebag) Model",
         verbatimTextOutput("treebag_MethodSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput("treebag_Preprocess", "Pre-processing",
                                 choices = unique(c(treebag_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = treebag_initial
                  )
           ),
           column(width = 1,
                  actionButton("treebag_Go", "Train", icon = icon("play"))
           ),
           column(width = 1,
                  actionButton("treebag_Load", "Load", icon = icon("file-arrow-up"))
           ),
           column(width = 1,
                  actionButton("treebag_Delete", "Forget", icon = icon("trash-can"))
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput("treebag_Metrics"),
         hr(),
         h3("Recipe:"),
         htmlOutput("treebag_RecipePrint"),
         h3("Outputs"),
         tableOutput("treebag_RecipeOutput"),
         fluidRow(
           column(width = 6,
                  h3("Training Summary:"),
                  verbatimTextOutput("treebag_TrainSummary")
           )
         )
),

#--------------------------ExtraTress Method------------------

tabPanel("Extra Trees Model",
         verbatimTextOutput("extratrees_MethodSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput("extratrees_Preprocess", "Pre-processing",
                                 choices = unique(c(extratrees_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = extratrees_initial
                  )
           ),
           column(width = 1, actionButton("extratrees_Go", "Train", icon = icon("play"))),
           column(width = 1, actionButton("extratrees_Load", "Load", icon = icon("file-arrow-up"))),
           column(width = 1, actionButton("extratrees_Delete", "Forget", icon = icon("trash-can")))
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput("extratrees_Metrics"),
         hr(),
         h3("Recipe:"),
         htmlOutput("extratrees_RecipePrint"),
         h3("Outputs"),
         tableOutput("extratrees_RecipeOutput"),
         fluidRow(
           column(width = 6,
                  h3("Training Summary:"),
                  verbatimTextOutput("extratrees_TrainSummary")
           )
         )
),

#---------------Random Forest Ranger-----------------
tabPanel("Random Forest (ranger)",
         verbatimTextOutput(outputId = "ranger_MethodSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput("ranger_Preprocess", "Pre-processing",
                                 choices = unique(c(ranger_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = ranger_initial),
                  bsTooltip(id = "ranger_Preprocess", title = "Preprocessing steps", placement = "top")
           ),
           column(width = 1,
                  actionButton("ranger_Go", "Train", icon = icon("play")),
                  bsTooltip("ranger_Go", title = "Train and save model")
           ),
           column(width = 1,
                  actionButton("ranger_Load", "Load", icon = icon("file-arrow-up")),
                  bsTooltip("ranger_Load", title = "Load previously saved model")
           ),
           column(width = 1,
                  actionButton("ranger_Delete", "Forget", icon = icon("trash-can")),
                  bsTooltip("ranger_Delete", title = "Remove model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput("ranger_Metrics"),
         hr(),
         h3("Hyperparameter Tuning:"),
         plotOutput("ranger_ModelTune"),
         hr(),
         h3("Recipe:"),
         htmlOutput("ranger_RecipePrint"),
         h3("Outputs"),
         tableOutput("ranger_RecipeOutput"),
         fluidRow(
           column(width = 6,
                  h3("Training Summary:"),
                  verbatimTextOutput("ranger_TrainSummary")
           )
         )
)



                
                         
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                         
                         
                         
                         
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot", width = "600", height="600")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals", height="600")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals", height="600"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
