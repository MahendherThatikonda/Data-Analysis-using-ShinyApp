#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("DATA SCIENCE IN INDUSTRY - ASSIGNMENT 2 by Mahendher Thatikonda"),
  tabsetPanel(
    tabPanel("Exploratory Data Analysis",
             tabsetPanel(
               tabPanel("Summary",verbatimTextOutput(outputId="SummaryA1")),
               tabPanel("Histogram",
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectInput("var","Choose a Variable:",
                                        choices=c("POPULATION",
                                                  "AGE25_PROPTN",
                                                  "AGE_MEDIAN",
                                                  "AGE50_PROPTN",
                                                  "POP_DENSITY",
                                                  "GDP",
                                                  "INFANT_MORT",
                                                  "DOCS",
                                                  "VAX_RATE",
                                                  "HEALTHCARE_COST",
                                                  "DEATH_RATE")
                                        
                            ), sliderInput("bins","Choose the Bin Size:",min=5,max=50,value=15,step=5),
                            selectInput("colour","Choose a colour:",
                                        choices = c("red","blue","green","purple","orange","skyblue"),
                                        selected = "skyblue"),  
                          ),
                          mainPanel(plotOutput(outputId="histogramsum"))           
                        ) 
                        
                        
               ),
               tabPanel("Boxplot",
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectInput("var_box","Choose a Variable:",
                                        choices=c("POPULATION",
                                                  "AGE25_PROPTN",
                                                  "AGE_MEDIAN",
                                                  "AGE50_PROPTN",
                                                  "POP_DENSITY",
                                                  "GDP",
                                                  "INFANT_MORT",
                                                  "DOCS",
                                                  "VAX_RATE",
                                                  "HEALTHCARE_COST",
                                                  "DEATH_RATE"),
                                        selected = "POPULATION"
                                        
                            ), sliderInput("range","Whisker Range (IQR Mulitplier):",
                                           min=0.5,max=4,value=1.5,step=0.3
                                           
                            ),
                            selectInput("colour","Choose a colour:",
                                        choices = c("red","blue","green","purple","orange","skyblue"),
                                        selected = "skyblue"),  
                          ),
                          mainPanel(plotOutput(outputId="Boxplotsum"))           
                        ) 
                        
                        
               ),
               tabPanel("Barplot",
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectInput("var_bar","Choose a Categorical Variable:",
                                        choices=c("POLITICS",
                                                  "HEALTHCARE_BASIS",
                                                  "OBS_TYPE"
                                        )
                            ),
                            selectInput("colour","Choose a colour:",
                                        choices = c("red","blue","green","purple","orange","skyblue"),
                                        selected = "skyblue"),  
                          ),
                          mainPanel(plotOutput(outputId="Barplotsum"))           
                        ) 
               ),
               tabPanel("Correlationplot",
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("corr_method","Choose the type of Correlation:",
                                         choices=c("pearson",
                                                   "spearman",
                                                   "kendall"),
                                         selected="pearson",
                                         inline = TRUE
                            ),
                            selectInput("Method_cor","Choose a Method:",
                                        choices = c("circle","square","ellipse","number","shade","color","pie"),
                                        selected = "circle"),
                            selectInput("type_cor","Choose a Type:",
                                        choices = c("full","lower","upper"),
                                        selected = "lower"),
                          ),
                          mainPanel(plotOutput(outputId="Corrrplotsum"))           
                        )
               ) ,
               
               
               tabPanel("Display of Raw Data",
                        sidebarLayout(
                          sidebarPanel(width=2,
                                       sliderInput("obs_range", "Select Observation Range:",
                                                   min = 1, max = nrow(dat), value = c(1, 50), step = 1),
                                       selectInput("operator_filter", "Filter by Operator:",
                                                   choices = c("All", unique(as.character(dat$POLITICS))),
                                                   selected = "All")
                          ),
                          mainPanel(dataTableOutput(outputId="Rawdatasum"))           
                        )
               ) ,
             ),
    ),
    tabPanel("Missing Values",
             tabsetPanel(
               tabPanel("Missing Values Graph",
                        sidebarLayout(
                          sidebarPanel(width=2,
                                       radioButtons("missing_method","Choose the type of Missing Data Visualization:",
                                                    choices=c("gg_miss_upset",
                                                              "vis_dat",
                                                              "vis_miss"),
                                                    selected="gg_miss_upset"
                                                    
                                       ),
                          ),
                          mainPanel(plotOutput(outputId="missingdatasum"))           
                        )
               ) 
               
               ,tabPanel("Data Cleaning",
                         sidebarLayout(
                           sidebarPanel(width=2,
                                        sliderInput("var_thresold", "Select the Maximum Column Missing %:",
                                                    min = 0, max = 100, value = 75, step = 5),
                                        
                                        sliderInput("obs_thresold", "Select the Maximum Observation missing %:",
                                                    min = 0, max = 100, value = 75, step = 5),
                                        
                           ),
                           mainPanel(verbatimTextOutput(outputId="cleaned_data"),
                                     h4("Missing Data Visualization: Before Cleaning"),
                                     plotOutput(outputId="visdat_before_cleaning"),
                                     h4("Missing Data Visualization: After Cleaning"),
                                     plotOutput(outputId="visdat_after_cleaning")
                           )           
                         )
               ),
               tabPanel("Investigations",
                        sidebarLayout(
                          sidebarPanel(width=2,
                                       selectInput("cor_method", "Correlation Method:",
                                                   choices = c("pearson", "spearman", "kendall"), selected = "pearson"),
                                       selectInput("corr_order", "Ordering Method:",
                                                   choices = c("OLO", "None"), selected = "OLO"),
                          ),
                          mainPanel(
                            h4("Correlation Plot:"),
                            plotOutput(outputId="corr_missingval"),
                            h4("Investigate Missingness"),
                            plotOutput(outputId="informative_missng"),
                          )           
                        )
                        
                        
               ),
             )
    ),
    
    
    tabPanel("Imputation and Modelling",
             tabsetPanel(
               tabPanel("Imputations",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "imputeMethod", 
                              "Choose Imputation Method:", 
                              choices = c("knn", "mean", "median"),
                              selected = "knn"
                            ),
                            sliderInput(
                              "knnValue", 
                              "K for KNN Imputation:", 
                              min = 1, max = 10, value = 5
                            ),
                            checkboxInput(
                              "center", 
                              "Center numeric predictors", 
                              value = TRUE
                            ),
                            checkboxInput(
                              "scale", 
                              "Scale numeric predictors", 
                              value = TRUE
                            ),
                            actionButton(
                              "apply_imputation", 
                              "Apply Imputation"
                            )
                          ),
                          
                          mainPanel(
                            plotOutput("imputation_model"),
                            verbatimTextOutput("modelSummary")
                          )
                        )
               ),
               
               
               tabPanel("Imputation Plot",
                        fluidRow(
                          column(12,
                                 plotOutput("imputated_Plot"),
#                                 h4("Coefficients Table"),
#                                 verbatimTextOutput("imputated_coefficients")
                          )
                        )
               ),
               tabPanel("Coefficient Matrix",
                        fluidRow(
                          column(12,
                                 h4("Coefficients Table"),
                                 verbatimTextOutput("imputated_coefficients")
                          )
                        )
               ),
               
               tabPanel("Outlier Residual Plot",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("IQRmultiplier", 
                                        "IQR Multiplier for Outlier Detection:", 1, 5, 1.5),
                          ),
                          
                          mainPanel(
                            plotOutput("outlier_residual_Plot"),
                            DTOutput("outlier_Table"),
                          )
                        )
               ),

tabPanel("Outlier Table",
         fluidRow(
           column(12,
                  DTOutput("outlier_Table")
           )
         )
),

               
             ),
             
             
             
    ),
    
    
    
    
    )
    
)