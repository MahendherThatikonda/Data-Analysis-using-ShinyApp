#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Assignment-2 by Mahendher Thatikonda"),

    # Sidebar with a slider input for number of bins
    tabsetPanel(
      tabPanel("Summary Chart",
               h3("ASS1DATA"),
               tabsetPanel(
                 tabPanel("Summary",
                          verbatimTextOutput(outputId = "SummaryA1")
                 ),
                 tabPanel(
                   "Histogram",
                   fluidPage(
                      fluidRow(
                        column(6,
                       selectInput("var", "Choose a variable:",
                                   choices = c("POPULATION",
                                               "AGE25_PROPTN",
                                               "AGE_MEDIAN",
                                               "AGE50_PROPTN",
                                               "POP_DENSITY",
                                               "GDP",
                                               "INFANT_MORT",
                                               "DOCS",
                                               "VAX_RATE",
                                               "HEALTHCARE_COST",
                                               "DEATH_RATE"))),

                       column(6,                       sliderInput("bins", "Choose the Factor:",
                                   min = 5, max = 50, value = 5, step = 5),
                       
                      ),
                      
                      plotOutput(outputId = "histogramA1")) 
                   )
                 ),
                 tabPanel(
                   "Boxplot",
                   fluidPage(
                     fluidRow(
                       column(6,
                              selectInput("var", "Choose a variable:",
                                          choices = c("POPULATION",
                                                      "AGE25_PROPTN",
                                                      "AGE_MEDIAN",
                                                      "AGE50_PROPTN",
                                                      "POP_DENSITY",
                                                      "GDP",
                                                      "INFANT_MORT",
                                                      "DOCS",
                                                      "VAX_RATE",
                                                      "HEALTHCARE_COST",
                                                      "DEATH_RATE")),
                       ),
                       column(6,
                              sliderInput("range", "Whisker Range (IQR multiplier):", 
                                          min = 0.5, max = 4, value = 1.5, step = 0.3)
                       )
                     ),
                     plotOutput(outputId = "BoxplotA1")
                   )
                 ),
                 tabPanel(
                   "Barplot",
                   fluidPage(
                     
                     selectInput("varbar", "Choose a variable:",
                                 choices = c("POLITICS", "HEALTHCARE_BASIS","OBS_TYPE")),
                     
                     plotOutput(outputId = "BarplotA1")
                     
                   )
                 ),
                 tabPanel(
                   "Corrplot",
                   fluidPage(
                     radioButtons("cor_method", "Choose Correlation Method:",
                                  choices = c("Pearson" = "pearson",
                                              "Spearman" = "spearman",
                                              "Kendall" = "kendall"),
                                  selected = "pearson",
                                  inline = TRUE), 
                     
                     plotOutput(outputId = "corPlot")
                     
                   )
                 ),
                 tabPanel(
                   "Missing Values",
                   fluidPage(
                     
                     plotOutput(outputId = "missingdataA2")
                     
                   )
                 ),
                 tabPanel("Display of Raw Data",
                          
                            fluidPage("Visualisation",
                                     DT::dataTableOutput(outputId = "rawdataset")
                                     #                        verbatimTextOutput(outputId = "SummaryA2")
                            ),
                          
                 ),
               )
      ),
      
      tabPanel("Missing Data",
               h3("ASS1DATA"),
               tabsetPanel(
                 tabPanel("Visualization",
                          plotOutput(outputId = "missingdataA1"),
                     #     plotOutput(outputId = "missingdataA2"),
                          plotOutput(outputId = "missingdataA3")
                 ),
                 tabPanel(
                   "Data Cleaning",
                   fluidPage(
                     
                     plotOutput(outputId = "datacleaningA1"),

                     sliderInput("threshold",
                                 "Select Missing Value Threshold (%)",
                                 min = 0, max = 100, value = 50),
                     plotOutput(outputId = "CleanData"),
                     selectInput("threshold", "Choose a Thresold Value:",
                                 choices = c(50, 40, 30, 20, 10)),
                     verbatimTextOutput(outputId = "datacleaningA2"),
                     
                   )
                 ),
                 tabPanel(
                   "Investigations",
                   fluidPage(
                     
#                     selectInput("var", "Choose a variable:",
#                                 choices = c("POPULATION", "GDP", "DEATH_RATE", "VAX_RATE", "HEALTHCARE_COST")),
                     radioButtons("invest_corr_method", "Choose correlation method:",
                                 choices = c("Pearson" = "pearson",
                                             "Spearman" = "spearman",
                                             "Kendall" = "kendall"),
                                 selected = "pearson"),                    
                     plotOutput(outputId = "investigationsA1")
                     
                   )
                 ),
                 
                 tabPanel(
                   "Prediction",
                   fluidPage(
                     
                     plotOutput(outputId = "predictionA1")
                     
                   )
                 ),
tabPanel(
  "Informative Missingness",
  fluidPage(
    
    plotOutput(outputId = "infomissingnessA1")
    
  )
),
#tabPanel(
#  "Imputation",
#  fluidPage(
    
#    plotOutput(outputId = "imputationA1"),
#    verbatimTextOutput(outputId = "imputation_metrics")
    
#  )
#),

tabPanel(
  "Imputation",
  fluidPage(
    plotOutput(outputId = "imputationA1"),
   # verbatimTextOutput(outputId = "imputation_metrics"),
    plotOutput(outputId = "residualPlot")  # ✅ Add this
  )
),

tabPanel(
  "Imputed Values",
  fluidPage(
    h4("Imputed Dataset (KNN, train data only)"),
    DT::dataTableOutput("imputed_data_table")
  )
),
tabPanel(
  "Imputated Values",
  fluidPage(
    DT::dataTableOutput(outputId = "imputed_data_table"),
    br(),
    downloadButton("downloadImputed", "Download Imputed CSV")
  )
),


               )
      ),
tabPanel("Outliers",
         h3("ASS1DATA"),
         tabsetPanel(
           tabPanel("Visualization",
#                    plotOutput(outputId = "outliersA1"),
#                    plotOutput(outputId = "missingdataA2"),
#                    plotOutput(outputId = "missingdataA3"),
                    
                    fluidPage(
                      
                      fluidRow(
                        column(6,
                               selectInput("var", "Choose a variable:",
                                           choices = c("POPULATION", "GDP", "DEATH_RATE", "VAX_RATE", "HEALTHCARE_COST"))
                        ),
                        column(6,
                               sliderInput("bin", "Choose number of bins:",
                                           min = 5, max = 100, value = 30, step = 5)
                        )
                      ),
                      plotOutput(outputId = "outliersA1")
                      ,
                      fluidRow(
                        column(6,
                               selectInput("var", "Choose a variable:",
                                           choices = c("POPULATION", "GDP", "DEATH_RATE", "VAX_RATE", "HEALTHCARE_COST"))
                        ),
                        column(6,
                               sliderInput("bin", "Choose number of bins:",
                                           min = 1, max = 10, value = 5, step = 1)
                        )
                      ),
                      plotOutput(outputId = "outliersA2"),
                      fluidRow(
                        column(6,
                               selectInput("var1", "Choose a variable:",
                                           choices = c("POPULATION", "GDP", "DEATH_RATE", "VAX_RATE", "HEALTHCARE_COST"))
                        ),
                        column(6,
                               selectInput("var2", "Choose a Second variable:",
                                           choices = c("POPULATION", "GDP", "DEATH_RATE", "VAX_RATE", "HEALTHCARE_COST"))
                        ),
                        column(6,
                               sliderInput("Factor", "Choose the Factor:",
                                           min = 1, max = 10, value = 5, step = 1)
                        )
                      ),
                      plotOutput(outputId = "outliersA3")
                      
#                      selectInput("var", "Choose a variable:",
 #                                 choices = c("POPULATION", "GDP", "DEATH_RATE", "VAX_RATE", "HEALTHCARE_COST")),
#                      sliderInput("bin", "Choose number of bins:", min = 5, max = 100, value = 30, step = 5),                 
#                      plotOutput(outputId = "outliersA1")
                      
                    )
           ),
           tabPanel(
             "Transform",
             fluidPage(
               
#               plotOutput(outputId = "boxcoxA1"),
#               selectInput("threshold", "Choose a Thresold Value:",
#                           choices = c(50, 40, 30, 20, 10)),
               verbatimTextOutput(outputId = "boxcoxA1"),
               plotOutput(outputId = "yeojohnsonA1")
             )
           ),
#           tabPanel(
 #            "Investigations",
#             fluidPage(
#               
#               #                     selectInput("var", "Choose a variable:",
#               #                                 choices = c("POPULATION", "GDP", "DEATH_RATE", "VAX_RATE", "HEALTHCARE_COST")),
#               
#               plotOutput(outputId = "investigationsA1")
#               
#             )
#           ),
           
#           tabPanel(
#             "Prediction",
#             fluidPage(
               
#               plotOutput(outputId = "predictionA1")
               
#             )
#           ),
#           tabPanel(
#             "Informative Missingness",
#             fluidPage(
               
#               plotOutput(outputId = "infomissingnessA1")
               
#             )
#           ),
           
           
           
           
         )
)
    )
)
