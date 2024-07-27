library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(dplyr)
library(caret)
library(pROC) 
library(DT)
library(plotly)
library(markdown)#we are creating an app that will allow us to explore the data and build a model to predict if a player will be inducted into the Hall of Fame
ui <- fluidPage(
  titlePanel("Baseball Hall of Fame Predictor Using Logistic Regression"),
  
  tabsetPanel(
    tabPanel("How to Use",
             h2("How to Use"),
             includeMarkdown("documentation/how_to_use.Rmd")
    ),
    tabPanel("About",
             h2("About"),
             includeMarkdown("documentation/about.Rmd")
    ),
    tabPanel("Dataset",
             h2("Dataset"),
             sidebarLayout(
               sidebarPanel(
                 h4("Select the features to include in the model:"),
                 checkboxGroupInput(
                   "features",
                   "Features:",
                   c("TotalYears" = "TotalYears",
                     "TotalGames" = "TotalGames",
                     "TotalAtBats" = "TotalAtBats",
                     "TotalRuns" = "TotalRuns",
                     "TotalHits" = "TotalHits",
                     "TotalHomeRuns" = "TotalHomeRuns",
                     "TotalRunsBattedIn" = "TotalRunsBattedIn",
                     "TotalStolenBases" = "TotalStolenBases",
                     "TotalWalks" = "TotalWalks",
                     "TotalStrikeouts" = "TotalStrikeouts",
                     "TotalAwards" = "TotalAwards"),
                     selected = c("TotalYears")
                 ),
                sliderInput(
                  "year_filter",
                  "Filter by Year:",
                  min = min(hitting_data_agg$StartYear),
                  max = max(hitting_data_agg$StartYear),
                  value = c(min(hitting_data_agg$StartYear), max(hitting_data_agg$StartYear)),
                  step = 1
                ),
                sliderInput(
                  "games_played_filter",
                  "Filter by Games Played:",
                  min = min(hitting_data_agg$TotalGames),
                  max = max(hitting_data_agg$TotalGames),
                  value = c(min(hitting_data_agg$TotalGames), max(hitting_data_agg$TotalGames)),
                ),
                sliderInput(
                  "at_bats_filter",
                  "Filter by At Bats:",
                  min = min(hitting_data_agg$TotalAtBats),
                  max = max(hitting_data_agg$TotalAtBats),
                  value = c(min(hitting_data_agg$TotalAtBats), max(hitting_data_agg$TotalAtBats)),
                ),
                sliderInput(
                  "batting_average_filter",
                  "Filter by Batting Average:",
                  min = min(hitting_data_agg$BattingAverage),
                  max = max(hitting_data_agg$BattingAverage),
                  value = c(min(hitting_data_agg$BattingAverage), max(hitting_data_agg$BattingAverage)),
               )),
               mainPanel(
                  dataTableOutput("data_table")
               )
             )
    ),
    tabPanel("Explore Dataset",
         h2("Explore Dataset"),
         sidebarLayout(
           sidebarPanel(
             selectInput(
             "feature_select1",
             "Plot Feature 1:",
             choices = c(NULL)
             ),
             selectInput(
             "feature_select2",
             "Plot Feature 2:",
             choices = c(NULL)
             ),
             selectInput(
             "feature_select3",
             "Plot Feature 3:",
             choices = c(NULL)
             )
           ),
           mainPanel(
           plotOutput("feature_plot1"),
           plotOutput("percentage_plot1"),
           plotOutput("feature_plot2"),
           plotOutput("percentage_plot2"),
           plotOutput("feature_plot3"),
           plotOutput("percentage_plot3")
           )
         )
    ),
    tabPanel("Model",
             h2("Model"),
             sidebarLayout(
               sidebarPanel(
                p("The AUC (Area Under the Curve) of the ROC curve is a measure of how well the model can distinguish between the two classes. An AUC of 0.5 indicates that the model is no better than random guessing, while an AUC of 1 indicates perfect separation between the two classes."),
                 p("The coefficients in the model represent the impact of each feature on the prediction. A positive coefficient indicates that an increase in the feature value increases the likelihood of being inducted into the Hall of Fame, while a negative coefficient indicates the opposite. The magnitude of the coefficient represents the strength of the impact.")
               ),
               mainPanel(
                 plotOutput("model_plot"),
                 verbatimTextOutput("model_summary")
               )
             )
    ),
    tabPanel("Model Evaluation",
             h2("Model Evaluation"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                  "probality_plot_feature1",
                  "Plot Feature 1:",
                  choices = c(NULL)
                  ),
                  selectInput(
                  "probality_plot_feature2",
                  "Plot Feature 2:",
                  choices = c(NULL)
                  ),
                  selectInput(
                  "probality_plot_feature3",
                  "Plot Feature 3:",
                  choices = c(NULL)
                  )),
               mainPanel(
                  plotOutput("probality_plot1"),
                  plotOutput("probality_plot2"),
                  plotOutput("probality_plot3")
                  )
             )
    ),
    tabPanel("Prediction",
             h2("Prediction"),
             sidebarLayout(
               sidebarPanel(
                 h4("Enter the player's statistics to predict if they will be inducted into the Hall of Fame:"),
                 textOutput("displayed_batting_average"),
                 textOutput("displayed_on_base_percentage"),
                 textInput("player_name", "Player Name:", value = "Random Name"),
                 numericInput("TotalYears", "Total Years Played:", value = 10),
                 numericInput("TotalGames", "Total Games Played:", value = 1000),
                 numericInput("TotalAtBats", "Total At Bats:", value = 3000),
                 numericInput("TotalRuns", "Total Runs:", value = 500),
                 numericInput("TotalHits", "Total Hits:", value = 1000),
                 numericInput("TotalHomeRuns", "Total Home Runs:", value = 200),
                 numericInput("TotalRunsBattedIn", "Total Runs Batted In:", value = 500),
                 numericInput("TotalStolenBases", "Total Stolen Bases:", value = 100),
                 numericInput("TotalWalks", "Total Walks:", value = 300),
                 numericInput("TotalStrikeouts", "Total Strikeouts:", value = 500),
                 numericInput("TotalAwards", "Total Awards:", value = 5),
                 actionButton("predict_player", "Predict")
               ),
               mainPanel(
                  textOutput("prediction_text"),
                 imageOutput("prediction")
               )
             )
    )
  )
)