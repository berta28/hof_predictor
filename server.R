
library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(dplyr)
library(caret)
library(pROC) 
library(DT)
library(plotly)
library(markdown)

server <- function(input, output, session) {
  # Create a reactive dataset based on the selected features
  model_data <- reactive({
    hitting_data_agg %>%
      filter(StartYear >= input$year_filter[1] & StartYear <= input$year_filter[2]) %>%
      filter(TotalGames >= input$games_played_filter[1] & TotalGames <= input$games_played_filter[2]) %>%
      filter(TotalAtBats >= input$at_bats_filter[1] & TotalAtBats <= input$at_bats_filter[2]) %>%
      filter(BattingAverage >= input$batting_average_filter[1] & BattingAverage <= input$batting_average_filter[2]) %>%
      select(-BattingAverage,-OnBasePercentage, -StartYear,-EndYear, -TeamsPlayedFor)
  })

  displayed_data <- reactive({
    hitting_data_agg %>%
      filter(StartYear >= input$year_filter[1] & StartYear <= input$year_filter[2]) %>%
      filter(TotalGames >= input$games_played_filter[1] & TotalGames <= input$games_played_filter[2]) %>%
      filter(TotalAtBats >= input$at_bats_filter[1] & TotalAtBats <= input$at_bats_filter[2]) %>%
      filter(BattingAverage >= input$batting_average_filter[1] & BattingAverage <= input$batting_average_filter[2]) %>%
      select(playerID, BattingAverage, OnBasePercentage, input$features, StartYear,EndYear, TeamsPlayedFor, HOF)
  })

  selected_features <- reactive({
    input$features
    
  })
  # Update dropdowns in other tabs based on selected features
  observe({
    updateSelectInput(session, "feature_select1", choices = selected_features())
    updateSelectInput(session, "feature_select2", choices = selected_features())
    updateSelectInput(session, "feature_select3", choices = selected_features())
    updateSelectInput(session, "probality_plot_feature1", choices = selected_features())
    updateSelectInput(session, "probality_plot_feature2", choices = selected_features())
    updateSelectInput(session, "probality_plot_feature3", choices = selected_features())
  })
  evaluation_data <- reactiveVal(NULL)
  # Create a reactive model based on the selected features
  model <- reactiveVal(NULL)
  observe({
    displayed_batting_average <- round(input$TotalHits / input$TotalAtBats, 3)
    displayed_on_base_percentage <- round((input$TotalHits + input$TotalWalks) / input$TotalAtBats, 3)
    
    output$displayed_batting_average <- renderText({
      paste("Displayed Batting Average:", displayed_batting_average)
    })
    
    output$displayed_on_base_percentage <- renderText({
      paste("Displayed On-Base Percentage:", displayed_on_base_percentage)
    })
  })
  # Create a reactive evaluation based on the selected features
  evaluation <- reactiveVal(NULL)

  # Build the model based on the selected features
  observe({
    if (!is.null(input$features)) {
      # Define the control for cross-validation
      train_control <- trainControl(
        method = "cv",      # Cross-validation
        number = 10,         # Number of folds
        classProbs = TRUE,  # Needed for AUC
        summaryFunction = twoClassSummary # Use AUC as the metric
      )
      
      formula <- as.formula(paste("HOF ~", paste(input$features, collapse = " + ")))
      logistic_model_cv <- train(
         formula,
        data = model_data(),
        method = "glm",
        family = "binomial",
        trControl = train_control,
        metric = "ROC"
      )
      model(logistic_model_cv)
      #use model to create probability predictions
      evaluation <- model_data()
      evaluation$prob <- predict(logistic_model_cv, evaluation, type = "prob")$Yes
      evaluation_data(evaluation)
    }
  })

  # Plot the selected features
  output$feature_plot1 <- renderPlot({
    ggplot(model_data(), aes_string(x = input$feature_select1, fill = "HOF")) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Density Plot of", input$feature_select1))
  })

  output$feature_plot2 <- renderPlot({
    ggplot(model_data(), aes_string(x = input$feature_select2, fill = "HOF")) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Density Plot of", input$feature_select2))
  })

  output$feature_plot3 <- renderPlot({
    ggplot(model_data(), aes_string(x = input$feature_select3, fill = "HOF")) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Density Plot of", input$feature_select3))
  })


  # Plot the model
  output$model_plot <- renderPlot({
    if (!is.null(model())) {
      roc_curve <- roc(model_data()$HOF, predict(model(), model_data(), type = "prob")$Yes)
      plot(x = 1- roc_curve$specificities, y=roc_curve$sensitivities, col = "blue", main = paste("ROC Curve for Logistic Regression Model (AUC =", round(auc(roc_curve), 3), ")"), xlab = "False Positive Rate", ylab = "True Positive Rate")
    }
  })

  # Display the model summary
  output$model_summary <- renderPrint({
    if (!is.null(model())) {
      round(exp(coef(model()$finalModel)), 3)
    }
  })
  
  # Render the data table
  output$data_table <- renderDT({
    displayed_data()
  })

  output$probality_plot1 <- renderPlot({
    ggplot(evaluation_data(), aes_string(x = input$probality_plot_feature1, y = "prob")) +
      geom_point(aes(color = HOF)) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = paste("Predicted Probability of Being a Hall of Famer Based on", input$probality_plot_feature1),
           x = input$probality_plot_feature1, y = "Predicted Probability",
           color = "Hall of Fame Status")+
           scale_color_manual(values = c("No" = "red", "Yes" = "blue")) +
           ylim(0, 1)
  })

  output$probality_plot2 <- renderPlot({
    ggplot(evaluation_data(), aes_string(x = input$probality_plot_feature2, y = "prob")) +
      geom_point(aes(color = HOF)) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = paste("Predicted Probability of Being a Hall of Famer Based on", input$probality_plot_feature2),
           x = input$probality_plot_feature2, y = "Predicted Probability",
           color = "Hall of Fame Status")+
           scale_color_manual(values = c("No" = "red", "Yes" = "blue")) +
           ylim(0, 1)
  })

  output$probality_plot3 <- renderPlot({
    ggplot(evaluation_data(), aes_string(x = input$probality_plot_feature3, y = "prob")) +
      geom_point(aes(color = HOF)) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = paste("Predicted Probability of Being a Hall of Famer Based on", input$probality_plot_feature3),
           x = input$probality_plot_feature3, y = "Predicted Probability",
           color = "Hall of Fame Status")+
           scale_color_manual(values = c("No" = "red", "Yes" = "blue")) +
           ylim(0, 1)
  })

  # Predict if a player will be inducted into the Hall of Fame
  observeEvent(input$predict_player, {
    output$prediction <- renderImage({
      if (input$predict_player > 0) {
        new_data <- data.frame(
          TotalYears = input$TotalYears,
          TotalGames = input$TotalGames,
          TotalAtBats = input$TotalAtBats,
          TotalRuns = input$TotalRuns,
          TotalHits = input$TotalHits,
          TotalHomeRuns = input$TotalHomeRuns,
          TotalRunsBattedIn = input$TotalRunsBattedIn,
          TotalStolenBases = input$TotalStolenBases,
          TotalWalks = input$TotalWalks,
          TotalStrikeouts = input$TotalStrikeouts,
          TotalAwards = input$TotalAwards
        )
        # remove data that is not in selected features
        new_data <- new_data[, colnames(new_data) %in% selected_features()]
        
        output$prediction_text <- renderText({
          prob <- predict(model(), new_data, type = "prob")$Yes
          if (prob > 0.7) {
            paste("Player", input$player_name, "is likely to be inducted into the Hall of Fame with a probability of", round(prob, 3))
          } else {
            paste("Player", input$player_name, "is not likely to be inducted into the Hall of Fame with a probability of", round(prob, 3))
          }
        })
  
        prob <- predict(model(), new_data, type = "prob")$Yes
  
        if (prob > 0.7) {
          list(src = "images/likely_image.png", alt = "Likely to be inducted into the Hall of Fame", width = "700px", height = "700px")
        } else {
          list(src = "images/unlikely_image.png", alt = "Not likely to be inducted into the Hall of Fame", width = "700px", height = "700px")
        }
  
      }
    }, deleteFile = FALSE)
  })


  # Calculate the total number of players and Hall of Famers for each year
  output$percentage_plot1 <- renderPlot({

    # Find percentage of hall of famers for each cut group
    hall_of_famers <- model_data() %>%
      mutate(Group = cut(!!sym(input$feature_select1), breaks = 10)) %>%
      group_by(Group) %>%
      summarise(
      TotalPlayers = n(),
      TotalHallOfFamers = sum(HOF == "Yes"),
      percentage_hall_of_famers = TotalHallOfFamers / TotalPlayers
      )
      
    # Plot the comparison
    ggplot(hall_of_famers, aes(x = Group, y = percentage_hall_of_famers)) +
      geom_point() +
      geom_area(aes(fill = "Yes"), alpha = 1, group = 1, stat = "identity", color="blue", show.legend = TRUE) +
      geom_area(aes(fill = "No"), alpha = 0, group = 1, stat = "identity", color="red", show.legend = TRUE)+
      scale_color_manual(values = c("No" = "red", "Yes" = "blue"))+
      labs(title = paste("Percentage of Hall of Famers vs. Total Players by ", input$feature_select1),
       x = input$feature_select1,
       y = "Percentage of Hall of Famers",
       fill = "HOF") +
      geom_line(group = 1)+
      ylim(0, 1) +
      theme(panel.background= element_rect(fill = "red"))
  })
  output$percentage_plot2 <- renderPlot({

    # Find percentage of hall of famers for each cut group
    hall_of_famers <- model_data() %>%
      mutate(Group = cut(!!sym(input$feature_select2), breaks = 10)) %>%
      group_by(Group) %>%
      summarise(
      TotalPlayers = n(),
      TotalHallOfFamers = sum(HOF == "Yes"),
      percentage_hall_of_famers = TotalHallOfFamers / TotalPlayers
      )
      
    # Plot the comparison
    ggplot(hall_of_famers, aes(x = Group, y = percentage_hall_of_famers)) +
      geom_point() +
      geom_area(aes(fill = "Yes"), alpha = 1, group = 1, stat = "identity", color="blue", show.legend = TRUE) +
      geom_area(aes(fill = "No"), alpha = 0, group = 1, stat = "identity", color="red", show.legend = TRUE)+
      scale_color_manual(values = c("No" = "red", "Yes" = "blue"))+
      labs(title = paste("Percentage of Hall of Famers vs. Total Players by ", input$feature_select2),
       x = input$feature_select2,
       y = "Percentage of Hall of Famers",
       fill = "HOF") +
      geom_line(group = 1)+
      ylim(0, 1) +
      theme(panel.background= element_rect(fill = "red"))
  })

  output$percentage_plot3 <- renderPlot({

    # Find percentage of hall of famers for each cut group
    hall_of_famers <- model_data() %>%
      mutate(Group = cut(!!sym(input$feature_select3), breaks = 10)) %>%
      group_by(Group) %>%
      summarise(
      TotalPlayers = n(),
      TotalHallOfFamers = sum(HOF == "Yes"),
      percentage_hall_of_famers = TotalHallOfFamers / TotalPlayers
      )
      
    # Plot the comparison
    ggplot(hall_of_famers, aes(x = Group, y = percentage_hall_of_famers)) +
      geom_point() +
      geom_area(aes(fill = "Yes"), alpha = 1, group = 1, stat = "identity", color="blue", show.legend = TRUE) +
      geom_area(aes(fill = "No"), alpha = 0, group = 1, stat = "identity", color="red", show.legend = TRUE)+
      scale_color_manual(values = c("No" = "red", "Yes" = "blue"))+
      labs(title = paste("Percentage of Hall of Famers vs. Total Players by ", input$feature_select3),
       x = input$feature_select3,
       y = "Percentage of Hall of Famers",
       fill = "HOF") +
      geom_line(group = 1) +
      ylim(0, 1) +
      theme(panel.background= element_rect(fill = "red"))
  })  
}