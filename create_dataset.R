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

create_dataset <- function() {
    # Reading and preparing the hitting data#
    hitting_data <- read.csv("baseball/Batting.csv")
    hof_data <- read.csv("baseball/HallOfFame.csv")
    awards_data <- read.csv("baseball/AwardsPlayers.csv")
    # Debug log for pathing
    print(getwd())

    # Aggregating data to get total statistics for each player
    hitting_data_agg <- hitting_data %>%
        group_by(playerID) %>%
        summarise(
            TotalYears = n_distinct(yearID),
            TotalGames = sum(G),
            TotalAtBats = sum(AB),
            TotalRuns = sum(R),
            TotalHits = sum(H),
            TotalHomeRuns = sum(HR),
            TotalRunsBattedIn = sum(RBI),
            TotalStolenBases = sum(SB),
            TotalWalks = sum(BB),
            TotalStrikeouts = sum(SO),
            StartYear = min(yearID),
            EndYear = max(yearID),
            TeamsPlayedFor = toString(unique(teamID))
        )

    awards_data <- awards_data %>%
        group_by(playerID) %>%
        summarise(
            TotalAwards = n_distinct(awardID)
        )

    # Add awards to hitting data if player is not in awards awards = 0
    hitting_data_agg <- hitting_data_agg %>%
        left_join(awards_data, by = "playerID") %>%
        mutate(
            TotalAwards = ifelse(is.na(TotalAwards), 0, TotalAwards)
        )

    # Checking for and removing players with incomplete stats
    hitting_data_agg <- hitting_data_agg %>%
        filter(
            !is.na(TotalYears) & !is.na(TotalGames) & !is.na(TotalAtBats) &
                !is.na(TotalRuns) & !is.na(TotalHits) & !is.na(TotalHomeRuns) &
                !is.na(TotalRunsBattedIn) & !is.na(TotalStolenBases) &
                !is.na(TotalWalks) & !is.na(TotalStrikeouts)
        )

    # Adding batting average and on-base percentage to the dataset
    hitting_data_agg <- hitting_data_agg %>%
        mutate(
            BattingAverage = round(TotalHits / TotalAtBats, 3),
            OnBasePercentage = round((TotalHits + TotalWalks) / TotalAtBats, 3)
        )
    
    # Filter data for Real averages
    # some of the averages calculated are not realistic and that is faulty data ex 3.00, max is 1.000
    hitting_data_agg <- hitting_data_agg %>%
        filter(BattingAverage <= 1 & OnBasePercentage <= 1)

    # Remove NA values
    hitting_data_agg <- na.omit(hitting_data_agg)

    # Reading and preparing the Hall of Fame data
    hof_data <- hof_data %>% filter(inducted == "Y" & category == "Player")

    # Adding a column to indicate if a player is in the Hall of Fame
    hitting_data_agg$HOF <- as.factor(ifelse(hitting_data_agg$playerID %in% hof_data$playerID, "Yes", "No"))

    return(hitting_data_agg)
}
