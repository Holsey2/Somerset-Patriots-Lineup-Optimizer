#Somerset Patriots Lineup Optimizer & Hitting by Pitch Type Tables (full file)
#Author: Holsey, Patrick

#The lineup optimizer provides the most productive lineup in terms of runs scored projections (calculated)
#from OBP and SLG % vs left- and right-handed starting pitchers and pitch type. A shiny app has
#been created to allow the user to move a player in the lineup to determine a new run probability.
#The second app displays hitter performance tables (OBP and SLG) vs left- and right-handed pitchers and 
#their graded pitch types Fastball, Curveball, Changeup, Slider, Cutter, Splitter by Baseball-Savant and 
#Fangraphs.

#The dataset acquired is from MiLB.com, using the first 56 games of the 2025 Patriots season

#Load packages
library(tidyverse)
library(tidyr)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(reactable)
library(sortable)
library(DT)
library(purrr)
library(stringr)
library(rsconnect)

#Load datasets
Positions <- read_excel("Positions.xlsx")
SP_data <- read_excel("Pitch types by date.xlsx")
Lineup_perf <- read_excel("Lineup perf by date.xlsx")

#Calculate total number of games in SP_data by counting and summing dates
games_by_date <- SP_data %>%
  count(Date)

doubleheader_dates <- SP_data %>%
  count(Date) %>%
  filter(n == 2) %>%
  pull(Date)

total_games <- n_distinct(SP_data$Date) + length(doubleheader_dates)
total_games

#Calculate games pitched by SP_Hand = R and SP_Hand = L
games_by_hand <- SP_data %>%
  distinct(Date, SP_Name, SP_Hand) %>%
  group_by(SP_Hand) %>%
  summarise(Games_Pitched = n())
games_by_hand

#Calculate total runs scored vs SP_Hand = R and SP_Hand = L
earned_runs_summary <- SP_data %>%
  group_by(SP_Hand) %>%
  summarize(total_earned_runs = sum(Earned_Runs, na.rm = TRUE))
earned_runs_summary

#Calculate total plate_appearances from SP_data
PA_avg <- mean(SP_data[["Plate_Appearances"]], na.rm = TRUE)
PA_avg

#Calculate total innings pitched by starting pitchers, grouped by SP_Hand = L and R
sum_innings_pitched <- function(innings_vector) {
  total_outs <- 0
  for (ip in innings_vector) {
    full_innings <- floor(ip)
    decimal_part <- round((ip - full_innings) * 10)
    
    # Convert decimal part (0.1 or 0.2) into outs
    outs <- switch(as.character(decimal_part),
                   "0" = 0,
                   "1" = 1,
                   "2" = 2,
                   0)  # default fallback
    
    total_outs <- total_outs + (full_innings * 3) + outs
  }
  
  # Convert total outs back into innings
  total_innings <- floor(total_outs / 3) + (total_outs %% 3) / 10
  return(total_innings)
}
total_IP <- sum_innings_pitched(SP_data$Innings_Pitched)
total_IP

#Apply innings pitched function by SP_Hand = L and R
innings_by_hand <- SP_data %>%
  group_split(SP_Hand) %>%
  map_dfr(~{
    tibble(
      SP_Hand = unique(.x$SP_Hand),
      total_IP = sum_innings_pitched(.x$Innings_Pitched)
    )
  })
innings_by_hand

#Add position to lineup_perf dataset
Lineup_perf <- left_join(Lineup_perf, Positions, by = "PlayerName")

#Assigning hitter stats as events
event_cols <- c("Out_no_SF", "Single", "Double", "Triple", "Homerun", 
                "Walk", "Hit_by_Pitch", "Sacrifice_Fly")

#Assigning events as numeric
Lineup_perf_clean <- Lineup_perf %>%
  mutate(across(all_of(event_cols), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(event_cols), ~ replace_na(., 0)))

#Calculating Plate_Appearances and At_Bats by PlayerName, SP_Hand, and Batting Order
PA_stats <- Lineup_perf_clean %>%
  group_by(PlayerName, SP_Hand, Order) %>%
  summarise(
    At_Bat = sum(Out_no_SF + Single + Double + Triple + Homerun, na.rm = TRUE),
    Plate_Appearance = sum(Out_no_SF + Walk + Hit_by_Pitch + Sacrifice_Fly + 
                             Single + Double + Triple + Homerun, na.rm = TRUE),
    .groups = "drop"
  )
PA_stats

#Calculate individual averages for OBP and SLG grouped by SP_Hand and Order, round to 3 decimals
Avg_OBPSLG <- Lineup_perf_clean %>%
  group_by(PlayerName, SP_Hand, Order) %>%
  summarise(
    Singles = sum(Single, na.rm = TRUE),
    Doubles = sum(Double, na.rm = TRUE),
    Triples = sum(Triple, na.rm = TRUE),
    Homeruns = sum(Homerun, na.rm = TRUE),
    Walks = sum(Walk, na.rm = TRUE),
    HBP = sum(Hit_by_Pitch, na.rm = TRUE),
    PA = sum(Out_no_SF + Walk + Hit_by_Pitch + Sacrifice_Fly + Single + Double + Triple + Homerun, na.rm = TRUE),
    AB = sum(Out_no_SF + Single + Double + Triple + Homerun, na.rm = TRUE),
    Hits = Singles + Doubles + Triples + Homeruns,
    OBP = ifelse(PA > 0, round((Hits + Walks + HBP) / PA, 3), 0),
    Total_Bases = Singles + (2 * Doubles) + (3 * Triples) + (4 * Homeruns),
    SLG = ifelse(AB > 0, round(Total_Bases / AB, 3), 0),
    .groups = "drop"
  ) %>%
  select(PlayerName, SP_Hand, Order, Avg_OBP = OBP, Avg_SLG = SLG, AB, PA)

#Add position to Avg_OBPSLG dataset
Avg_OBPSLG <- left_join(Avg_OBPSLG, Positions, by = "PlayerName")

#Create Projected Runs by calculating Avg_OBP * Avg_SLG * 2 (avg PA/player vs SP)
Avg_OBPSLG <- Avg_OBPSLG %>%
  mutate(Run_Expectancy = round(Avg_OBP * Avg_SLG * 2, 2))

#Calculate team_OBP, team_SLG grouped by SP_Hand
team_OBPSLG_by_hand <- Lineup_perf_clean %>%
  mutate(
    Hits = Single + Double + Triple + Homerun,
    At_Bat = Out_no_SF + Single + Double + Triple + Homerun,
    Plate_Appearance = At_Bat + Walk + Hit_by_Pitch + Sacrifice_Fly,
    Total_Bases = Single + (2 * Double) + (3 * Triple) + (4 * Homerun)
  ) %>%
  group_by(SP_Hand) %>%
  summarise(
    Total_Hits = sum(Hits),
    Total_BB = sum(Walk),
    Total_HBP = sum(Hit_by_Pitch),
    Total_PA = sum(Plate_Appearance),
    Total_TB = sum(Total_Bases),
    Total_AB = sum(At_Bat),
    Team_OBP = round((Total_Hits + Total_BB + Total_HBP) / Total_PA, 3),
    Team_SLG = round(Total_TB / Total_AB, 3),
    .groups = "drop"
  )

#Add actual runs per game (earned_runs_summary / games_by_hand) to team_OBPSLG_by_Hand
team_OBPSLG_by_hand <- team_OBPSLG_by_hand %>%
  left_join(earned_runs_summary, by = "SP_Hand") %>%
  left_join(games_by_hand, by = "SP_Hand") %>%
  mutate(
    runs_per_game = round(total_earned_runs / Games_Pitched, 2)
  )

#Add Run_Expectancy to team_OBPSLG_by_hand calculated as Team_OBP * Team_SLG * 24 (base out event states)
team_OBPSLG_by_hand <- team_OBPSLG_by_hand %>%
  mutate(Run_Expectancy = round(Team_OBP * Team_SLG * 18, 2))

#Build optimal lineups
build_optimal_lineup <- function(hand) {
  min_pa <- ifelse(hand == "L", 6, 8)
  excluded_players <- c("Jazz Chisholm Jr", "DJ LeMahieu")  # Exclude here
  
  lineup <- data.frame()
  used_players <- character()
  
  for (ord in 1:9) {
    best_player <- Avg_OBPSLG %>%
      filter(SP_Hand == hand,
             PA >= min_pa,
             Order == ord,
             !(PlayerName %in% c(used_players, excluded_players))) %>%
      arrange(desc(Run_Expectancy)) %>%
      slice(1)
    
    if (nrow(best_player) == 1) {
      lineup <- bind_rows(lineup, best_player)
      used_players <- c(used_players, best_player$PlayerName)
    } else {
      fallback <- Avg_OBPSLG %>%
        filter(SP_Hand == hand,
               PA >= min_pa,
               !(PlayerName %in% c(used_players, excluded_players))) %>%
        arrange(desc(Run_Expectancy)) %>%
        distinct(PlayerName, .keep_all = TRUE) %>%
        slice(1)
      
      if (nrow(fallback) == 1) {
        fallback$Order <- ord
        lineup <- bind_rows(lineup, fallback)
        used_players <- c(used_players, fallback$PlayerName)
      }
    }
  }
  
  lineup <- lineup %>%
    arrange(Order) %>%
    mutate(
      Order = as.integer(Order),
      PA = round(as.numeric(PA)),
      Avg_OBP = sprintf("%.3f", as.numeric(Avg_OBP)),
      Avg_SLG = sprintf("%.3f", as.numeric(Avg_SLG))
    )
  
  return(lineup)
}

# Optimal lineups
optimal_lineup_L <- build_optimal_lineup("L")
optimal_lineup_R <- build_optimal_lineup("R")

get_lineup_by_hand <- function(hand) {
  if (hand == "L") return(optimal_lineup_L)
  else return(optimal_lineup_R)
}

#Create UI for Optimal Lineup
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    body, html {
      height: 100%;
      margin: 0;
    }

    /* Background image */
    body {
      background-image: url('https://img.mlbstatic.com/mlb-images/image/upload/t_16x9/t_w1024/mlb/jkxurdo008t6l5jwtmkd');
      background-size: cover;
      background-repeat: no-repeat;
      background-attachment: fixed;
      position: relative;
      z-index: 0;
    }

    /* Overlay on top of background image */
    body::before {
      content: '';
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background-color: rgba(200, 200, 200, 0.6);  
      z-index: -1;
    }

    /* Output panels with partial opacity */
    .panel {
      background-color: rgba(255, 255, 255, 0.01);
      border: 1px solid #ccc !important;
      padding: 15px;
      border-radius: 5px;
      color: black;
    }

    /* Run Expectancy styles */
    .run-optimal {
      color: green;
      font-weight: bold;
      font-size: 18px;
    }

    .run-suboptimal {
      color: red;
      font-weight: bold;
      font-size: 18px;
    }

    /* Table content */
    table {
      background-color: white;
    }
  "))
  ),
  
  titlePanel("Somerset Patriots Optimal Lineup"),
  
  sidebarLayout(
    sidebarPanel(
      class = "panel",
      selectInput("sp_hand", "Pitcher Handedness", choices = c("L", "R")),
      selectInput("replace_order", "Select Batting Order Spot to Replace (1-9):", choices = 1:9),
      uiOutput("bench_ui"),
      actionButton("update_lineup", "Update Lineup")
    ),
    mainPanel(
      class = "panel",
      h3("Batting Order"),
      uiOutput("run_expectancy_ui"),
      tableOutput("batting_order"),
      h4("Available Bench Players"),
      tableOutput("bench_players")
    )
  )
)

#Create Server for Optimal Lineup
server <- function(input, output, session) {
  
  get_lineup_by_hand <- function(hand) {
    if (hand == "L") return(optimal_lineup_L)
    else return(optimal_lineup_R)
  }
  
  get_bench_players <- function(current_lineup, sp_hand) {
    excluded_players <- c("Jazz Chisholm Jr", "DJ LeMahieu")
    
    position_order <- c("C", "1B", "2B", "3B", "SS", "OF")
    
    all_players <- Avg_OBPSLG %>%
      filter(
        SP_Hand == sp_hand,
        !(PlayerName %in% current_lineup$PlayerName),
        !(PlayerName %in% excluded_players),
        !is.na(PlayerName)
      ) %>%
      distinct(PlayerName, .keep_all = TRUE) %>%
      select(PlayerName, Position) %>%
      mutate(
        PrimaryPosition = sub(",.*", "", Position),  # Take the first position if multiple
        PositionRank = match(PrimaryPosition, position_order)
      ) %>%
      arrange(PositionRank, PlayerName) %>%
      select(PlayerName, Position)
    
    return(all_players)
  }
  
  current_lineup <- reactiveVal()
  
  observe({
    current_lineup(get_lineup_by_hand(input$sp_hand))
  })
  
  observeEvent(input$sp_hand, {
    current_lineup(get_lineup_by_hand(input$sp_hand))
  })
  
  output$bench_ui <- renderUI({
    bench <- get_bench_players(current_lineup(), input$sp_hand)
    selectInput("bench_player", "Select Bench Player to Add:", choices = bench$PlayerName)
  })
  
  observeEvent(input$update_lineup, {
    lineup <- current_lineup()
    bench <- get_bench_players(lineup, input$sp_hand)
    selected_bench <- bench %>% filter(PlayerName == input$bench_player)
    
    if (nrow(selected_bench) == 1) {
      order_num <- as.integer(input$replace_order)
      
      replacement <- Avg_OBPSLG %>%
        filter(PlayerName == selected_bench$PlayerName,
               SP_Hand == input$sp_hand,
               Order == order_num)
      
      if (nrow(replacement) == 0) {
        replacement <- selected_bench
        replacement$Order <- order_num
        replacement$Avg_OBP <- "0.000"
        replacement$Avg_SLG <- "0.000"
        replacement$PA <- 0
        replacement$Run_Expectancy <- 0
      }
      
      replacement <- replacement %>%
        mutate(
          Avg_OBP = sprintf("%.3f", as.numeric(Avg_OBP)),
          Avg_SLG = sprintf("%.3f", as.numeric(Avg_SLG)),
          PA = round(as.numeric(PA)),
          Order = as.integer(Order)
        )
      
      updated_lineup <- lineup %>%
        filter(Order != order_num) %>%
        bind_rows(replacement) %>%
        arrange(Order)
      
      current_lineup(updated_lineup)
    }
  })
  
  output$batting_order <- renderTable({
    current_lineup() %>%
      mutate(PA = as.integer(PA)) %>%
      select(Order, PlayerName, Avg_OBP, Avg_SLG, PA, Position)
  })
  
  output$bench_players <- renderTable({
    get_bench_players(current_lineup(), input$sp_hand)[, c("PlayerName", "Position")]
  })
  
  output$run_expectancy_ui <- renderUI({
    optimal <- get_lineup_by_hand(input$sp_hand)
    is_optimal <- identical(current_lineup(), optimal)
    expectancy <- sum(as.numeric(current_lineup()$Run_Expectancy), na.rm = TRUE)
    
    class_name <- if (is_optimal) "run-optimal" else "run-suboptimal"
    
    tags$div(class = class_name, 
             tags$strong(paste("Projected Runs Scored:", round(expectancy, 2))))
  })
}

#Run app
shinyApp(ui, server)

#Long format of SP_data: one row per SP_Hand + pitch type + grade
pitch_grades_long <- SP_data %>%
  pivot_longer(cols = c(Fastball, Curveball, Slider, Changeup, Cutter, Splitter),
               names_to = "Pitch_Type", values_to = "Grade") %>%
  filter(!is.na(Grade))

#Join lineup_perf_clean with pitch_grades_long, filter out NA grades, DJ LeMahieu, and Jazz Chisholm Jr
Lineup_with_pitchtype <- Lineup_perf_clean %>%
  left_join(pitch_grades_long, by = c("SP_Name", "SP_Hand"), relationship = "many-to-many") %>%
  filter(!is.na(Grade)) %>%
  filter(!PlayerName %in% c("DJ LeMahieu", "Jazz Chisholm Jr"))

# Convert event columns to numeric
event_cols <- c("Single", "Double", "Triple", "Homerun", "Walk", "Hit_by_Pitch", "Out_no_SF", "Sacrifice_Fly")
Lineup_with_pitchtype[event_cols] <- Lineup_with_pitchtype[event_cols] %>%
  lapply(function(x) as.numeric(replace_na(x, 0)))

#Calculate OBP and SLG on Lineup_with_pitchtype
player_pitch_summary <- Lineup_with_pitchtype %>%
  mutate(
    Hits = Single + Double + Triple + Homerun,
    At_Bat = Out_no_SF + Hits,
    Plate_Appearance = At_Bat + Walk + Hit_by_Pitch + Sacrifice_Fly,
    Total_Bases = Single + 2 * Double + 3 * Triple + 4 * Homerun
  ) %>%
  group_by(PlayerName, SP_Hand, Pitch_Type, Grade) %>%
  summarise(
    PA = sum(Plate_Appearance, na.rm = TRUE),
    Avg_OBP = round(sum(Hits + Walk + Hit_by_Pitch, na.rm = TRUE) / sum(Plate_Appearance, na.rm = TRUE), 3),
    Avg_SLG = round(sum(Total_Bases, na.rm = TRUE) / sum(At_Bat, na.rm = TRUE), 3),
    .groups = "drop"
  )

#Create UI for Pitch Type tables
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://patriots.milbstore.com/cdn/shop/products/WhiteLogoBall_1200x.jpg?v=1627489370');
        background-size: cover;
        background-repeat: no-repeat;
        background-attachment: fixed;
      }
      .well {
        background-color: rgba(200,200,200,0.6);
      }
      .dataTables_wrapper {
        background-color: rgba(255,255,255,0.96);
        padding: 10px;
        border-radius: 5px;
        font-size: 13px;
      }
    "))
  ),
  titlePanel("Somerset Patriots Batting by Pitch Type and Scout Grade"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("sp_hand", "Filter by SP Handedness", choices = c("L", "R"), selected = "L"),
      selectInput("pitch_type", "Filter by Pitch Type", choices = sort(unique(player_pitch_summary$Pitch_Type)), selected = "Fastball"),
      selectInput("grade", "Filter by Pitch Grade", choices = c("All", sort(unique(player_pitch_summary$Grade))))
    ),
    mainPanel(
      width = 7,
      DTOutput("summary_table")
    )
  )
)

#Create server for Pitch Type tables
server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- player_pitch_summary
    if (input$sp_hand != "All") {
      data <- data %>% filter(SP_Hand == input$sp_hand)
    }
    if (input$pitch_type != "All") {
      data <- data %>% filter(Pitch_Type == input$pitch_type)
    }
    if (input$grade != "All") {
      data <- data %>% filter(Grade == as.numeric(input$grade))
    }
    data
  })
  
  output$summary_table <- renderDT({
    datatable(
      filtered_data() %>%
        mutate(
          Avg_OBP = formatC(Avg_OBP, format = "f", digits = 3),
          Avg_SLG = formatC(Avg_SLG, format = "f", digits = 3)
        ) %>%
        select(PlayerName, SP_Hand, Pitch_Type, Grade, PA, Avg_OBP, Avg_SLG),
      rownames = FALSE,
      options = list(pageLength = 25)
    )
  })
}

#Run app
shinyApp(ui, server)