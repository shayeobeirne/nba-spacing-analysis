# *****************************************************************************************************
#
# title: NBA Playoff Spacing Analysis (2021-22)
# author: Shaye O'Beirne
# owner: Shaye O'Beirne
# date: 07-30-2023
# description: This project seeks to analyze court spacing among NBA players during a single possession.
# The user has the ability to select a team within the app, and a possession by that team during the 
# specified game to view various insights related to spacing during that possession.
#
# *****************************************************************************************************
# ------------------------------------------------------------------------------------------------------

# Loading Packages
library(shiny)
library(dplyr)
library(scales)
library(DT)
library(reactable)
library(fivethirtyeight)
library(reactablefmtr)
library(reticulate)
library(bslib)
library(extrafont)
library(shinydashboard)
library(data.table)
library(htmlwidgets)
library(plotly)

# ------------------------------------------------------------------------------------------------------

# all_games <- c('ECF Game 1', 'ECF Game 2', 'ECF Game 3', 'ECF Game 4', 'ECF Game 5', 'ECF Game 6', 'ECF Game 7',
#                'WCF Game 1', 'WCF Game 2', 'WCF Game 3', 'WCF Game 4', 'WCF Game 5',
#                'Finals Game 1', 'Finals Game 2', 'Finals Game 3', 'Finals Game 4', 'Finals Game 5', 'Finals Game 6')
# 
# all_teams <- c('BOS', 'MIA', 'DAL', 'GSW')

all_games <- c('ECF Game 1')

all_teams <- c('BOS', 'MIA')

stats_2022 <- read.csv('data/nba_player_stats_2022.csv')
stats_2022 <- stats_2022 %>%
  mutate(`HEADSHOT` = ifelse(!is.na(PLAYER_ID), paste0('<img src="https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/', PLAYER_ID, '.png"  height="52"></img>'), 'Image NA'))

games <- read.csv('data/games.csv')
teams <- read.csv('data/teams.csv')

# ------------------------------------------------------------------------------------------------------

# Define UI for application
ui <- fluidPage(
  theme = bs_theme(bg = '#202123', fg = '#B8BCC2', primary = '#EA80FC', base_font = 'Andale Mono'),
  titlePanel("NBA Playoff Spacing Analysis (2021-22)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      textOutput("home_title"),
      dataTableOutput("home_table"),
      div(style = "height:50px"),
      textOutput("away_title"),
      dataTableOutput("away_table")
    ),
    mainPanel(
      fluidRow(
        column(
          selectInput("team_choice", label = "Choose Team:", choices = all_teams, selected = all_teams[[1]], width = "100%"),
          width = 4),
        column(
          width = 4
        ),
        column(
          selectInput("possession_choice", label = "Choose Possession:", choices = c()),
          width = 4
        )
      ),
      fluidRow(
        column(
          div("Offensive Spacing"),
          textOutput('off_spacing'),
          width = 4
        ),
        column(
          div("Defensive Spacing"),
          textOutput('def_spacing'),
          width = 4
        ),
        column(
          div("Shot Outcome"),
          textOutput('shot_outcome'),
          width = 4
        )
      ),
      fluidRow(
        tabsetPanel(
          tabPanel("FGA Spacing",
                   fluidRow(
                     column(
                       imageOutput("animation_img"),
                       div(style = "height:110px"),
                       width = 12)
                   )
          ),
          tabPanel("Transition in Spacing",
                   fluidRow(
                     column(
                       imageOutput("animation_gif"),
                       div(style = "height:110px"),
                       width = 12)
                   )
          ),
          tabPanel("Broadcast Video",
                   fluidRow(
                     column(
                       uiOutput("video"),
                       width = 12)
                   )
          )
        )
      ),
      fluidRow(
        column(
          textOutput("shot_chart_title"),
          plotlyOutput("shotChart"),
          width = 6
        ),
        column(
          div("Game Progression", style = "color: #B8BCC2 ; font-size: 24px; font-style: bold"),
          plotlyOutput("gametracker"),
          width = 6
        )
      )
    )
  ),

  tags$head(tags$style("#off_spacing{color: #EA80FC; font-size: 30px; font-style: bold}", 
                       "#def_spacing{color: #EA80FC; font-size: 30px; font-style: bold}",
                       "#shot_outcome{color: #EA80FC; font-size: 30px; font-style: bold}" ,
                       "#home_title{color: #98002E; font-size: 24px; font-tyle: bold}",
                       "#away_title{color: #007A33; font-size: 24px; font-tyle: bold}",
                       "#shot_chart_title{color: #B8BCC2; font-size: 24px; font-tyle: bold}"
    )
  )
)

# ------------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Saving input choices
  game <- reactive(input$game_choice)
  team <- reactive(input$team_choice)
  possession <- reactive(input$possession_choice)
  
  # Reading in data from CSV
  file_path <-'app_data/ECF_Game_1.csv'
  data <- fread(file_path)
  teams <- unique(data$Team)[2:3]
  
  # Updating Team Choices
  observe({
    updateSelectInput(
      session = session,
      inputId = "team_choice",
      choices = teams
    )
  })

  # Possession choices
  observe({
    possessions <- unique(
      data %>%
        filter(wasShot == 1) %>%
        filter(grepl(team(), POSSESSION_ID)) %>%
        select(POSSESSION_ID)
    )
    
    updateSelectInput(
      session = session,
      inputId = "possession_choice",
      choices = possessions
    )
  })
  
  # Away lineup table
  output$away_table <- renderDataTable({
    req(possession)
    away_poss <- data[which(data$POSSESSION_ID == possession()),]
    away_lineup <- unique(c(away_poss$X0_name_h, away_poss$X1_name_h, away_poss$X2_name_h, away_poss$X3_name_h, away_poss$X4_name_h))
    jerseys <- unique(c(away_poss$X0_jersey_h, away_poss$X1_jersey_h, away_poss$X2_jersey_h, away_poss$X3_jersey_h, away_poss$X4_jersey_h))
    jersey_df <- data.frame(away_lineup, jerseys)
    colnames(jersey_df) <- c('Player', 'Jersey')
    
    table_df <- stats_2022 %>% 
      filter(Player %in% away_lineup) %>% 
      mutate(`FG%` = paste0(round((FG/FGA*100),1), '%')) %>% 
      left_join(jersey_df, by = c('Player' = 'Player')) %>%
      select(Jersey, HEADSHOT, Player, `FG%`, Pos) %>%
      rename(c('#' = 'Jersey'))
    
    away_table <- datatable(table_df,
                            escape = FALSE, 
                            options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), 
                            rownames = FALSE)
    away_table
  })
  
  # Away Table Title
  
  output$away_title <- renderText({
    away_team <- unique(data$AWAY_TEAM)
    
    away_title <- paste0(away_team, ' Lineup')
    away_title
  })
  
  # Home lineup table
  output$home_table <- renderDataTable({
    req(possession)
    home_poss <- data[which(data$POSSESSION_ID == possession()),]
    home_lineup <- unique(c(home_poss$X0_name_a, home_poss$X1_name_a, home_poss$X2_name_a, home_poss$X3_name_a, home_poss$X4_name_a))
    jerseys <- unique(c(home_poss$X0_jersey_a, home_poss$X1_jersey_a, home_poss$X2_jersey_a, home_poss$X3_jersey_a, home_poss$X4_jersey_a))
    jersey_df <- data.frame(home_lineup, jerseys)
    colnames(jersey_df) <- c('Player', 'Jersey')
    
    table_df <- stats_2022 %>% 
      filter(Player %in% home_lineup) %>% 
      mutate(`FG%` = paste0(round((FG/FGA*100),1), '%')) %>% 
      left_join(jersey_df, by = c('Player' = 'Player')) %>%
      select(Jersey, HEADSHOT, Player, `FG%`, Pos) %>%
      rename(c('#' = 'Jersey'))
    
    home_table <- datatable(table_df,
                            escape = FALSE, 
                            options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), 
                            rownames = FALSE)
    home_table
  })
  
  # Home Table Title
  
  output$home_title <- renderText({
    home_team <- unique(data$HOME_TEAM)
    
    home_title <- paste0(home_team, ' Lineup')
    home_title
  })
  
  # Shot Chart
  output$shotChart <- renderPlotly({
    req(data)
    
    js <- "
          function(el, x) {
            el.on('plotly_click', function(d) {
              var point = d.points[0];
              var url = point.data.customdata[point.pointIndex];
              window.open(url);
            });
          }"
    
    shot_data <- data %>% filter(wasShot == 1)
    par(mar = c(4,6,4,4))
    shot_data[1:max(which(shot_data$POSSESSION_ID == possession())),] %>%
      mutate(madeShot = ifelse(shotMade == 1, 'MadeShot', 'MissedShot')) %>%
      filter(grepl(team(), POSSESSION_ID), !is.na(madeShot)) %>%
      plot_ly(x=~BALL_Y_SHOTCHART, 
              y=~abs(BALL_X),
              color=~madeShot,
              marker = list(size = 10), 
              opacity = 0.6,
              colors=c('green3', 'red3'),
              type = 'scatter', 
              mode = 'markers',
              text=~Player,
              customdata=~Video,
              hovertemplate = paste('%{text}'),
              hoverlabel = list(font = list(family = 'Andale Mono'))) %>%
      layout(
      xaxis = list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        range = c(-25,25)
      ),
      yaxis = list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        range = c(50,0)
      ),
      images = list(
        list(
          # Add images
          source =  "https://miro.medium.com/v2/resize:fit:1400/format:webp/1*oqNDw7qGuhViV_P8bfG2zA.png",
          xref = "x",
          yref = "y",
          x = -25.5,
          y = -1,
          sizex = 50,
          sizey = 50,
          sizing = "stretch",
          opacity = 1,
          layer = "below"
        )
      ), 
      legend = list(x = 0.5, y = 0,
                    xanchor = 'center',
                    orientation = "h", 
                    font = list(family = 'Andale Mono')),
      title = list(text = 'Click on Shot for Possession Video', 
                   font = list(family = 'Andale Mono'))
      ) %>%
      onRender(js)
      

  })
  
  output$shot_chart_title <- renderText({
    shot_chart_title <- paste0(team(), ' Shot Chart')
    shot_chart_title
  })
  
  # Video
    
  output$video <- renderUI({
    poss_data <- data[which(data$POSSESSION_ID == possession()),]
    link <- tail(poss_data$Video[which(poss_data$Video != '')], n = 1)
    tags$video(width="896", height="504", src = link, type = "video/mp4", autoplay = TRUE, controls = NA, muted = TRUE, loop = TRUE)
  })
  
  # Offensive Spacing
  
  output$off_spacing <- renderText({
    poss_data <- data[which(data$POSSESSION_ID == possession()),]
    off_spacing <- poss_data %>% 
      filter(wasShot == 1) %>% 
      mutate(OFF_SPACING = round(OFF_SPACING,2))
    off_spacing <- paste0(off_spacing$OFF_SPACING[nrow(off_spacing)], ' feet')
    off_spacing
  })
  
  # Defensive Spacing
  
  output$def_spacing <- renderText({
    poss_data <- data[which(data$POSSESSION_ID == possession()),]
    def_spacing <- poss_data %>% 
      filter(wasShot == 1) %>% 
      mutate(DEF_SPACING = round(DEF_SPACING,2)) 
    def_spacing <- paste0(def_spacing$DEF_SPACING[nrow(def_spacing)], ' feet')
    def_spacing
  })
  
  # Shot Outcome
  
  output$shot_outcome <- renderText({
    if (is.null(possession())) {
      shot_outcome == ' '
      shot_outcome
    }
    else {
      poss_data <- data[which(data$POSSESSION_ID == possession()),]
      outcome <- poss_data %>% 
        filter(wasShot == 1)
      
      if (is.na(outcome$shotMade[nrow(outcome)])) {
        shot_outcome <- 'No Shot'
      }
      else {
        if (outcome$shotMade[nrow(outcome)] == 1) {
          shot_outcome <- 'Made Shot'
        }
        else {
          shot_outcome <- 'Missed Shot'
        }
      }
      
      shot_outcome
    }
    
  })
    
  # Game Progression
  
  output$gametracker <- renderPlotly({
    req(possession)
    
    home_team <- 'MIA'
    away_team <- 'BOS'
    
    home_color <- ifelse(
      home_team == 'BOS', '#007A33', ifelse(
        home_team == 'DAL', '#00538C', ifelse(
          home_team == 'GSW', '#1D428A', ifelse(
            home_team == 'MIA', '#98002E', 'black'
          )
        )
      )
    )
    
    away_color <- ifelse(
      away_team == 'BOS', '#007A33', ifelse(
        away_team == 'DAL', '#00538C', ifelse(
          away_team == 'GSW', '#1D428A', ifelse(
            away_team == 'MIA', '#98002E', 'black'
          )
        )
      )
    )
    
    vline <- function(x, color = "#f7f6f7", dash = 'solid', layer = 'below') {
      list(
        type = "line", 
        y0 = 0, 
        y1 = max(scoring$HOME_SCORE, scoring$VISITOR_SCORE), 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color, dash = dash),
        layer = layer
      )
    }
    
    scoring <- data %>% filter(gameClockStopped == FALSE | gameClockStopped == 'False')
    
    current <- scoring %>%
      filter(POSSESSION_ID == possession())
    
    current <- which(scoring$wallClock == current$wallClock[1])[1]
    
    scoring %>% 
      plot_ly(x = as.numeric(rownames(scoring)), 
              y = ~as.numeric(HOME_SCORE), 
              type = 'scatter', 
              mode = 'lines',
              customdata= ~period,
              name = home_team,
              line = list(color = home_color),
              hovertemplate = paste0('Q%{customdata}<br>','Score: %{y:}<br>','<extra></extra>'),
              hoverlabel = list(font = list(family = 'Andale Mono'))
      ) %>% 
      add_trace(y = ~as.numeric(VISITOR_SCORE), type = 'scatter', mode = 'lines', name = away_team, line = list(color = away_color)) %>%
      layout(
        font = list(family = 'Andale Mono'),
        xaxis = list(
          title = "",
          showline = FALSE,
          ticktext = list("Q1", "Q2", "Q3", "Q4"), 
          tickvals = list(nrow(scoring[which(scoring$period == 1),])/2,
                          3*nrow(scoring[which(scoring$period == 2),])/2,
                          5*nrow(scoring[which(scoring$period == 3),])/2,
                          7*nrow(scoring[which(scoring$period == 4),])/2
          ),
          showgrid = FALSE,
          rangeselector = list(xanchor = 'right')
        ),
        yaxis = list(
          title = list(text = "Score", font = list(family = 'Andale Mono')),
          showline = FALSE
        ),
        shapes = list(vline(nrow(scoring[which(scoring$period == 1),])),
                      vline(nrow(scoring[which(scoring$period %in% c(1,2)),])),
                      vline(nrow(scoring[which(scoring$period %in% c(1,2,3)),])),
                      vline(nrow(scoring[which(scoring$period %in% c(1,2,3,4)),])),
                      vline(current, color = 'black', dash = 'dot', layer = 'above')
        ),
        legend = list(font = list(family = 'Andale Mono'))
      )
  })
  
  # Animation Image
  
  output$animation_img <- renderImage({
    req(possession)
    file_path <- paste0('animation_png/', possession(), '.png')
    list(src = file_path,width="896px", height="504px")
  }, deleteFile = FALSE
  )
  
  # Animation GIF
  
  output$animation_gif <- renderImage({
    req(possession)
    file_path <- paste0('animation_gif/', possession(), '.gif')
    list(src = file_path,width="896px", height="504px")
  }, deleteFile = FALSE
  )
  
}


shinyApp(ui = ui, server = server)

