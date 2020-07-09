################################################################################
# Hard core codes for doing the real job
################################################################################
library(httr)
library(tibble)
library(tidyr)
library(dbplyr)
library(dplyr)
library(RSQLite)
library(purrr)
library(rio)
library(rdrop2)
library(DT)
source("BehindScene/bungie_api.R")
source("BehindScene/DestinyManifestSQL.R")
source("BehindScene/DownUpload.R")

################################################################################
# Website---user interface
################################################################################
library(shiny)
library(shinythemes)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # shinythemes::themeSelector(), 
  # theme = shinytheme("superhero"),
  # App title ----
  titlePanel("Destiny 2 PvP Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    position = "right",
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Source of D2 image: img(src = "rstudio.png", height = 140, width = 400)
      img(src = "D2Banner.jpg", width = "100%"),
      # Line break
      br(),
      # Input: platform choice
      selectInput(
        "platform",
        strong("Choose your platform"),
        choices = list(
          "Xbox" = 1,
          "PlayStation" = 2,
          "Steam" = 3,
          "Blizzard" = 4,
          "Stadia" = 5
        ),
        selected = 2
      ),
      br(),
      # Input: display name
      textInput("displayName",
                label = "Enter the name of your guardian",
                placeholder = "Enter the name of your guardian..."),
      actionButton("nameButton", "Search", class = "btn-success"),
      br(),
      # Input: select one character
      selectInput("character", strong("Choose one character"), choices = NULL),
      actionButton("charButton", "Submit character", class = "btn-success"),
      br(),
      br(),
      # Input: Track myself
      actionButton("trackMeButton", "Track semibruin!", class = "btn-danger"),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Description of the app.
      h3("Download Destiny 2 PvP games data about you and your opponents."),
      br(),
      # Dynamically show the emblem of the selected character
      htmlOutput("emblem"),
      # Test tracking myself
      h3(textOutput("semibruinNum")),
      # Show table of past 5 games
      tabsetPanel(
        id = 'dataset',
        tabPanel("Rumble 1", DT::dataTableOutput("rumble1")),
        tabPanel("Rumble 2", DT::dataTableOutput("rumble2"))
        # tabPanel("Rumble 3", DT::dataTableOutput("rumble3")),
        # tabPanel("Rumble 4", DT::dataTableOutput("rumble4")),
        # tabPanel("Rumble 5", DT::dataTableOutput("rumble5"))
      ),
      # Show the spider web of the players in my past games.
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  # Search characters
  foundChar <- eventReactive(
    input$nameButton,
    listChar(input$displayName, input$platform)
  )
  # Update the list of characters to use as choices
  observeEvent(foundChar(), {
    choices <- foundChar()$choicevalues
    # update the select input
    updateSelectInput(session, "character", choices = choices)
  })
  # Display the emblem of the selected player
  observeEvent(foundChar(), {
    output$emblem <- renderText({
      foundChar()$emblem[[as.integer(input$character)]]
    })
  })
  # Supply membership type, destiny id and chara id.
  anyoneId <- eventReactive({
    foundChar()
    input$charButton
    },
    # Obtain membership type, membership Id and character Id
    foundChar()$mTypeId[[as.integer(input$character)]]
  )
  # foundGames now is data frame of my games
  foundGames <- eventReactive({
    foundChar()
    input$charButton
    }, {
    # Obtain membership type, membership Id and character Id
    mTypeIdChar <- anyoneId()
    # Download games data
    dg <- downloadGames(mTypeIdChar, 2, NULL)
    dg <- dg %>%
      select(instanceId, displayName, score, efficiency, CombatRating, Weapon1, Weapon2)
    # Show only a part of dp
    group_split(dg, instanceId)
    
  })
  # List a table of game information
  observeEvent(foundGames(), {
    output$rumble1 <- DT::renderDataTable({ DT::datatable(foundGames()[[1]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
    output$rumble2 <- DT::renderDataTable({ DT::datatable(foundGames()[[2]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
    # output$rumble3 <- DT::renderDataTable({ DT::datatable(foundGames()[[3]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
    # output$rumble4 <- DT::renderDataTable({ DT::datatable(foundGames()[[4]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
    # output$rumble5 <- DT::renderDataTable({ DT::datatable(foundGames()[[5]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
  })
  # Works for me only
  myId <- eventReactive(
    input$trackMeButton,
    list(membershipType = 2, destinyMembershipId = "4611686018460891184", characterId = "2305843009612934251")
  )
  # Now currentData() is a tibble.
  currentData <- eventReactive({
    input$trackMeButton
    myId()
    }, {
    loadData()
 })
  # Feed my account to downloadGames.
  output$semibruinNum <- renderText({
    n <- nrow(currentData() %>% select(instanceId) %>% unique)
    paste0("We have collected ", n, " games for semibruin!")
  })
  foundMyGames <- eventReactive({
    myId()
    currentData()
    }, {
    # Download games data and save data into dropbox.
    dg <- downloadGames(myId(), Inf, currentData())
    dg <- dg %>%
      select(instanceId, displayName, score, efficiency, CombatRating, Weapon1, Weapon2)
    # Show only a part of dp
    group_split(dg, instanceId)
  })
  observeEvent(foundMyGames(), {
    output$rumble1 <- DT::renderDataTable({ DT::datatable(foundMyGames()[[1]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
    output$rumble2 <- DT::renderDataTable({ DT::datatable(foundMyGames()[[2]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
    # output$rumble3 <- DT::renderDataTable({ DT::datatable(foundMyGames()[[3]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
    # output$rumble4 <- DT::renderDataTable({ DT::datatable(foundMyGames()[[4]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
    # output$rumble5 <- DT::renderDataTable({ DT::datatable(foundMyGames()[[5]] %>% select(-instanceId)) %>% formatRound(columns=c('efficiency', 'CombatRating'), digits=1)})
  })
}

shinyApp(ui = ui, server = server)
