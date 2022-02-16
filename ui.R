library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

##### used code from the following: https://github.com/pspachtholz/BookRecommender/blob/master/ui.R

source('functions/helpers.R')

sidebar <- dashboardSidebar(
            sidebarMenu(
             menuItem("Recommend By Genre", tabName = "genre"),
             menuItem("Recommend By User", tabName = "user")
    ))
    
body <- dashboardBody(includeCSS("css/movies.css"),
         tabItems(
           tabItem(tabName = "genre",
                  fluidRow(
                    box(width = 12, title = "Select Your Genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        selectInput("Genre", "Genre:",
                                    c("Action", "Adventure", "Animation", 
                                      "Children.s", "Comedy", "Crime",
                                      "Documentary", "Drama", "Fantasy",
                                      "Film.Noir", "Horror", "Musical", 
                                      "Mystery", "Romance", "Sci.Fi", 
                                      "Thriller", "War", "Western"))
                        ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("results")
                  )))),
           
            tabItem(tabName = "user", 
                    fluidRow(
                      box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                          div(class = "rateitems",
                              uiOutput('ratings')
                          )
                      )
                    ),
                    fluidRow(
                      useShinyjs(),
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                          actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results2"))))))
  
shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movie Recommender"),
    sidebar,
    body))