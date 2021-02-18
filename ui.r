## ui.R ##
library(shiny); library(DT); library(shinyjs); library(shinyBS); library(zoo); library(quantmod);
library(tidyverse); library(htmltools); library(dygraphs); library(xts); library(d3scatter) 
library(crosstalk); library(plotly); library(scatterD3); library(janitor)

#setwd("C:/Users/natha/OneDrive/Desktop/Shiny DR")

source("appParts.r")
source("readData.r")


ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),
  position = "fixed-top",
  tags$head(
    tags$link(
              rel="shortcut icon", 
              href="https://www.shareicon.net/data/128x128/2016/08/18/815787_basketball_512x512.png"
              ),
    tags$style(HTML("tbody > tr:last-child {font-weight: bold;}"))
           ),
  title = "Fantasy Draft" ,
  navbarPage("Draft Center",
             tabPanel("Home",
                      h2(style = "text-align: center", "Welcome to the Fantasy Draft Center!" )),
             tabPanel("Tables",
                      tabsetPanel(type = "tabs",
                                  tabPanel("Overview",  
                                           overviewSection()),
                                  tabPanel("Depth Charts",  
                                           depthSection()),
                                  tabPanel("Player Bios",
                                           bios()),
                                  tabPanel("Draft Offers",
                                           drafts()),
                                  tabPanel("Per 30 Percentile",
                                           pct30Section()),
                                  tabPanel("Full Season Percentile", 
                                           pctSection()) ,
                                  tabPanel("Second Half Percentile",  
                                           pctHalf2Section()) ,
                                  tabPanel("First Half Percentile",  
                                           pctHalf1Section()) ,                                  
                                  tabPanel("Z-Score",  
                                           zSection())
                      )),
             tabPanel("Progress Tracker",
                      progressSection()  
                    ),  
             tabPanel("Explore",
                      crossTalksection()),
             tabPanel("Mock Draft",
                      mockDraftSection()
             ),
             tabPanel("About",
                      h2("Author"),
                      h3("Nathan Cahn")) 
             
   ) # close navbar page
) # close app