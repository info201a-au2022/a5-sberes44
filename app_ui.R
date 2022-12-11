library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(tidyr)

source("./app_server.R")

chart_sidebar_content <- sidebarPanel(
  varSelectInput(
    "country_s",
    label = "Region to Explore:",
    country_list,
    multiple = FALSE
  ),
  varSelectInput(
    "min_year",
    label = "Minimum Year to Evaluate",
    year_list,
    selected = 1750,
    multiple = FALSE
  ),
  varSelectInput(
    "max_year",
    label = "Maximum Year to Evaluate",
    year_list,
    selected = 2021,
    multiple = FALSE
  )
)

chart_main_content <- mainPanel(
  plotlyOutput("chart")
)

chart_panel <- tabPanel(
  "CO2 Sources Graph",
  titlePanel("Cumulative CO2 Sources Within Selected Country By Year"),
  p(""),
  sidebarLayout(chart_sidebar_content, chart_main_content),
  p("I included this chart as it reveals the total cumulative CO2 contributions 
    separated by the different CO2 sources. The interactive chart allows you to 
    select the country or region in which you want to look into, along with the 
    minimum and maximum year you want to evaluate. This chart allows you to see 
    which countries have contributed a little or large amount to CO2 pollution, 
    along with seeing what types of sources are contributing most to a country's 
    emissions. I found it very interesting that land-use change is one of the 
    largest contributors to most of the countries' CO2 emissions, as opposed to 
    gas or oil."))

ui <- navbarPage(
  "Country CO2 Emission Sources",
  tabPanel("Introduction",
           h3("Introduction to Cumulative CO2 Sources"),
           p(textOutput("TotalRecentUSco2")),
           p(textOutput("TotalAverageco2")),
           p(textOutput("HighestCountryco2")),
           h3("CO2 Sources Graph Tab"),
           p("On the next tab, there is a chart which allows you to select a 
             country or region in the selection box on the left, which will 
             result in five lines being plotted in the graph on the right, 
             revealing the cumulative CO2 emissions from five different sources 
             of CO2. To select the next tab, please click on the ~CO2 Sources 
             Graph~ tab at the top of the page!")),
  chart_panel)
