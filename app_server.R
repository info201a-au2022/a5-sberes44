# server.r
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(tidyr)

# Import data as a data frame
co2_data <- read.csv('./data/owid-co2-data.csv')

# Manipulate data frame to reduce data to country and cumulative co2 sources
# Data can then be sorted to show a single country's cumulative co2 over years
get_cumulative_country_co2_data <- function(data) {
  cumulative_country_co2_data <- data %>%
  select(country, year, cumulative_cement_co2, cumulative_coal_co2,
         cumulative_flaring_co2, cumulative_gas_co2, cumulative_luc_co2,
         cumulative_oil_co2, cumulative_other_co2) %>%
  return(cumulative_country_co2_data)
}

c02_country_test_data <- get_cumulative_country_co2_data(co2_data)

build_plot_chart <- function(data, target_country, min_year, max_year) {
  
  country_specific_data <- get_cumulative_country_co2_data(data) %>%
    filter(country == target_country)
  
  min_max_year_country_data <- country_specific_data %>%
    filter(year > min_year) %>%
    filter(year < max_year)
  
  p <- plot_ly(min_max_year_country_data, x = ~year,y = ~cumulative_cement_co2,
               name = 'Cement',type = 'scatter', mode = 'lines') %>%
    layout(title = list(text = '<b>CO2 Sources Graph</b>', font = list(size = 16)),
           plot_bgcolor = "#efefef",
           xaxis = list(title = '<b>Year</b>'), 
           yaxis = list(title = '<b>CO2 Emissions</b> (Millions of Tons)'),
           legend = list(title=list(text='<b> CO2 Source Type </b>')))
  p <- p %>% add_trace(y = ~cumulative_flaring_co2, name = 'Flaring',
                       mode = 'lines')
  p <- p %>% add_trace(y = ~cumulative_gas_co2, name = 'Gas', mode = 'lines')
  p <- p %>% add_trace(y = ~cumulative_luc_co2, name = 'Land-Use Change',
                       mode = 'lines')
  p <- p %>% add_trace(y = ~cumulative_oil_co2, name = 'Oil', mode = 'lines')
  p <- p %>% add_trace(y = ~cumulative_other_co2, name = 'Other', mode = 'lines')
  return(p)
}

# Country List
country_list <- co2_data %>%
  select(country, population) %>%
  group_by(country) %>%
  summarize(avg_population = mean(population, na.rm = TRUE)) %>%
  spread(key = country, value = avg_population) %>%
  head(0)
  
# Year Range List
year_list <- co2_data %>%
  select(year, population) %>%
  group_by(year) %>%
  summarize(avg_population = mean(population, na.rm = TRUE)) %>%
  spread(key = year, value = avg_population) %>%
  head(0)
  

server <- function(input, output) {
  
  # Find cumulative CO2 in the U.S. for most recent year (2021)
  cumulative_co2_in_US <- reactive({
    cumulative_US <- co2_data %>%
      select(country, year, cumulative_co2) %>%
      filter(country == "United States") %>%
      filter(year == max(year)) %>%
      select(cumulative_co2) %>%
      mutate(across(cumulative_co2, round, 2))
      cumulative_us <- cumulative_US$cumulative_co2[1]
      cumulative_us <- paste(cumulative_us, "million tons of CO2.")
      paragraph_1 <- paste("My analysis of Co2 emissions data focuses on the 
             cumulative accumulation of CO2 within each country, focusing on the
             amount of CO2 that has been contributed in total by each source of
             CO2 over the years within each country. It was found that the total
             cumulative amount of CO2 emitted within the U.S. for the most
             recent year of 2021 was found to equal", cumulative_us)
    return(paragraph_1)
  })
  
  # Find cumulative CO2 averaged across all of the countries (2021)
  cumulative_co2_average <- reactive({
    cumulative_avg <- co2_data %>%
      select(country, year, cumulative_co2) %>%
      filter(year == max(year)) %>%
      summarize(average_cumulative_co2 = mean(cumulative_co2, na.rm = TRUE)) %>%
      mutate(across(average_cumulative_co2, round, 2))
      cumulative_avg <- cumulative_avg$average_cumulative_co2[1]
      cumulative_avg <- paste(cumulative_avg, "million tons of CO2.")
      paragraph_2 <- paste0("Along with this value, the average cumulative CO2
              spread across all of the countries for the most recent year of
              2021 was found to equal ", cumulative_avg, ".")
    return(paragraph_2)
  })
  
  # Find which country produced the highest cumulative CO2 in most recent year
  country_highest_cumulative_co2 <- reactive({
    
    # First the continents and world have to be removed from the dataframe
    removal_list <- paste(c("World", "Africa", "Africa (GCP)", "Asia",
                            "Asia (excl. China and India)", "Asia (GCP)",
                            "Central America (GCP)", "Europe",
                            "Europe (excl. EU-27)", "Europe (excl. EU-28)",
                            "Europe (GCP)", "European Union (27)",
                            "European Union (27) (GCP)", "European Union (28)",
                            "French Equatorial Africa (GCP)",
                            "Frech West Africa (GCP)", "High-income countries",
                            "International transport", "Kuwaiti Oil Fires (GCP)",
                            "Low-income countries",
                            "Lower-middle-income countries", "Middle East (GCP)",
                            "Non-OECD (GCP)", "North America",
                            "North America (excl. USA)", "North America (GCP)",
                            "Oceania (GCP)", "OECD (GCP)",
                            "Panama Canal Zone (GCP)", "South America",
                            "South America (GCP)", "Upper-middle-income countries"
                            ), collapse = '|')
    only_countries_co2_data <- co2_data %>%
      filter(!grepl(removal_list, country))
      
    country_high <- only_countries_co2_data %>%
      select(country, year, cumulative_co2) %>%
      filter(year == max(year)) %>%
      filter(cumulative_co2 == max(cumulative_co2, na.rm = TRUE))
      country_high <- country_high$country[1]
      paragraph_3 <- paste("The countries were then each analyzed to see which
              country resulted in the highest cumulative CO2 emissions for the
              most recent year of 2021. The country that was found to have
              contributed the most to CO2 emissions was", country_high, "which
              had the highest cumulative CO2 emissions as of 2021.")
      return(paragraph_3)
  })
  
  output$TotalRecentUSco2 <- function() {
    cumulative_co2_in_US()
  }
  
  output$TotalAverageco2 <- function() {
    cumulative_co2_average()
  }

  output$HighestCountryco2 <- function() {
    country_highest_cumulative_co2()
  }
  
  output$chart <- renderPlotly({
    return(build_plot_chart(co2_data, input$country_s,
                            input$min_year, input$max_year))
  })
}

