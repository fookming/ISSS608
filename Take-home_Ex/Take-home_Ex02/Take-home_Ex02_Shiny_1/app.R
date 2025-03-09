#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(plotly)
library(tidyr)
library(DT)  # Interactive table

# Load Data
df_imports <- read_excel("data/outputFile.xlsx", sheet = "T1")
df_exports <- read_excel("data/outputFile.xlsx", sheet = "T2")
df_reexports <- read_excel("data/outputFile.xlsx", sheet = "T3")

names(df_imports)[1] <- "Region"
names(df_exports)[1] <- "Region"
names(df_reexports)[1] <- "Region"

all_cols <- names(df_imports)[3:ncol(df_imports)]
all_years <- sort(unique(str_sub(all_cols, 1, 4)))

quarters <- list(
  "All" = all_cols,  
  "Q1" = c("Jan", "Feb", "Mar"),
  "Q2" = c("Apr", "May", "Jun"),
  "Q3" = c("Jul", "Aug", "Sep"),
  "Q4" = c("Oct", "Nov", "Dec")
)

regions_available <- c("All", unique(df_imports$Region))

# Shiny UI
ui <- fluidPage(
  titlePanel("Year-over-Year Trade Analysis"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("region", "Region:", choices = regions_available, selected = "All"),
      selectInput("start_year", "Start Year:", choices = all_years[all_years <= "2024"], selected = "2014"),
      selectInput("end_year", "End Year:", choices = all_years[all_years <= "2024"], selected = "2024"),
      selectInput("quarter", "Select Quarter:", choices = c("All", names(quarters)[-1]), selected = "All")
    ),
    mainPanel(
      width = 10,
      plotlyOutput("trade_plot", height = "600px"),
      br(),
      h4("Trade Summary by Year"),
      DTOutput("summary_table")  # Summary table
    )
  )
)

# Server logic
server <- function(input, output) {
  
  filtered_df <- reactive({
    req(input$start_year, input$end_year, input$quarter)
    
    yrs <- as.character(input$start_year:input$end_year)
    
    if (input$quarter == "All") {
      month_cols <- all_cols[grepl(paste(yrs, collapse = "|"), all_cols)]
    } else {
      month_cols <- all_cols[grepl(paste(quarters[[input$quarter]], collapse = "|"), all_cols) &
                               grepl(paste(yrs, collapse = "|"), all_cols)]
    }
    
    imports <- df_imports %>%
      select(Region, all_of(month_cols)) %>%
      pivot_longer(-Region, names_to = "Month", values_to = "Value") %>%
      group_by(Region, Year = substr(Month, 1, 4)) %>%
      summarise(Imports = sum(Value, na.rm = TRUE) / 1e3, .groups = "drop")
    
    exports <- df_exports %>%
      select(Region, all_of(month_cols)) %>%
      pivot_longer(-Region, names_to = "Month", values_to = "Value") %>%
      group_by(Region, Year = substr(Month, 1,4)) %>%
      summarise(Domestic_Exports = sum(Value, na.rm = TRUE)/1e3, .groups = "drop")
    
    reexports <- df_reexports %>%
      select(Region, all_of(month_cols)) %>%
      pivot_longer(-Region, names_to = "Month", values_to = "Value") %>%
      group_by(Region, Year = substr(Month, 1,4)) %>%
      summarise(Reexports = sum(Value, na.rm = TRUE)/1e3, .groups = "drop")
    
    combined_df <- imports %>%
      left_join(exports, by = c("Region", "Year")) %>%
      left_join(reexports, by = c("Region", "Year")) %>%
      mutate(
        Total_Exports = Domestic_Exports + Reexports,
        Trade_Balance = Total_Exports - Imports
      ) %>% arrange(Year)
    
    if(input$region == "All") {
      combined_df <- combined_df %>%
        group_by(Year) %>%
        summarise(across(c(Imports, Domestic_Exports, Reexports, Total_Exports, Trade_Balance), sum, na.rm=TRUE)) %>%
        mutate(Region = "All")
    } else {
      combined_df <- combined_df %>% filter(Region == input$region)
    }
    
    combined_df %>%
      mutate(
        Imports_YoY = round(100 * (Imports / lag(Imports) - 1), 1),
        DomExp_YoY = round(100 * (Domestic_Exports / lag(Domestic_Exports) - 1), 1),
        Reexp_YoY = round(100 * (Reexports / lag(Reexports) - 1), 1),
        TotalExp_YoY = round(100 * (Total_Exports / lag(Total_Exports) - 1), 1),
        Balance_YoY = round(100 * (Trade_Balance / lag(Trade_Balance) - 1), 1)
      )
  })
  
  # Plotly output
  output$trade_plot <- renderPlotly({
    plot_data <- filtered_df()
    
    plot_ly(plot_data, x = ~Year) %>%
      add_trace(y = ~Imports, type = "scatter", mode = "lines+markers+text",
                text = ~paste0(Imports_YoY, "%"), textposition = "top center", name = "Imports") %>%
      add_trace(y = ~Domestic_Exports, type = "scatter", mode = "lines+markers+text",
                text = ~paste0(DomExp_YoY, "%"), textposition = "top center", name = "Domestic Exports") %>%
      add_trace(y = ~Reexports, type = "scatter", mode = "lines+markers+text",
                text = ~paste0(Reexp_YoY, "%"), textposition = "top center", name = "Re-exports") %>%
      add_trace(y = ~Total_Exports, type = "scatter", mode = "lines+markers+text",
                text = ~paste0(TotalExp_YoY, "%"), textposition = "top center", name = "Total Exports") %>%
      add_trace(y = ~Trade_Balance, type = "scatter", mode = "lines+markers+text",
                text = ~paste0(Balance_YoY, "%"), textposition = "top center", name = "Trade Balance") %>%
      layout(
        title = paste("YoY Trade Analysis (", input$quarter, input$start_year, "-", input$end_year, ")"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Trade Volume (K Millions)"),
        margin = list(b=100)
      )
  })
  
  # Render DataTable for Trade Summary
  output$summary_table <- renderDT({
    filtered_df() %>%
      select(Year, Region, Imports, Domestic_Exports, Reexports, Total_Exports, Trade_Balance) %>%
      mutate(across(where(is.numeric), round, 1))
  })
}

shinyApp(ui, server)
