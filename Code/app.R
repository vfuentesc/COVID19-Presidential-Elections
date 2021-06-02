rm(list=ls())
options(scipen = 10^6)

library(shiny)
library(tidyverse)
library(lubridate)
library(urbnmapr)
library(sf)
library(shinythemes)
library(rsconnect)
library(plotly)
library(scales)
library(kableExtra)

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project"

setwd(wd)

panel_elections <- read_csv(file.path("Intermediate","1976-2020_panel_elections.csv"))
states_sf <- get_urbn_map("states", sf = TRUE)

Plots <- readRDS(file.path("Intermediate","PlotsToShiny.rds"))

# Define UI 
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "The Impact of COVID-19 on 2020 Presidential Elections",
    tabPanel(
      "1976-2020 Results by State",
      mainPanel(
        fluidRow(
          column(
            4,
            sliderInput(
              inputId = "date",
              label = h3("Choose an election year"),
              min = min(panel_elections$year),
              max = max(panel_elections$year),
              step = 4,
              sep = "",
              value = max(panel_elections$year))
            
          ),
          column(
            8,
            htmlOutput("candidates")
          )
        ),
        fluidRow(
          plotlyOutput("elections")
        )
      )
    )
  )
)

# Define server 
server <- function(input, output) {
    
    df <-
        reactive({panel_elections %>%
        filter(year == input$date,
               winner == 1)})

    party_colors <- c("#2E74C0", "#CB454A")
    
    output$elections <-
        renderPlotly({
            year_title <- mean(df()$year)
            
            ggplotly(
                states_sf %>%
                    left_join(df(), by = c("state_abbv" = "state_lab")) %>%
                    ggplot() +
                    geom_sf(aes(fill = party), color = "grey55", size = 0.25) +
                    geom_sf_text(aes(label = paste(state_abbv, "\n", electoral_votes)), 
                                 size = 2)+
                    scale_fill_manual(values = party_colors) + 
                    theme_minimal() + 
                    labs(fill = "") +
                    theme(legend.position = c(0.9, 0.15),
                          panel.background = element_blank(),
                          axis.title = element_blank(),
                          axis.text = element_blank(),
                          panel.grid = element_blank()),
                tooltip = c("state", "party", "votes"),
                height = 400
                ) %>%
                layout(title = list(text = paste("Year:",year_title),
                                    y = 0.95)) %>%
                highlight(
                    on = 'plotly_hover', persistent = FALSE, opacityDim = getOption("opacityDim", .1))
            })
    
    output$candidates <-
        renderText(
            panel_elections %>%
                filter(year == input$date, type == "absolute") %>%
                group_by(candidate, party) %>%
                summarise(total = sum(votes),
                          electoral = sum(electoral_votes * winner)) %>%
                kable(col.names = c("Candidate", "Party", "Popular", "Electoral"),
                      full_width = F,
                      align = c("lccc"),
                      format.args = list(big.mark = ",", scientific = FALSE)) %>%
                kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
                add_header_above(c(" " = 1, " " = 1, "Votes" = 2))
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)