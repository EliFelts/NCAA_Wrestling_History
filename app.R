### Data are compiled, start building pieces and parts of shiny app

library(tidyverse)
library(googlesheets4)
library(bslib)
library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(conflicted)
library(bsicons)
library(fontawesome)
library(arrow)


conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag,
                 plotly::layout)


# right now reading in from google sheets
# because cleaning up a couple things still; will
# move these to feather to improve speed later

matches_master <- read_sheet("https://docs.google.com/spreadsheets/d/1yDlDRlShRcc_aWd_SmDuJ-5naNwhjIQHPVN4UVcpM24/edit?gid=1555277773#gid=1555277773")
wrestlers_master <- read_sheet("https://docs.google.com/spreadsheets/d/11TR6yUScjdF4OJYoqiVV4PqruJg2-KXmCCBljk9ABSw/edit?gid=325863048#gid=325863048")

# start building individual page

ind_years_formatted <- wrestlers_master %>% 
  mutate(display_name=word(wrestler_id,1,sep="_",),
         Record=str_c(wins,losses,sep="-"),
         Matches=wins+losses,
         `Bonus Percent`=round(bonus/Matches*100)) %>% 
  select(Name = display_name,Team=team,Weight=weight_class,
         Seed=seed,
         Year=year,
         Placement=placement,`Team Points`=team_points,
         `Bonus Points`=bonus_points,Record,
         Terminations=terminations,Pins=falls,
         Bonus=bonus,
         `Bonus Percent`,Matches,Wins=wins) %>% 
  mutate(Placement=ifelse(is.na(Placement),"DNP",Placement),
         Placement=factor(Placement,
                          levels=c("First","Second","Third","Fourth",
                                   "Fifth","Sixth","Seventh","Eighth",
                                   "DNP")))

# make some formatting changes for individual tournaments

# think about adding a plot showing the distribution of team points among the 
# selection

ind_years_hist <- ind_years_formatted %>% 
  filter(Year>1980,Seed==2) %>% 
  ggplot(aes(x=`Team Points`)) + 
  geom_histogram()+
  facet_wrap(~Seed,
             scales="free_y")+
  coord_flip()+
  theme_bw()

ind_years_hist



# build user interface

ui <- page_navbar(
  
  
  title="NCAA Wrestling Tournament Results",
  
  theme=bs_theme(preset="cerulean"),
  
  sidebar=sidebar(width=500,
                  
                  accordion(
                    
                    accordion_panel(
                      
                      "Explore Data",
                      
                      sliderInput(inputId = "ind_dates",
                                  label="Choose a range of years",
                                  min=min(wrestlers_master$year),
                                  max=max(wrestlers_master$year),
                                  value=c(1980,max(wrestlers_master$year)),
                                  sep=""),
                      
                      uiOutput("team_options"),

                      
                      
                      pickerInput(inputId = "placement_filter",
                                  label="Filter by Placement",
                                  choices=levels(ind_years_formatted$Placement),
                                  selected=levels(ind_years_formatted$Placement),
                                  multiple=TRUE,
                                  options=list(
                                    
                                    `actions-box` = TRUE
                                    
                                  )),
                      
                      pickerInput(inputId = "seed_filter",
                                  label="Filter by Seed",
                                  choices=seq(1,33,1),
                                  selected=seq(1,33,1),
                                  multiple=TRUE,
                                  options=list(
                                    
                                    `actions-box` = TRUE
                                    
                                  ))
                      
                       
                    )
                    
                    )),
  
  nav_panel("Explore Data",

            page_fillable(
                      
                      card(
                        
                        card_header("Individual Tournament Summaries"),
                        DTOutput("ind_tourneys_table"),
                        full_screen = TRUE
                        
                      )
                      
                    ))
                    
                  
                  
                  
  
)

server <- function(input,output,session){
  

  
  # make the individual tournaments by wrestlers filter reactively
  
  ind_tourneys_reactive <- reactive({
    
    req(input$ind_dates)
    
    ind_tourney_min <- min(input$ind_dates)
    ind_tourney_max <- max(input$ind_dates)
    
    ind_tourney.dat <- ind_years_formatted %>% 
      filter(Year>=ind_tourney_min,
             Year<=ind_tourney_max,
             Placement %in% input$placement_filter,
             Seed %in% input$seed_filter)
    
    
  })
  
  # make the teams available selectInput reactive so
  # it only includes the teams avaialable in the date range selected
  
  output$team_options <- renderUI({
    
    req(input$ind_dates)
    
    dat <- ind_tourneys_reactive() %>% 
      arrange(Team)
    
    pickerInput(inputId = "team_filter",
                label="Filter by Team",
                choices=unique(dat$Team),
                selected=unique(dat$Team),
                multiple=TRUE,
                options=list(
                  
                  `actions-box` = TRUE,
                  `live-search` = TRUE
                  
                ))
    
    
  })
  
  # render a data table based on filters
  
  output$ind_tourneys_table <- renderDT({
    
    ind_tourneys_reactive() %>% 
      filter(Team %in% input$team_filter) %>% 
      arrange(desc(`Team Points`))
    
  })
  
}

shinyApp(ui,server)
