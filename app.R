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
library(hms)
library(forcats)


conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag,
                 plotly::layout)



# need to correct seeds because of team mismatch
# in original join

seeds_correct <- read_sheet("https://docs.google.com/spreadsheets/d/1Ot6SxKRJS4OHhIvXXp0pXFKAxs7p5_QDRXfIlLJfwh4/edit?gid=1291266525#gid=1291266525")

# right now reading in from google sheets
# because cleaning up a couple things still; will
# move these to feather to improve speed later

matches_master <- read_sheet("https://docs.google.com/spreadsheets/d/1yDlDRlShRcc_aWd_SmDuJ-5naNwhjIQHPVN4UVcpM24/edit?gid=1555277773#gid=1555277773") %>% 
  mutate(winner_firstlast=str_remove(winner, " \\(.*\\)$"),
         loser_firstlast=str_remove(loser, " \\(.*\\)$"),
         round=factor(round,levels=c("Prelim","Champ. Round 1","Champ. Round 2",
                                     "Consolation Prelim",
                                     "Cons. Round 1","Quarterfinal","Cons. Round 2","Cons. Round 3",
                                     "Semifinal","Cons. Round 4","Cons. Round 5",
                                     "Cons. Semi","7th Place Match",
                                     "5th Place Match","3rd Place Match","1st Place Match",
                                     "Cons. 2nd Quarterfinal","Cons. 2nd Semifinal",
                                     "2nd Place Match","Cons. 3rd Quarterfinal",
                                     "Cons. 3rd Semifinal","Round 1","Round 2",
                                     "Round 3","Round 4","Round 5","Round 6",
                                     "Round 7"
                                     )))


wrestlers_master <- read_sheet("https://docs.google.com/spreadsheets/d/11TR6yUScjdF4OJYoqiVV4PqruJg2-KXmCCBljk9ABSw/edit?gid=325863048#gid=325863048")%>% 
  select(-c(seed)) %>% 
  left_join(seeds_correct,by=c("wrestler","weight_class",
                               "year","team"))


# find wrestlers who won on their first appearance

first_champs <- wrestlers_master %>% 
  arrange(wrestler_id,year) %>% 
  group_by(wrestler_id) %>% 
  filter(year== min(year) & placement== "First") 

first_champs_summarize <- wrestlers_master %>% 
  inner_join(first_champs,by="wrestler_id") %>% 
  group_by(wrestler_id) %>% 
  summarize(finishes=paste(placement.x, collapse = ", "),
            appearances=n()) %>% 
  filter(appearances>3) %>% 
  mutate(years=word(wrestler_id,2,sep="_"),
         start_year=as.numeric(word(years,1,sep="-")))

# get the amount of points already earned
# by each wrestler at the start of each season

wrestlers_master2 <- wrestlers_master %>% 
  arrange(wrestler_id,year) %>% 
  group_by(wrestler_id) %>% 
  mutate(cumulative_prior_points=lag(cumsum(team_points),default=0),
         cumulative_prior_aa=lag(cumsum(!is.na(placement)), default=NA),
         cumulative_prior_titles=lag(cumsum(placement=="First"), default=0),
         cumulative_prior_finalists=lag(cumsum(placement %in% c("First","Second")), default=0)) %>%
  mutate(cumulative_prior_aa=ifelse(is.na(cumulative_prior_aa),0,
                                    cumulative_prior_aa),
         cumulative_prior_titles=ifelse(is.na(cumulative_prior_titles),0,
                                    cumulative_prior_titles),
         cumulative_prior_finalists=ifelse(is.na(cumulative_prior_finalists),0,
                                        cumulative_prior_finalists),
         prior_aa_logical=ifelse(cumulative_prior_aa>0,1,0),
         prior_champ_logical=ifelse(cumulative_prior_titles>0,1,0),
         prior_finalists_logical=ifelse(cumulative_prior_finalists>0,1,0)) %>% 
  ungroup()

# calculate total returning points by 
# weight class/year

weight_rankings <- wrestlers_master2 %>% 
  group_by(year,weight_class) %>% 
  summarize(earned_points=sum(cumulative_prior_points),
            aa_finishes=sum(cumulative_prior_aa,na.rm=T),
            individual_aa=sum(prior_aa_logical),
            champ_finishes=sum(cumulative_prior_titles,na.rm=T),
            individual_champs=sum(prior_champ_logical),
            finalist_finishes=sum(cumulative_prior_finalists,na.rm=T),
            individual_finalists=sum(prior_finalists_logical))

weight_search <- wrestlers_master2 %>% 
  filter(weight_class==177,
         year==1982) %>% 
  select(weight_class,year,wrestler,team,team_points,placement,
         cumulative_prior_points,cumulative_prior_aa,
         prior_aa_logical,prior_champ_logical,cumulative_prior_titles,
         cumulative_prior_finalists,prior_aa_logical)
  


# start building individual page

ind_years_formatted <- wrestlers_master %>% 
  mutate(display_name=word(wrestler_id,1,sep="_",),
         Record=str_c(wins,losses,sep="-"),
         Matches=wins+losses,
         `Bonus Percent`=round(bonus/Matches*100)) %>% 
  mutate(Name = display_name,Team=team,Weight=weight_class,
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
                                   "DNP")),
         Seed=as.factor(seed),
         Seed=fct_na_value_to_level(Seed, "Unseeded"))



careers_summary1 <- wrestlers_master %>% 
  mutate(falls_time=na_if(falls_time, "NA"),
         falls_time=as_hms(falls_time),
         tech_time=na_if(tech_time,"NA"),
         tech_time=as_hms(tech_time)) %>% 
  group_by(wrestler_id) %>% 
  summarize(teams_n=n_distinct(team),
            teams=toString(unique(team)),
            appearances=n(),
            wins=sum(wins),
            losses=sum(losses),
            matches=sum(wins,losses),
            team_points=sum(team_points),
            titles=sum(placement=="First",na.rm=T),
            finals=sum(placement %in% c("First","Second")),
            aa=sum(!is.na(placement)),
            falls=sum(falls,na.rm=T),
            fall_time=as_hms(sum(falls_time,na.rm=T)),
            terminations=sum(terminations,na.rm=T),
            bonus=sum(bonus,na.rm=T),
            termination_percent=round(terminations/matches*100),
            bonus_percent=round(bonus/matches*100)) %>%
  mutate(points_per_tourney=team_points/appearances) %>% 
  arrange(-team_points)

# five correction

fivers <- careers_summary1 %>% 
  filter(appearances>4)

fivers_career_summary <- wrestlers_master %>% 
  filter(wrestler_id %in% fivers$wrestler_id) %>% 
  mutate(falls_time=na_if(falls_time, "NA"),
         falls_time=as_hms(falls_time),
         tech_time=na_if(tech_time,"NA"),
         tech_time=as_hms(tech_time)) %>% 
  group_by(wrestler_id) %>% 
  arrange(wrestler_id,-team_points) %>% 
  mutate(season_rank=row_number()) %>% 
  filter(season_rank<5) %>% 
  group_by(wrestler_id) %>% 
  summarize(teams_n=n_distinct(team),
            teams=toString(unique(team)),
            appearances=n(),
            wins=sum(wins),
            losses=sum(losses),
            matches=sum(wins,losses),
            team_points=sum(team_points),
            titles=sum(placement=="First",na.rm=T),
            finals=sum(placement %in% c("First","Second")),
            aa=sum(!is.na(placement)),
            falls=sum(falls,na.rm=T),
            fall_time=as_hms(sum(falls_time,na.rm=T)),
            terminations=sum(terminations,na.rm=T),
            bonus=sum(bonus,na.rm=T),
            termination_percent=round(terminations/matches*100),
            bonus_percent=round(bonus/matches*100)) %>%
  mutate(points_per_tourney=team_points/appearances) %>% 
  arrange(-team_points)

careers_summary2 <- careers_summary1 %>% 
  filter(appearances<5) %>% 
  bind_rows(fivers_career_summary) %>% 
  mutate(career_range=word(wrestler_id,2,sep="_"),
         career_start=as.numeric(word(career_range,1,sep="-")),
         career_end=as.numeric(word(career_range,2,sep="-")))

careers_formatted <- careers_summary2 %>% 
  mutate(Wrestler=word(wrestler_id,1,sep="_")) %>% 
  select(wrestler_id,career_start,career_end,
         Wrestler,`Years Active`=career_range,
         `Team(s)`=teams,Appearances=appearances,
         `Team Points`=team_points,Titles=titles,
         `Team Points per Appearance`=points_per_tourney,
         `Finals Appearances`=finals,
         `AA Finishes`=aa,Wins=wins,Losses=losses,
         Falls=falls,`Total Falls Time`=fall_time,
         `Bonus Wins`=bonus,`Bonus Percent`=bonus_percent)


# for ranking brackets join in career accomplishments
# in addition to what they had already earned

career_relevant <- careers_summary2 %>% 
  select(wrestler_id,career_team_points=team_points,career_titles=titles,
         career_finals=finals,career_aa=aa) %>% 
  mutate(career_aa_logical=ifelse(career_aa>0,1,0),
         career_champ_logical=ifelse(career_titles>0,1,0))

wrestlers_master3 <- wrestlers_master2 %>% 
  left_join(career_relevant,by="wrestler_id")


weight_rankings <- wrestlers_master3 %>% 
  group_by(year,weight_class) %>% 
  summarize(earned_points=sum(cumulative_prior_points),
            career_points=sum(career_team_points),
            aa_finishes=sum(cumulative_prior_aa,na.rm=T),
            career_aa_finishes=sum(career_aa,na.rm=T),
            individual_aa=sum(prior_aa_logical),
            individiaul_aa_career=sum(career_aa_logical),
            champ_finishes=sum(cumulative_prior_titles,na.rm=T),
            career_champ_finishes=sum(career_titles,na.rm=T),
            individual_champs=sum(prior_champ_logical),
            individual_champs_career=sum(career_champ_logical),
            finalist_finishes=sum(cumulative_prior_finalists,na.rm=T),
            individual_finalists=sum(prior_finalists_logical))

weight_search <- wrestlers_master3 %>% 
  filter(weight_class==158,
         year==1994) %>% 
  select(weight_class,year,wrestler,team,team_points,placement,
         cumulative_prior_points,cumulative_prior_aa,
         prior_aa_logical,prior_champ_logical,cumulative_prior_titles,
         cumulative_prior_finalists,prior_aa_logical,career_team_points,
         career_aa,career_titles)

# career filters? Team, Number of Titles, Number of AA

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

# make a vector of team
# choices arranged alphabetically

team_choices <- ind_years_formatted %>% 
  distinct(Team) %>% 
  arrange(Team) %>% 
  pull(Team)


team_results_annual <- wrestlers_master %>% 
  group_by(team,year) %>% 
  summarize(score=sum(team_points,na.rm=T),
            qualifiers=n(),
            champs=sum(placement=="First",na.rm=T),
            finalists=sum(placement %in% c("First","Second"),na.rm=T),
            aa=sum(!is.na(placement)),
            bonus_points=sum(bonus_points,na.rm=T))

iowa <- team_results_annual %>% 
  filter(team=="Iowa")

# build user interface

ui <- page_navbar(
  
  
  title="NCAA Wrestling Tournament Results",
  
  theme=bs_theme(preset="cerulean"),
  
  id="nav",
  
  sidebar=sidebar(width=500,
                  
                  conditionalPanel("input.nav==`Individual Season Data`",
                  
                  accordion(
                    
                    accordion_panel(
                      
                      "Explore Data",
                      
                      sliderInput(inputId = "ind_dates",
                                  label="Choose a range of years",
                                  min=min(wrestlers_master$year),
                                  max=max(wrestlers_master$year),
                                  value=c(1980,max(wrestlers_master$year)),
                                  sep=""),
                      

                      pickerInput(inputId = "team_filter",
                                  label="Filter by Team",
                                  choices=team_choices,
                                  selected=team_choices,
                                  multiple=TRUE,
                                  options=list(
                                    `actions-box` = TRUE,
                                    `live-search` = TRUE
                                                )),

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
                                  choices=levels(ind_years_formatted$Seed),
                                  selected=levels(ind_years_formatted$Seed),
                                  multiple=TRUE,
                                  options=list(
                                    
                                    `actions-box` = TRUE
                                    
                                  ))
                      
                       
                    )
                    
                    )),
                  
                  conditionalPanel(
                    
                    "input.nav==`Individual Career Data`",
                    
                    accordion(
                      
                      accordion_panel(
          
                        "Filter Careers",
                        
                        sliderInput(inputId = "career_dates",
                                    label="Choose a range of years",
                                    min=min(wrestlers_master$year),
                                    max=max(wrestlers_master$year),
                                    value=c(1980,max(careers_summary2$career_end)),
                                    sep=""),
                        
                        pickerInput(inputId = "career_team_filter",
                                    label="Filter by Team",
                                    choices=team_choices,
                                    selected=team_choices,
                                    multiple=TRUE,
                                    options=list(
                                      `actions-box` = TRUE,
                                      `live-search` = TRUE
                                    ))
                        
                        
                        
                      )
                      
                      
                    )
                    
                    
                    
                  )
                  
                  ),
  
  nav_panel("Individual Season Data",

            page_fillable(
              
              layout_columns(
                
                col_widths=c(6,6),
                      
                      card(
                        
                        card_header("Individual Tournament Summaries"),
                        DTOutput("ind_tourneys_table"),
                        full_screen = TRUE
                        
                      ),
                      
                      card(
                        
                        card_header("Individual Tournament Matches by Selection"),
                        DTOutput("ind_matches_table"),
                        full_screen = TRUE
                        
                      )
              )
                      
                    )),
  
  nav_panel("Individual Career Data",
            
            page_fillable(
              
              
              
              card(
                
                card_header("Career Summaries"),
                            DTOutput("careers_table"),
                            full_screen = TRUE)
                
             
              
              
            )
            )
                    
                  
                  
                  
  
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
             Seed %in% input$seed_filter) %>% 
      filter(Team %in% input$team_filter) %>% 
      arrange(desc(`Team Points`)) 
    
    
  })
  

  
  # render a data table based on filters
  
  output$ind_tourneys_table <- renderDT({
    
    dat <- ind_tourneys_reactive() %>% 
      select(Name,Team,Weight,Seed,Year,Placement,`Team Points`,`Bonus Percent`)
    
    datatable(
      dat,
      selection="single",
      options=list(pageLength=25))
    
  })
  
  # filter individual matches based on a selected individual
  # from the individual tourneys table, first create
  # a reactive object of the selected row (right now
  # only allowing a single selection)
  
  tourneys_reactive <- reactive({
    
    tourney_dat <- ind_tourneys_reactive()
    selected_tourneys <- input$ind_tourneys_table_rows_selected 
    
    dat <-   tourney_dat[selected_tourneys,]

    
  })
  
  # now filter the data reactively
  
  matches_reactive <- reactive({
    
    req(tourneys_reactive())
    
    dat <- tourneys_reactive()

    output <- matches_master %>%
      filter(winner_wrestler_id %in% dat$wrestler_id|loser_wrestler_id%in% dat$wrestler_id,
             year %in% dat$Year,
             weight_class %in% dat$Weight) %>%
      arrange(round)

  })
  
  # make the filtered matches for selected individual render
  # to a datatable object
  
  output$ind_matches_table <- renderDT({
    
    req(tourneys_reactive())
    req(matches_reactive())
    
    dat <- matches_reactive() %>% 
      mutate(Winner=str_c(winner_firstlast,winner_team,sep=" - "),
             Loser=str_c(loser_firstlast,loser_team,sep=" - "),
             Score=str_c(winner_match_points,loser_match_points,sep= "-")) %>% 
      select(Round=round,Weight=weight_class,
             Winner,Result=result,
             Loser,Score,
             `Termination Time`=termination_time,
             `Team Points Secured`=winner_team_points_secured)
    
    datatable(
      
     dat
      
    )
    
  })

  
  # make tcareers by wrestlers filter reactively
  
  careers_reactive <- reactive({
    
    req(input$career_dates)
    
    teams_selected <- input$career_team_filter
    team_pattern <- paste0("\\b(", paste0(teams_selected, collapse="|"), ")\\b")
    
    career_min <- min(input$career_dates)
    career_max <- max(input$career_dates)
    
    career.dat <- careers_formatted %>% 
      filter(career_start>=career_min,
            career_end<=career_max,
            str_detect(`Team(s)`, regex(team_pattern))
            ) %>% 
      select(-c(wrestler_id,career_start,career_end))
    
    
  })
  
  
  
  # render a data table based on filters
  
  output$careers_table <- renderDT({
    
    dat <- careers_reactive() %>% 
      arrange(desc(`Team Points`))
    
    datatable(dat,
              filter="top")
    
  })
  
}

shinyApp(ui,server)
