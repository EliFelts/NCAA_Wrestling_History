### Data are compiled, start building pieces and parts of shiny app

library(tidyverse)
library(googlesheets4)
library(bslib)
library(shiny)
library(plotly)


# right now reading in from google sheets
# because cleaning up a couple things still; will
# move these to feather to improve speed later

matches_master <- read_sheet("https://docs.google.com/spreadsheets/d/1yDlDRlShRcc_aWd_SmDuJ-5naNwhjIQHPVN4UVcpM24/edit?gid=1555277773#gid=1555277773")
wrestlers_master <- read_sheet("https://docs.google.com/spreadsheets/d/11TR6yUScjdF4OJYoqiVV4PqruJg2-KXmCCBljk9ABSw/edit?gid=325863048#gid=325863048")