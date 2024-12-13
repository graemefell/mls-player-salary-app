# pull and clean MLS player salary data
rm(list=ls())
library(rvest)
library(tidyverse)
library(pdftools)

source("code/funs.R")

# get links where data is located ----------------------------------------------

links <- get_links()

#only 2022-2024 for now
patterns <- c("mlspa/Salary-Release-FALL-2024", "mlspa/2023", "mlspa/2022")
links <- links[grep(paste(patterns, collapse = "|"), links)]

# process each  ----------------------------------------------------------------

dta.24 <- load_main_data() %>%
  select(Club, `Last Name`, `First Name`, Position_long = `Position(s)`, `Base Salary`, 
         `Guaranteed Comp` = `Guaranteed Compensation`) %>%
  mutate(Name = paste(`First Name`, `Last Name`, sep = " ")) %>%
  mutate(Position = 
           case_when(
             Position_long %in% c("Goalkeeper") ~ "GK",
             Position_long %in% c("Right-back", "Left-back", "Center-back") ~ "D",
             Position_long %in% c("Defensive Midfield") ~ "D-M",
             Position_long %in% c("Right Midfield", "Left Midfield", "Central Midfield") ~ "M",
             Position_long %in% c("Attacking Midfield", "Right Wing", "Left Wing") ~ "M-F",
             Position_long %in% c("Center Forward") ~ "F",
             TRUE ~ "UNK"
           )) %>%
  mutate(Year = 2024)
  
#dta.list <- map(links, process_url) 
#test <- process_url("http://s3.amazonaws.com/mlspa/2023-Salary-Report-as-of-Sept-15-2023.pdf?mtime=20231018173909")

#downloaded data
dta.23 <- readxl::read_excel("input/salaries_2023.xlsx") %>%
  select(Club = Team, `Last Name`, `First Name`, Position, `Base Salary`, `Guaranteed Comp`) %>%
  mutate(Name = paste(`First Name`, `Last Name`, sep = " ")) %>%
  mutate(Year = 2023)

dta.20 <- readxl::read_excel("input/salaries_2020.xlsx")%>%
  mutate(Name = paste(`First Name`, `Last Name`, sep = " ")) %>%
  mutate(Position = ifelse(Position=="NA", "UNK", Position)) %>%
  mutate(Year = 2020)

dta.21 <- readxl::read_excel("input/salaries_2021.xlsx")%>%
  mutate(Club = 
           case_when(Club == "New England Revolutio"~ "New England Revolution",
                     TRUE ~ Club)) %>%
  mutate(`Last Name` = 
           case_when(Club == "New England Revolution"~ sub("n", "", `Last Name`),
                     TRUE ~ `Last Name`)) %>%
  mutate(Name = paste(`First Name`, `Last Name`, sep = " ")) %>%
  mutate(Position = ifelse(Position=="NA", "UNK", Position)) %>%
  mutate(Year = 2021)
  
dta.22 <- readxl::read_excel("input/salaries_2022.xlsx") %>%
  mutate(Club = 
           case_when(Club == "Colorado Rapid"~ "Colorado Rapids",
                     Club == "Houston Dynam"~ "Houston Dynamo",
                    is.na(Club) & grepl("Major League S", `Last Name`) ~ "Major League Soccer",
                    Club == "Minnesota Unit"~ "Minnesota United",
                    Club == "New England R"~ "New England Revolution",
                    Club == "New York City F"~ "New York City FC",
                    is.na(Club) & grepl("New York City F", `Last Name`) ~ "New York City FC",
                    Club == "New York Red"~ "New York Red Bulls",
                    is.na(Club) & grepl("Orlando City SC", `Last Name`) ~ "Orlando City SC",
                    Club == "Philadelphia Un"~ "Philadelphia Union",
                    Club == "Portland Timbe"~ "Portland Timbers",
                    Club %in% c("San Jose Earth","San Jose Earth quakes") ~ "San Jose Earthquakes",
                    Club == "Seattle Sounde" ~ "Seattle Sounders FC",
                    Club == "Sporting Kansa" ~ "Sporting Kansas City",
                    Club == "Vancouver Whit" ~ "Vancouver Whitecaps",
                    TRUE ~ Club)) %>%
  mutate(`Last Name` = 
           case_when(`Last Name` == "s"~ "Colorado Rapids",
                     `Last Name` == "o"~ "Houston Dynamo",
                     grepl("Major League S ", `Last Name`) ~ gsub("Major League S ", "", `Last Name`),
                     `Last Name` == "o"~ "Houston Dynamo",
                     Club == "Minnesota United"~ sub("e", "", `Last Name`),
                     grepl("New York City F ", `Last Name`) ~ gsub("New York City F ", "", `Last Name`),
                     Club == "New York Red Bulls"~ sub("B", "", `Last Name`),
                     grepl("Orlando City SC ", `Last Name`) ~ gsub("Orlando City SC ", "", `Last Name`),
                     Club == "San Jose Earthquakes" & !is.na(`Last Name`) ~ sub("q", "", `Last Name`),
                     Club == "Seattle Sounders FC"~ sub("r", "", `Last Name`),
                     TRUE ~ `Last Name`)) %>%
  mutate(Name = paste(`First Name`, `Last Name`, sep = " ")) %>%
  mutate(Position = ifelse(Position=="NA", "UNK", Position)) %>%
  mutate(Year = 2022)

dta.pre.20 <- data.table::fread("input/salaries_2013-2021.csv") %>% 
  mutate(Position = ifelse(Playing_Position =="NA", "UNK", Playing_Position),
         Year = as.numeric(format(Reporting_date, "%Y"))) %>%
  select(Club,
         `Last Name` = Last_name,
         `First Name` = First_name,
         Position,
         `Base Salary` = Base_salary,
         `Guaranteed Comp` = Guaranteed_Comp,
         Year) %>%
  mutate(Name = paste(`First Name`, `Last Name`, sep = " "))  %>%
  filter(!Year %in% c(2020,2021,2022,2023,2024))

# clean and append  ------------------------------------------------------------

dta.table <- bind_rows(dta.24, dta.23, dta.22, dta.21, dta.20, dta.pre.20) %>%
  select(Name, Club, Year, Position, `Base Salary`, `Guaranteed Comp`) %>%
  mutate(`Base Salary` = scales::dollar(`Base Salary`)) %>%
  mutate(`Guaranteed Comp` = scales::dollar(`Guaranteed Comp`)) %>%
  mutate(Year = as.character(Year)) %>% 
  mutate(Club = ifelse(Club %in% c("MLS Pool", "Major League Soccer", "Retired", ""),
                       "Unattached", Club)) %>%
  mutate(Club = ifelse(Club == "Montreal Impact","CF Montreal", Club)) %>%
  mutate(Club = ifelse(Club == "St. Louis SC","St. Louis City SCl", Club)) %>%
  mutate(Club = ifelse(Club == "NYCFC","New York City FC", Club)) %>%
  mutate(Club = ifelse(Club == "Montreal","CF Montreal", Club)) %>%
 mutate(Position = ifelse(is.na(Position), "UNK", Position)) 

dta.chart <- bind_rows(dta.24, dta.23, dta.22, dta.21,dta.20, dta.pre.20) %>%
  select(Name, Club, Year, Position, `Base Salary`, `Guaranteed Comp`) %>%
  mutate(Club = ifelse(Club %in% c("MLS Pool", "Major League Soccer", "Retired", ""),
                       "Unattached", Club)) %>%
  mutate(Club = ifelse(Club == "Montreal Impact","CF Montreal", Club)) %>%
  mutate(Club = ifelse(Club == "St. Louis SC","St. Louis City SC", Club)) %>%
  mutate(Club = ifelse(Club == "NYCFC","New York City FC", Club)) %>%
  mutate(Club = ifelse(Club == "Montreal","CF Montreal", Club)) %>%
 mutate(Position = ifelse(is.na(Position), "UNK", Position)) 


# save all  -------------------------------------------------------------------

saveRDS(dta.table, "data/salaries_table.rds")
saveRDS(dta.chart, "data/salaries_charts.rds")
