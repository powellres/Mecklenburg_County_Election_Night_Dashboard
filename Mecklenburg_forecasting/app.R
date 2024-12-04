#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

## Importing Librarys
library(tidyverse)
library(plotly)
library(sf)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(ggmap)

register_google(key = "AIzaSyDknZmEvyeKTyGnZN0ff0-A1_N696Dq3lY", write = TRUE)

#### Reading in Data
precinct_maps <- st_read("Voter_Precinct/Voter_Precinct.shp")
precinct_maps <- st_transform(precinct_maps, 3857)
year_races <- read.csv("Yearly_races.csv")
clean_data <- read.csv("cleaned_past_data.csv")%>% select(-X)
precinct_turnout <- read.csv("turnout_by_precinct.csv") %>% select(-X) ### Add a function to auto-update 2024 turnout
race_options <- year_races$contest_title
google_map <- get_googlemap("Charlotte, United States", zoom = 10, maptype = "roadmap")

dials_by_precinct <- read.csv("Meck Dems (+ CC) 2024 Door + Phone Canvass Results - Attempts (1).csv")
dials_by_precinct <- dials_by_precinct %>% separate(Precinct, into = c("PCT_why", "precinct")) %>% select(-PCT_why)
dials_by_precinct <- dials_by_precinct %>% separate(Contact.Type, into = c("why", "contact_type_category")) %>% select(-why)
dials_by_precinct$precinct <- dials_by_precinct$precinct %>% as.numeric()
dials_by_precinct <- dials_by_precinct %>% select(c(contact_type_category, precinct, X..Contact.Attempts)) %>% 
        pivot_wider(names_from = contact_type_category, values_from = X..Contact.Attempts)
volunteer_sums <- dials_by_precinct %>% colSums() %>% t() %>% as.data.frame()
doors_total <- volunteer_sums$Doors
calls_total <- volunteer_sums$Calls

#### 2024 Data Pipeline (TO-DO)
download.file("https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2024_11_05/results_pct_20241105.zip", "data_2024.zip")
unzip("data_2024.zip", exdir = "Data/")
data_2024 <- read.delim("Data/results_pct_20241105.txt") %>% filter(County == "MECKLENBURG")
#clean_2024 <- read.csv("clean_2024 for fast render.csv") %>% select(-X)

## Cleaning 2024 data
clean_data_fresh <- function(data){
  check1 <- data %>% filter(`Real.Precinct` == "Y") %>% select(c("Precinct", "Contest.Name", "Choice", "Choice.Party", "Election.Day",  "Early.Voting", "Absentee.by.Mail", "Provisional", "Total.Votes"))
  colnames(check1) <- c("Precinct", "Contest_Name", "Choice", "Choice_Party", "Election_Day",  "Early_Voting", "Absentee_by_Mail", "Provisional", "Total_Votes")
  check2 <- check1 %>% mutate(in_person = Election_Day, Early = Early_Voting, mail = Absentee_by_Mail + Provisional) %>% select(-c(Election_Day, Early_Voting, Absentee_by_Mail, Provisional))
  unique_race_precinct <- check1 %>% select(c(Contest_Name,Precinct)) %>% unique()
  for(i in 1:nrow(unique_race_precinct)){
    race_val <- check2 %>% filter(Contest_Name == unique_race_precinct$Contest_Name[i] 
                                  & Precinct == unique_race_precinct$Precinct[i])
    if(nrow(race_val != 0)){
      race_val <- race_val[order(race_val$Total_Votes, decreasing = TRUE), ]
      if(race_val[1,5] == 0){
        winning_candidate <- "No Votes In"
        winning_party <- "None"
      } else{
        winning_candidate <- race_val[1,3]
        winning_party <- race_val[1,4]
      }
      race_row <- cbind(race_val[1,2], race_val[1,1], winning_candidate, winning_party, race_val[2,3:4], race_val[1,7], race_val[1,6], race_val[1,8], race_val[1,5], race_val[2,7], race_val[2,6], race_val[2,8], race_val[2,5], sum(race_val$Total_Votes))
      colnames(race_row) <- c("contest_title", "precinct", "winning_candidate", "winning_party", "trailing_candidate", "trailing_party", "winning_count_early", "winning_count_person", "winning_count_mail", "winning_count_total", "trailing_count_early", "trailing_count_person", "trailing_count_mail", "trailing_count_total", "total_count")
      if(i == 1){ ## This is ignoring 3 way races, I'll have to see about that
        check3 <- race_row
      } else{
        check3 <- rbind(check3, race_row)
      }
    }
  }
  
  check3$precinct <- check3$precinct %>% as.integer()
  check3 <- check3 %>% filter(is.na(contest_title) == FALSE)
  return(check3)
}

clean_2024 <- clean_data_fresh(data_2024)
clean_2024$Year <- 2024

clean_test_data <- rbind(clean_data, clean_2024)

max_vote_precinct_2024 <- clean_test_data %>% filter(Year == 2024 & contest_title != "MECKLENBURG COUNTY BOARD OF COMMISSIONERS AT-LARGE") %>% 
  group_by(precinct) %>% 
  summarise(max_votes_2024 = max(total_count)) %>% ungroup()

turnout_total_2024 <- sum(max_vote_precinct_2024$max_votes_2024)
turnout_total_2020 <- sum(precinct_turnout$turnout_2020)

precinct_turnout <- merge(precinct_turnout, max_vote_precinct_2024)

## Adding difference

data_2020_split <- clean_test_data %>% filter(Year == 2020)
data_2024_split <- clean_test_data %>% filter(Year == 2024)

data_2020_split <- data_2020_split %>% mutate(vote_diff_2020 = winning_count_total - trailing_count_total)
data_2020_split <- data_2020_split %>% mutate(vote_diff_score_2020 = ifelse(winning_party == "REP", vote_diff_2020 * -1, vote_diff_2020))
data_2020_split <- data_2020_split %>% mutate(vote_diff_percent_2020 = round(vote_diff_score_2020/total_count,digits = 4) *100) %>% select(c(contest_title, precinct, vote_diff_score_2020, vote_diff_2020, vote_diff_percent_2020))

data_2024_split <- data_2024_split %>% mutate(vote_diff_2024 = winning_count_total - trailing_count_total)
data_2024_split <- data_2024_split %>% mutate(vote_diff_score_2024 = ifelse(winning_party == "REP", vote_diff_2024 * -1, vote_diff_2024))
data_2024_split <- data_2024_split %>% mutate(vote_diff_percent_2024 = round(vote_diff_score_2024/total_count,digits = 4) *100) %>% select(c(contest_title, precinct, vote_diff_score_2024, vote_diff_2024, vote_diff_percent_2024))

data_diff <- merge(data_2020_split,data_2024_split, by = c("contest_title","precinct"))

data_diff <- data_diff %>% mutate(vote_diff_4year = vote_diff_2024 - vote_diff_2020,
                                  vote_diff_prct_4year = vote_diff_percent_2024 - vote_diff_percent_2020) %>%
  select(c(contest_title, precinct, vote_diff_4year, vote_diff_prct_4year))

clean_test_data <- merge(clean_test_data,data_diff, by = c("contest_title","precinct"))
clean_test_data <- merge(clean_test_data,dials_by_precinct, by = "precinct")

#### Function used to improve hover in 
fixer <- function(gp){
  lapply(1:length(gp$x$data), \(m) {
    gp$x$data[[m]]$hoveron <<- "fills" # hover on the fill, not line
    if(length(gp$x$data[[m]]$text > 1)) {
      gp$x$data[[m]]$text <<- gp$x$data[[m]]$text[1] # only one tooltip per county
    }
  }
  )
  gp
}

#### Function Used to get colors broken up correctly
breaks_func <- function(max_max, num_breaks = 7){
  diff <- max_max + max_max
  breaks_num <- diff/num_breaks
  breaks_prep <- c()
  for(i in 1:(num_breaks-1)){
    val <- -max_max + breaks_num * i
    breaks_prep <- c(breaks_prep,val)
  }
  return(breaks_prep)
}

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
map <- ggmap_bbox(google_map)

## Function used to plot the races
plot_race <- function(data, metric){
  
  race_map <- data
  if(nrow(race_map) > 0){
    
    metric_options <- c("vote_diff_score", "vote_diff_percent", 
                        "vote_diff_4year", "vote_diff_prct_4year",
                        "turnout_2020", "max_vote_2024", "turnout_diff",
                        "Calls", "Doors")

    if(metric == "Vote Share Amount"){i = 1}
    else if(metric == "Vote Share Percent"){i = 2}
    else if(metric == "2024 Vote Gain Amount"){i = 3}
    else if(metric == "2024 Vote Gain Percent"){i = 4}
    else if(metric == "2020 Turnout"){i = 5}
    else if(metric == "2024 Turnout"){i = 6}
    else if(metric == "Turnout Difference"){i = 7}
    else if(metric == "2024 Calls Made"){i = 8}
    else if(metric == "2024 Doors Knocked"){i = 9}
    
    race_map[is.na(race_map)] <- 0
    race_map <- race_map %>% mutate(vote_diff = winning_count_total - trailing_count_total)
    race_map <- race_map %>% mutate(vote_diff_score = ifelse(winning_party == "REP", vote_diff * -1, vote_diff))
    race_map <- race_map %>% mutate(vote_diff_percent = round(vote_diff_score/total_count,digits = 4) *100,
                                    turnout2_2020 = max_votes_2024 - turnout_2020,
                                    turnout_diff = max_votes_2024 - turnout_2020)
    race_map <- race_map %>% mutate(mytext = paste("Precinct: ", precinct, 
                                                   "\n", winning_candidate,  ": ", winning_count_total, " votes",
                                                   "\n", trailing_candidate, ": ", trailing_count_total, " votes",
                                                   "\nMargin: ", 
                                                   paste(ifelse(winning_party == "REP", "R+", "D+"),abs(round(vote_diff_score/total_count, digits = 4) * 100), "%", sep = ""),
                                                   " with ", paste(ifelse(winning_party == "REP", "R+", "D+"),abs(vote_diff), " votes more", sep = ""),
                                                   "\n", metric, ": ", get(metric_options[i]),
                                                   sep = ""))
    
    race_map2 <- merge(precinct_maps, race_map)
    
    min_vote_share <- race_map %>% select(metric_options[i]) %>% min()
    max_vote_share <- race_map %>% select(metric_options[i]) %>% max()
    max_max <- max(min_vote_share*-1, max_vote_share)
    
  
    plot <- ggmap(map) + 
      geom_sf(data = race_map2, aes(fill = ifelse(total_count == 0, NA,get(metric_options[i])), text = mytext),color = "white", size = .2, inherit.aes = FALSE, alpha = .6) +
      guides(fill = "none") + 
      theme_void() +
      scale_fill_stepsn(colors = brewer.pal(n = 11, name = "RdBu"),
                        breaks = breaks_func(max_max,15),
                        limits = c(-max_max,max_max))
    
  } else{
    nonrace_map <- data %>% select(precinct) %>% unique()
    rownames(nonrace_map) <- NULL
    nonrace_map <- nonrace_map %>% mutate(mytext = paste("Precinct:", precinct, "\nThis race did not occur in this Year", sep = " "))
    nonrace_map <- merge(precinct_maps, nonrace_map)
    
    plot <- ggplot() + 
      theme_void() +
      labs(title = "This Contest did not happen this Year")
  }
  

  ggplot_thing <- ggplotly(plot, tooltip = "text") %>% fixer()
  ggplot_thing <- ggplot_thing # %>% layout(hovermode = 'x unified', hoverlabel=list(bgcolor='rgba(255,255,255,0.75)', font=list(color='black')))
  return(ggplot_thing)
}

get_candidates <- function(data){
  race_inst <- data
  if(nrow(race_inst) > 0){
    candidate_1 <- race_inst$winning_candidate[1]
    candidate_2 <- race_inst$trailing_candidate[1]
    
    candidate_1_party <- race_inst$winning_party[1]
    candidate_2_party <- race_inst$trailing_party[1]
    
    ## Testing alternative
    summary1 <- race_inst %>% filter(winning_candidate == candidate_1) %>% dplyr::select(where(is.numeric)) %>% colSums() %>% t() %>% as.data.frame()
    summary1_names <- race_inst %>% filter(winning_candidate == candidate_1) %>% dplyr::select(!where(is.numeric))
    summary2 <- race_inst %>% filter(winning_candidate == candidate_2) %>% dplyr::select(where(is.numeric)) %>% colSums() %>% t() %>% as.data.frame()
    summary2_names <- race_inst %>% filter(winning_candidate == candidate_2) %>% dplyr::select(!where(is.numeric))
    
    candidate_1_block <- c(candidate_1, candidate_1_party,
                           summary1$winning_count_person + summary2$trailing_count_person,
                           summary1$winning_count_mail + summary2$trailing_count_mail,
                           summary1$winning_count_total + summary2$trailing_count_total,
                           summary1$vote_diff_4year + summary2$vote_diff_4year,
                           (summary1$vote_diff_prct_4year + summary2$vote_diff_prct_4year)/nrow(race_inst)
    ) %>% t() %>% as.data.frame()
    
    candidate_2_block <- c(candidate_2, candidate_2_party,
                           summary2$winning_count_person + summary1$trailing_count_person,
                           summary2$winning_count_mail + summary1$trailing_count_mail,
                           summary2$winning_count_total + summary1$trailing_count_total,
                           summary2$vote_diff_4year + summary1$vote_diff_4year,
                           (summary1$vote_diff_prct_4year + summary1$vote_diff_prct_4year)/nrow(race_inst)
    ) %>% t() %>% as.data.frame()
    block_advanced <- c("candidate", "party", "in_person_vote", "mail_vote", "total_vote", 
                        "vote_gain", "vote_gain_precent")
    colnames(candidate_1_block) <- block_advanced
    colnames(candidate_2_block) <- block_advanced
    
    candidates <- rbind(candidate_1_block, candidate_2_block)
  }
}

overall_summary <- function(data){
  race_inst <- data
  
  if(nrow(race_inst) > 0){
    candidates <- get_candidates(race_inst)
    
    if(race_inst$Year[1] == 2020){
      turnout_year <- turnout_total_2020
    } else{
      turnout_year <- turnout_total_2024
    }
    
    if(is.na(candidates$candidate[2])){
      
      if(candidates[1,2] == "DEM"){
        party_text <- '<font color = "blue">'
      } else{
        party_text <- '<font color = "red">'
      }
      
      return_values <- paste(candidates[1,1], " (", party_text , candidates[1,2], "</font>)",":", candidates[1,5], "\n",
                             "Turnout:", turnout_year)
                          
    } else{
    
      if(candidates[1,2] == "DEM"){
        party_text <- '<font color = "blue">'
        party_text2 <- '<font color = "red">'
      } else{
        party_text <- '<font color = "red">'
        party_text2 <- '<font color = "blue">'
      }

      if(candidates[1,2] == "DEM"){
        margin <- paste("<b>Margin: </b>", 'D+' ,round(as.numeric(candidates[1,5])/turnout_year, digits = 4)*100,
                        " with ", as.numeric(candidates[1,5]) - as.numeric(candidates[2,5]), " votes" ,sep = "")
      } else{
        margin <- as.character(paste("<b>Margin: </b>", 'D-' ,round(as.numeric(candidates[1,5])/turnout_year, digits = 4)*100,
                                     " with ", as.numeric(candidates[1,5]) - as.numeric(candidates[2,5]), " votes" ,sep = ""))
      }
      
      further_inst_2020 <- clean_test_data %>% filter(contest_title == race_inst$contest_title[1] &
                                                          Year == 2020)
      further_inst_2024 <- clean_test_data %>% filter(contest_title == race_inst$contest_title[1] &
                                                          Year == 2024)
      if(nrow(further_inst_2024) == 0 | nrow(further_inst_2020 == 0)) {
        margin_diff = "Not Applicable"
        margin_prct_diff = "Not Applicable"
      } else{
        candidates_2020 <- get_candidates(further_inst_2020)
        candidates_2024 <- get_candidates(further_inst_2024)
        
        margin_diff = sum(further_inst_2024$vote_diff_4year)
        margin_prct_diff <- round(candidates_2024[1,5]/turnout_total_2024)*100 - round(candidates_2020[1,5]/turnout_total_2020)*100
      }
      
      
      return_values <- paste("<b>", candidates[1,1],"</b>" ," (", party_text, candidates[1,2], "</font>)",": ", candidates[1,5], "<br>",
                             "<b>", candidates[2,1],"</b>"," (", party_text2, candidates[2,2], "</font>)",": ", candidates[2,5], "<br>",
                             margin, "<br>",
                             "<b>2020 Turnout:</b> ", turnout_total_2020, " | 2024 Turnout ", turnout_total_2024, "<br>",
                             "<b>Margin Difference:</b> ", margin_diff, " | <b>Margin Difference </b> ", margin_prct_diff, "%<br>",
                             "<b>Calls Made by Coordinated</b>: ", calls_total, "<br><b>Door Knocked by Coordinated:</b> ", doors_total,
                             sep = "")
    }
    
    
    return(return_values)
  } else{
    return("No Results for this Race")
  }
}
    
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(
      fluidRow(
        column(9, "Mecklenburg Election Night Forecasting"),
        #column(5, img(heigth = 50, width = 200, src = "https://drive.google.com/drive/folders/1xhgPZK4vWf1mhQXcxOIFaxEyMg2jtt7F")),
        #column(3, img(heigth = 50, width = 200, src = "https://static.wixstatic.com/media/730684_370b1a730906433f9464d7a28f3bd821~mv2.png/v1/fill/w_430,h_116,al_c,q_85,usm_0.66_1.00_0.01,enc_auto/730684_370b1a730906433f9464d7a28f3bd821~mv2.png"))
      )
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Select Campaign and Year"),
            selectInput("Race",
                        "Chose a Contest:",
                        choices = race_options),
            p("Note: These are all of the contests since 2020 and not all will be in every year."),
            selectInput("Year",
                        "Choose an Election Year",
                        choice = c(2024,2022,2020)),
            selectInput("Metric",
                        "Choose a value to color the map",
                        choice = c("Vote Share Percent", "Vote Share Amount", 
                                   "2024 Vote Gain Percent", "2024 Vote Gain Amount", 
                                   "2020 Turnout", "2024 Turnout", "Turnout Difference",
                                   "2024 Calls Made", "2024 Doors Knocked")),
            p("Note: colors on the map will always be blue for larger values and red for smaller numbers.\n
              Additionally, not all results will be in initially so "),
            h3("County Wide Results"),
            uiOutput("total_summary")

        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Precinct-level Results Map", 
                     plotlyOutput("racePlot",width = "60vw", height = "80vh")),
            tabPanel("Precinct-level Results Table", 
                     DTOutput("race_table"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    race_data <- reactive({
      round1 <- clean_test_data %>% filter(contest_title == input$Race & Year == input$Year)
      round2 <- left_join(round1, precinct_turnout)
      round2
    })
    
    metric <- reactive({
      input$Metric
    })
    
    output$selected_race <- renderText({
      paste("You have selected ", race())
    })
    
    output$race_table <- renderDT({
      detail_table <- race_data() 
      print(colnames(detail_table))
      colnames(detail_table) <- c("Precinct", "Contest", "#1 Candidate", "#1 Party", "#2 Candidate", "#2 Party", 
                                   "#1 Early Votes","#1 In-person Votes", "#1 Mail-in Votes", "#1 Total Votes",
                                   "#2 Early Votes","#2 In-person Votes", "#2 Mail-in Votes", "#2 Total Votes", "Total Count", "Year",
                                   "Vote Amount Difference", "Vote Percent Difference",
                                   "2024 Calls Made", "2024 Doors Knocked", "Other Contact",
                                   "2020 Turnout", "2022 Turnout", "2024 Turnout")
      detail_table <- detail_table[order(detail_table$Precinct),]
      #rownames(detail_table) <- FALSE
      detail_table
    })
  
    output$racePlot <- renderPlotly({
      race_map <- race_data()
      
      plot_race(race_map, metric())
    })
    
    output$total_summary <- renderUI({
      total_summary <- race_data()
      HTML(overall_summary(total_summary))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
