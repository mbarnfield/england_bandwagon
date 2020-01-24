###### NEW!!!!!!!!!!!!!!



library(shiny)
library(tidyverse)
library(gmodels)
library(miceadds)
library(estimatr)
library(randomNames)
library(extraDistr)
library(skellam)

# load experiment data ----
experiment <- readRDS("data/experiment.rds")
profiles <- readRDS("data/profiles.rds")

# source helper functions -----
source("helpers.R")

# app -----------------------------------
ui <- fluidPage( # -----------------
                 titlePanel("Bandwagon effect conjoint analysis: England"),
                 tags$h4(
                   "A conjoint analysis experiment, simulating a United Kingdom general election as if held in an English constituency. Scroll down to see, download, and model data."
                 ),
                 br(),
                 br(),
                 tags$h2("Experiment"),
                 br(),
                 selectInput("preference", "Which party would you vote for in an election tomorrow?",
                             c("Labour", "Conservative", "Liberal Democrat", "Green Party")),
                 br(),
                 tags$h3("Candidates"),
                 br(),
                 tabPanel(
                   "Candidates",
                   tags$h5(
                     "Imagine a general election is being held, and you have to choose either to vote for one of the following candidates in your constituency, or not vote at all. Please consider the information available and indicate how you would like to vote using the buttons below each profile. NOTE: CANDIDATE NAMES CURRENTLY NOT INCLUDED."),
                   fluidRow(
                     column(width = 3, DT::dataTableOutput("cand1"),
                            align = "center"
                     ),
                     column(width = 3, DT::dataTableOutput("cand2"),
                            align = "center"
                     ),
                     column(width = 3, DT::dataTableOutput("cand3"),
                            align = "center"
                     ),
                     column(width = 3, DT::dataTableOutput("cand4"),
                            align = "center"
                     )
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(
                       width = 3,
                       actionButton(inputId = "one", label = "Vote for Candidate One"),
                       align = "center"
                     ),
                     column(
                       width = 3,
                       actionButton(inputId = "two", label = "Vote for Candidate Two"),
                       align = "center"
                     ),
                     column(
                       width = 3,
                       actionButton(inputId = "three", label = "Vote for Candidate Three"),
                       align = "center"
                     ),
                     column(
                       width = 3,
                       actionButton(inputId = "four", label = "Vote for Candidate Four"),
                       align = "center"
                     )
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(
                       width = 12,
                       actionButton(inputId = "abstain", label = "I would not turn out to vote"),
                       align = "center"
                     )
                   ),
                   br()
                   ),
                 br(),
                 tags$h2("Data"),
                 br(),
                 tags$h3("Your Choices"),
                 br(),
                 br(),
                 DT::dataTableOutput("choice"),
                 br(),
                 tags$h3("Download"),
                 radioButtons("filetype", "File type:",
                              choices = c("csv", "tsv")),
                 br(),
                 downloadButton('downloadData', 'Download'),
                 br(),
                 tags$h2("Model"),
                 br(),
                 tags$h3("Your AMCEs"),
                 tags$h5(
                   "Note: please make several choices before running model. It will crash with fewer than two choices made, and will not produce confidence intervals before a minimum of four choices are made (and may often require even more than that -- it depends how many features you have)."
                 ),
                 br(),
                 actionButton(inputId = "model", label = "Run model"),
                 br(),
                 br(),
                 plotOutput("amce_plot", height = 1200),
                 br(),
                 br(),
                 tags$h3("Switching AMCEs - indicates presence of bandwagon effects and strategic voting."),
                 tags$h5(
                   "Note: this model requires you to make a lot of choices, especially if you are rarely switching from your preferred party."
                 ),
                 br(),
                 actionButton(inputId = "switch_model", label = "Run model"),
                 br(),
                 br(),
                 plotOutput("switch_plot", height = 1200)
)

# ----------------------
server <- function(input, output) {
  
  amce_r <-
    reactiveValues(
      amces = data.frame(
        data.frame(
          outcome = 0,
          statistic = "amce",
          feature = "all",
          level = "all",
          estimate = 0,
          std.error = 0,
          t = 0,
          p = 0,
          lower = 0,
          upper = 0,
          r_squared = 0
        )),
      switches = data.frame(
        data.frame(
          outcome = 0,
          statistic = "amce",
          feature = "all",
          level = "all",
          estimate = 0,
          std.error = 0,
          t = 0,
          p = 0,
          lower = 0,
          upper = 0,
          r_squared = 0
        )))
  
  row_num_1 <- 1
  row_num_2 <- 2
  row_num_3 <- 3
  row_num_4 <- 4
  
  rv_shown <- reactiveValues(
    data1 = profiles[row_num_1,],
    data2 = profiles[row_num_2,],
    data3 = profiles[row_num_3,],
    data4 = profiles[row_num_4,]
  )
  
  rv_hidden <- reactiveValues(
    data1 = experiment[row_num_1,],
    data2 = experiment[row_num_2,],
    data3 = experiment[row_num_3,],
    data4 = experiment[row_num_4,]
  )
  
  col_no <- reactiveValues(col_no = sample(ncol(profiles[1, ])))
  
  choice <- reactiveValues(choice = data.frame(),
                           for_model = data.frame(),
                           for_switch_model = data.frame())
  
  
  observeEvent(input$one, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data1 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    party == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = constituency,
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = change,
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = constituency_change,
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = sn_emphasised,
                                  switched_to_local_emph = rv_hidden$data1[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data1[, "dl_emphasised"]
                                  )}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data1[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data1[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data1[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data1[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data1[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data1[, "dl_emphasised"])},
      {rv_hidden$data3 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 3,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data1[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data1[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data1[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data1[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data1[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data1[, "dl_emphasised"])},
      {rv_hidden$data4 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 4,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data1[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data1[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data1[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data1[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data1[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data1[, "dl_emphasised"])}) %>% 
        mutate(switched = sum(switched),
               switched_from = sum(switched_from),
               switched_from_local = sum(switched_from_local),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from_local_dynamic = sum(switched_from_local_dynamic)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    
    
    rv_shown$data1 <- profiles[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    
    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$two, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data2 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four+ input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    party == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = constituency,
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = change,
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = constituency_change,
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data2[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data2[, "dl_emphasised"])},
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data2[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data2[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data2[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data2[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data2[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data2[, "dl_emphasised"])}, 
      {rv_hidden$data3 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 3,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data2[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data2[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data2[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data2[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data2[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data2[, "dl_emphasised"])},
      {rv_hidden$data4 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 4,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data2[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data2[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data2[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data2[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data2[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data2[, "dl_emphasised"])}) %>% 
        mutate(switched = sum(switched),
               switched_from = sum(switched_from),
               switched_from_local = sum(switched_from_local),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from_local_dynamic = sum(switched_from_local_dynamic)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    
    rv_shown$data1 <- profiles[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    
    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$three, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data3 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 3,
                                  switched = case_when(
                                    party == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = constituency,
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = change,
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = constituency_change,
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data3[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data3[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data3[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data3[, "dl_emphasised"])
      },
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data3[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data3[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data3[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data3[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data3[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data3[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data3[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data3[, "dl_emphasised"])}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data3[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data3[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data3[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data3[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data3[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data3[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data3[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data3[, "dl_emphasised"])},
      {rv_hidden$data4 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 4,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data3[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data3[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data3[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data3[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data3[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data3[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data3[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data3[, "dl_emphasised"])}) %>% 
        mutate(switched = sum(switched),
               switched_from = sum(switched_from),
               switched_from_local = sum(switched_from_local),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from_local_dynamic = sum(switched_from_local_dynamic)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]

    
    rv_shown$data1 <- profiles[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]

    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$four, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data4 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 4,
                                  switched = case_when(
                                    party == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = constituency,
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = change,
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = constituency_change,
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data4[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data4[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data4[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data4[, "dl_emphasised"])},
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data4[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data4[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data4[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data4[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data4[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data4[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data4[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data4[, "dl_emphasised"])}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data4[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data4[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data4[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data4[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data4[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data4[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data4[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data4[, "dl_emphasised"])},
      {rv_hidden$data3 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 3,
                                  switched = case_when(
                                    party == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data4[, "polls"],
                                  switched_from = case_when(
                                    party == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data4[, "constituency"],
                                  switched_from_local = case_when(
                                    party == input$preference ~ constituency,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data4[, "change"],
                                  switched_from_dynamic = case_when(
                                    party == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data4[, "constituency_change"],
                                  switched_from_local_dynamic = case_when(
                                    party == input$preference ~ constituency_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data4[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data4[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data4[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data4[, "dl_emphasised"])}) %>% 
        mutate(switched = sum(switched),
               switched_from = sum(switched_from),
               switched_from_local = sum(switched_from_local),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from_local_dynamic = sum(switched_from_local_dynamic)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]

    
    rv_shown$data1 <- profiles[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]

    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  
  observeEvent(input$abstain, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 1,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_to_local = 0,
                                  switched_from = 0,
                                  switched_from_local = 0,
                                  switched_to_dynamic = 0,
                                  switched_to_local_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_from_local_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_local_emph = 0,
                                  switched_to_dynamic_emph = 0,
                                  switched_to_local_dynamic_emph = 0
      )}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 2,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_to_local = 0,
                                  switched_from = 0,
                                  switched_from_local = 0,
                                  switched_to_dynamic = 0,
                                  switched_to_local_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_from_local_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_local_emph = 0,
                                  switched_to_dynamic_emph = 0,
                                  switched_to_local_dynamic_emph = 0
      )},
      {rv_hidden$data3 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 3,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_to_local = 0,
                                  switched_from = 0,
                                  switched_from_local = 0,
                                  switched_to_dynamic = 0,
                                  switched_to_local_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_from_local_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_local_emph = 0,
                                  switched_to_dynamic_emph = 0,
                                  switched_to_local_dynamic_emph = 0
      )},
      {rv_hidden$data4 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$three + input$four + input$abstain,
                                  profile_no = 4,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_to_local = 0,
                                  switched_from = 0, 
                                  switched_from_local = 0,
                                  switched_to_dynamic = 0,
                                  switched_to_local_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_from_local_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_local_emph = 0,
                                  switched_to_dynamic_emph = 0,
                                  switched_to_local_dynamic_emph = 0
      )}) %>% 
        mutate(switched = sum(switched),
               switched_from = sum(switched_from),
               switched_from_local = sum(switched_from_local),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from_local_dynamic = sum(switched_from_local_dynamic)))
    
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data3 <- experiment[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_hidden$data4 <- experiment[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]

    
    rv_shown$data1 <- profiles[row_num_1 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data2 <- profiles[row_num_2 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data3 <- profiles[row_num_3 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]
    rv_shown$data4 <- profiles[row_num_4 + input$one*4 + input$two*4 + input$three*4 + input$four*4 + input$abstain*4,]

    col_no$col_no <- sample(ncol(profiles[1, ]))
  })
  
  observeEvent(input$model, {
    choice$for_model <- choice$choice %>%
      mutate(
        sn_emphasised = as_factor(case_when(
          sn_emphasised == 1 ~ "Static national emphasised",
          TRUE ~ "Static national not emphasised"
        )),
        dn_emphasised = as_factor(case_when(
          dn_emphasised == 1 ~ "Dynamic national emphasised",
          TRUE ~ "Dynamic national not emphasised"
        )),
        sl_emphasised = as_factor(case_when(
          sl_emphasised == 1 ~ "Static local emphasised",
          TRUE ~ "Static local not emphasised"
        )),
        dl_emphasised = as_factor(case_when(
          dl_emphasised == 1 ~ "Dynamic local emphasised",
          TRUE ~ "Dynamic local not emphasised"
        )),
        party = as_factor(party),
        polls = as_factor(paste0("National polls: ", polls, "%")),
        constituency = as_factor(paste0("Constituency polls: ", constituency, "%")),
        change = as_factor(paste0("Change in national polls: ", change)),
        constituency_change = as_factor(paste0("Change in constituency polls: ", constituency_change)),
        sn_interaction = interaction(polls, sn_emphasised),
        dn_interaction = interaction(change, dn_emphasised),
        sl_interaction = interaction(constituency, sl_emphasised),
        dl_interaction = interaction(constituency_change, dl_emphasised)
      ) %>%
      select(
        party,
        polls,
        constituency,
        change,
        constituency_change,
        sn_emphasised,
        dn_emphasised,
        sl_emphasised,
        dl_emphasised,
        sn_interaction,
        dn_interaction,
        sl_interaction,
        dl_interaction,
        chosen
      ) 

    choice$for_model[, "polls"] <- factor(
      choice$for_model[, "polls"],
      levels = c(
        "National polls: 20%",
        "National polls: 5%",
        "National polls: 10%",
        "National polls: 30%",
        "National polls: 40%"
      )
    )
    
    
    choice$for_model[, "change"] <- factor(
      choice$for_model[, "change"],
      levels = c(
        "Change in national polls: 0",
        "Change in national polls: -10",
        "Change in national polls: -7",
        "Change in national polls: -5",
        "Change in national polls: -2",
        "Change in national polls: 2",
        "Change in national polls: 5",
        "Change in national polls: 7",
        "Change in national polls: 10"
      )
    )
    
    choice$for_model[, "constituency_change"] <- factor(
      choice$for_model[, "constituency_change"],
      levels = c(
        "Change in constituency polls: 0",
        "Change in constituency polls: -10",
        "Change in constituency polls: -7",
        "Change in constituency polls: -5",
        "Change in constituency polls: -2",
        "Change in constituency polls: 2",
        "Change in constituency polls: 5",
        "Change in constituency polls: 7",
        "Change in constituency polls: 10"
      )
    )
    
    choice$for_model[, "constituency"] <- factor(
      choice$for_model[, "constituency"],
      levels = c(
        "Constituency polls: 20%",
        "Constituency polls: 5%",
        "Constituency polls: 10%",
        "Constituency polls: 30%",
        "Constituency polls: 40%",
        "Constituency polls: 60%"
      )
    )
    
    # run function on data
    amce_r$amces <- amce_rsq(choice$for_model,
                             chosen ~ party + polls + constituency + change + constituency_change + 
                               sn_interaction + dn_interaction + sl_interaction + dl_interaction
    )
  })
  
  observeEvent(input$switch_model, {
    choice$for_switch_model <- choice$choice %>%
      filter(voted == 1) %>%
      mutate(
        switched_to_emph = as_factor(case_when(
          switched_to_emph == 1 ~ "Switched-to party polling emphasised",
          TRUE ~ "Switched-to party polling not emphasised")),
        switched_to_local_emph = as_factor(case_when(
          switched_to_local_emph == 1 ~ "Switched-to party constituency polling emphasised",
          TRUE ~ "Switched-to party constituency polling not emphasised")),
        switched_to_dynamic_emph = as_factor(case_when(
          switched_to_dynamic_emph == 1 ~ "Switched-to party change in polling emphasised",
          TRUE ~ "Switched-to party change in polling not emphasised")),
        switched_to_local_dynamic_emph = as_factor(case_when(
          switched_to_local_dynamic_emph == 1 ~ "Switched-to party change in constituency polling emphasised",
          TRUE ~ "Switched-to party change in constituency polling not emphasised")),
        party = as_factor(party),
        switched_to = as_factor(paste0("Switched-to party polling: ", switched_to, "%")),
        switched_from = as_factor(paste0("Switched-from party polling: ", switched_from, "%")),
        switched_to_local = as_factor(paste0("Switched-to party constituency polling: ", switched_to_local, "%")),
        switched_from_local = as_factor(paste0("Switched-from party constituency polling: ", switched_from_local, "%")),
        switched_to_dynamic = as_factor(paste0("Switched-to party change in polling: ", switched_to_dynamic)),
        switched_from_dynamic = as_factor(paste0("Switched-from party change in polling: ", switched_from_dynamic)),
        switched_to_local_dynamic = as_factor(paste0("Switched-to party change in constituency polling: ", switched_to_local_dynamic)),
        switched_from_local_dynamic = as_factor(paste0("Switched-from party change in constituency polling: ", switched_from_local_dynamic)),
        sn_interaction = interaction(polls, sn_emphasised),
        dn_interaction = interaction(change, dn_emphasised),
        sl_interaction = interaction(constituency, sl_emphasised),
        dl_interaction = interaction(constituency_change, dl_emphasised)
        ) %>%
      select(switched_to,
             switched_from,
             switched_to_local,
             switched_from_local,
             switched_to_dynamic,
             switched_from_dynamic,
             switched_to_local_dynamic,
             switched_from_local_dynamic,
             switched_to_emph,
             switched_to_local_emph,
             switched_to_dynamic_emph,
             switched_to_local_dynamic_emph,
             switched) 
    
    choice$for_switch_model[, "switched_to"] <- factor(
      choice$for_switch_model[, "switched_to"],
      levels = c(
        "Switched-to party polling: 20%",
        "Switched-to party polling: 5%",
        "Switched-to party polling: 10%",
        "Switched-to party polling: 30%",
        "Switched-to party polling: 40%"
      )
    )
    
    choice$for_switch_model[, "switched_to_local"] <- factor(
      choice$for_switch_model[, "switched_to_local"],
      levels = c(
        "Switched-to party constituency polling: 20%",
        "Switched-to party constituency polling: 5%",
        "Switched-to party constituency polling: 10%",
        "Switched-to party constituency polling: 30%",
        "Switched-to party constituency polling: 40%",
        "Switched-to party constituency polling: 60%"
      )
    )
    
    choice$for_switch_model[, "switched_from"] <- factor(
      choice$for_switch_model[, "switched_from"],
      levels = c(
        "Switched-from party polling: 20%",
        "Switched-from party polling: 5%",
        "Switched-from party polling: 10%",
        "Switched-from party polling: 30%",
        "Switched-from party polling: 40%"
      )
    )
    
    choice$for_switch_model[, "switched_from_local"] <- factor(
      choice$for_switch_model[, "switched_from_local"],
      levels = c(
        "Switched-from party constituency polling: 20%",
        "Switched-from party constituency polling: 5%",
        "Switched-from party constituency polling: 10%",
        "Switched-from party constituency polling: 30%",
        "Switched-from party constituency polling: 40%",
        "Switched-from party constituency polling: 60%"
      )
    )
    
    choice$for_switch_model[, "switched_to_dynamic"] <- factor(
      choice$for_switch_model[, "switched_to_dynamic"],
      levels = c(
        "Switched-to party change in polling: 0",
        "Switched-to party change in polling: -10",
        "Switched-to party change in polling: -7",
        "Switched-to party change in polling: -5",
        "Switched-to party change in polling: -2",
        "Switched-to party change in polling: 2",
        "Switched-to party change in polling: 5",
        "Switched-to party change in polling: 7",
        "Switched-to party change in polling: 10"
      )
    )
    
    choice$for_switch_model[, "switched_from_dynamic"] <- factor(
      choice$for_switch_model[, "switched_from_dynamic"],
      levels = c(
        "Switched-from party change in polling: 0",
        "Switched-from party change in polling: -10",
        "Switched-from party change in polling: -7",
        "Switched-from party change in polling: -5",
        "Switched-from party change in polling: -2",
        "Switched-from party change in polling: 2",
        "Switched-from party change in polling: 5",
        "Switched-from party change in polling: 7",
        "Switched-from party change in polling: 10"
      )
    )
    
    choice$for_switch_model[, "switched_to_local_dynamic"] <- factor(
      choice$for_switch_model[, "switched_to_local_dynamic"],
      levels = c(
        "Switched-to party change in constituency polling: 0",
        "Switched-to party change in constituency polling: -10",
        "Switched-to party change in constituency polling: -7",
        "Switched-to party change in constituency polling: -5",
        "Switched-to party change in constituency polling: -2",
        "Switched-to party change in constituency polling: 2",
        "Switched-to party change in constituency polling: 5",
        "Switched-to party change in constituency polling: 7",
        "Switched-to party change in constituency polling: 10"
      )
    )
    
    choice$for_switch_model[, "switched_from_local_dynamic"] <- factor(
      choice$for_switch_model[, "switched_from_local_dynamic"],
      levels = c(
        "Switched-from party change in constituency polling: 0",
        "Switched-from party change in constituency polling: -10",
        "Switched-from party change in constituency polling: -7",
        "Switched-from party change in constituency polling: -5",
        "Switched-from party change in constituency polling: -2",
        "Switched-from party change in constituency polling: 2",
        "Switched-from party change in constituency polling: 5",
        "Switched-from party change in constituency polling: 7",
        "Switched-from party change in constituency polling: 10"
      )
    )
    
    
    
    
    
    

    
    
   
    # run function on data
    amce_r$switches <- amce_rsq(choice$for_switch_model,
                                switched ~ switched_to + switched_from +
                                  switched_to_local + switched_from_local +
                                  switched_to_dynamic + switched_from_dynamic +
                                  switched_to_local_dynamic + switched_from_local_dynamic +
                                  switched_to_emph + switched_to_dynamic_emph + 
                                  switched_to_local_emph + switched_to_local_dynamic_emph
    )
  })
  
  output$cand1 <- DT::renderDataTable({
    t(rv_shown$data1[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate One", ""), rownames = rep(""))
  
  output$cand2 <- DT::renderDataTable({
    t(rv_shown$data2[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate Two", ""), rownames = rep(""))
  
  output$cand3 <- DT::renderDataTable({
    t(rv_shown$data3[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate Three", ""), rownames = rep(""))
  
  output$cand4 <- DT::renderDataTable({
    t(rv_shown$data4[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate Four", ""), rownames = rep(""))
  
  output$choice <- DT::renderDataTable({
    choice$choice %>% arrange(desc(contest_no))
  })
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("choices", input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(choice$choice, file, sep = sep,
                  row.names = FALSE)
    })
  
  output$amce_plot <- renderPlot({
    ggplot(amce_r$amces, aes(estimate, level, colour = feature)) +
      geom_vline(linetype = "dashed", alpha = 0.5, xintercept = 0) +
      ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), na.rm = TRUE) +
      ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                              size = 0.2, height = 0, na.rm = TRUE,
                              position = ggstance::position_dodgev(height = 0.75)) +
      labs(x = "Change: Pr(Chosen Candidate)", y = "") +
      theme_minimal()
  })
  
  output$switch_plot <- renderPlot({
    ggplot(amce_r$switches, aes(estimate, level, colour = feature)) +
      geom_vline(linetype = "dashed", alpha = 0.5, xintercept = 0) +
      ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), na.rm = TRUE) +
      ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                              size = 0.2, height = 0, na.rm = TRUE,
                              position = ggstance::position_dodgev(height = 0.75)) +
      labs(x = "Change: Pr(Chosen Candidate)", y = "") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)


