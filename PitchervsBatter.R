#
# For next time: multiple pitchers
# For next time: how to account for 0 of something (ie Josh Hader hasn't hit a batter)
# Down the Road: Pitcher Heatmaps
#

library(shiny)
library(baseballr)
library(DT)
library(tidyverse)
library(dplyr)

baseballr::get_chadwick_lu()
baseballr::chadwick_player_lu()

chadwick <- baseballr::chadwick_player_lu()

chadwick <- baseballr::chadwick_player_lu() %>%
        mutate(Name = paste(name_first,name_last)) %>%
        dplyr::filter(mlb_played_last == 2023)

game_pks <- baseballr::mlb_game_pks(Sys.Date()) %>%
        dplyr::select(game_pk,away_team=teams.away.team.name,
                      home_team=teams.home.team.name,field=venue.name)

gids <- game_pks %>% pull(game_pk)

mlb_game_logs <- function(id,stat_group,year) {
        
        url <- glue::glue("http://statsapi.mlb.com/api/v1/people/",id,"/stats?stats=gameLog,statSplits&group=",stat_group,"&season=",year,"&language=en")
        
        resp <- url %>% baseballr:::mlb_api_call()
        
        df <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)[[2]][[1]] 
        
        df <- df %>% 
                janitor::clean_names()  %>% 
                tibble()
        
        df
}

batters <- chadwick %>% dplyr::distinct(key_mlbam, .keep_all = T) %>%
        dplyr::pull(Name)
pitchers <- chadwick %>% dplyr::distinct(key_mlbam, .keep_all = T) %>%
        dplyr::pull(Name)

#sh <- mlb_game_logs(645261,"pitching",2023)

matchup_model <- function(batter,pitcher,n_sims) {
        
        set.seed(123)
        
        # This is acquiring the fangraphs ID for the batter that we want to use
        b_mlb_id <- chadwick %>% dplyr::filter(Name %in% batter) %>% dplyr::pull(key_mlbam)
        
        # This is acquiring the game level stats for the batter
        batter_stats <- mlb_game_logs(b_mlb_id,"hitting",2023) %>%
                dplyr::select(PlayerName=player_full_name,PA=stat_plate_appearances,
                              H=stat_hits,BB=stat_base_on_balls,
                              HBP=stat_hit_by_pitch,SO=stat_strike_outs) %>%
                dplyr::mutate(contact_outs = PA - H - BB - HBP - SO) %>%
                dplyr::group_by(PlayerName) %>% dplyr::summarise_all(.,sum) %>%
                dplyr::ungroup() %>% dplyr::mutate(p_h = 100*round(H/PA,3),
                                                   p_bb = 100*round(BB/PA,3),
                                                   p_hbp = 100*round(HBP/PA,3),
                                                   p_so = 100*round(SO/PA,3),
                                                   p_cout = 100*round(contact_outs/PA,3))
        
        # This is acquiring the fangraphs ID for the batter that we want to use
        p_mlb_id <- chadwick %>% dplyr::filter(Name %in% pitcher) %>% dplyr::pull(key_mlbam)
        
        # This is acquiring the game level stats for the batter
        pitcher_stats <- mlb_game_logs(p_mlb_id,"pitching",2023) %>%
                dplyr::select(PlayerName=player_full_name,TBF=stat_batters_faced,
                              H=stat_hits,BB=stat_base_on_balls,
                              HBP=stat_hit_by_pitch,SO=stat_strike_outs) %>%
                dplyr::mutate(contact_outs = TBF - H - BB - HBP - SO) %>%
                dplyr::group_by(PlayerName) %>% dplyr::summarise_all(.,sum) %>%
                dplyr::ungroup() %>% dplyr::mutate(p_h = 100*round(H/TBF,3),
                                                   p_bb = 100*round(BB/TBF,3),
                                                   p_hbp = 100*round(HBP/TBF,3),
                                                   p_so = 100*round(SO/TBF,3),
                                                   p_cout = 100*round(contact_outs/TBF,3))
        
        ### Part 2 ####
        # This gets the probabilities of each event
        probs <- bind_rows(batter_stats %>% select(p_h,p_bb,p_hbp,p_so,p_cout),
                           pitcher_stats %>% select(p_h,p_bb,p_hbp,p_so,p_cout)) %>%
                mutate(matchup = "Batter v Pitcher") %>%
                group_by(matchup) %>% summarise_all(.,mean) %>%
                ungroup() %>% mutate(total = round(p_h+p_bb+p_hbp+p_so+p_cout))
        
        # This runs the simulation of the batter versus pitcher matchup
        sims <- sample(x = c("Hit","Walk","Hit By Pitch","Strikeout","Contact Out"), size = n_sims, replace =T, prob = c(probs$p_h,
                                                                                                                         probs$p_bb,
                                                                                                                         probs$p_hbp,
                                                                                                                         probs$p_so,
                                                                                                                         probs$p_cout))
        
        # final_output <- tibble(result = sims) %>% dplyr::mutate(pa = n_sims,
        #                                                         hit = ifelse(result == "Hit",1,0),
        #                                                         out = ifelse(result == "Contact Out",1,0),
        #                                                         bb = ifelse(result == "Walk",1,0),
        #                                                         hbp = ifelse(result == "Hit By Pitch",1,0),
        #                                                         so = ifelse(result == "Strikeout",1,0),
        #                                                         s_hit = sum(hit),
        #                                                         s_out = sum(out),
        #                                                         s_bb = sum(bb),
        #                                                         s_hbp = sum(hbp),
        #                                                         s_so = sum(so),
        #                                                         avg = round(s_hit/(n_sims-s_bb-s_hbp),3),
        #                                                         bb_pct = round(s_bb/n_sims,3),
        #                                                         so_pct = round(s_so/n_sims,3),
        #                                                         hbp_pct = round(s_hbp/n_sims,3),
        #                                                         out_pct = 1 - avg) %>%
        #   dplyr::select(pa,avg,bb_pct,so_pct,hbp_pct,out_pct) %>% slice(1)
        
        # This spits out a batting average/walk rate/strikeout rate etc
        final_output <- tibble(result = sims) %>% group_by(result) %>% summarise(event = n()) %>%
                ungroup() %>% mutate(pct = round(event/sum(event),3)) %>% select(-event) %>% pivot_wider(names_from = result, values_from = pct) %>%
                dplyr::mutate(batter_name = batter, pitcher_name = pitcher) %>%
                dplyr::select(batter_name, pitcher_name,avg = `Hit`, bb_pct = `Walk`, so_pct = `Strikeout`, hbp_pct = `Hit By Pitch`,out_pct = `Contact Out`)
        
        
        
        return(final_output)
        
        
}
# Define UI for application that draws a histogram
ui <- fluidPage(
        
        includeCSS("Styles.css"),  # Include your custom CSS file

        # Application title
        titlePanel("MLB Player Matchup Simulator"),
        
        # Sidebar with a slider input for number of bins 
        fluidRow(
                column(4,
                       selectInput("game_ids","Select Game ID:",choices = gids),
                       tableOutput('games')),
                column(4,
                       tableOutput('p_pitchers')),
                column(4,
                       tableOutput('lineups'))),
        fluidRow(
                column(3,
                       selectInput("batter","Select Batter:", choices = sort(batters),
                                   multiple = T),
                       selectInput("pitcher","Select Pitcher:", choices = sort(pitchers)),
                       actionButton("submit","Submit")
                ),
                
                # Show a plot of the generated distribution
                column(9,
                       DT::dataTableOutput("matchup_table")
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        
        
        
        output$matchup_table <- renderDataTable({
                
                input$submit
                
                df <- isolate(1:length(input$batter) %>% purrr::map_df(function(x) matchup_model(input$batter[x],input$pitcher,1000)))
                
                df
                
        })
        
        output$games <- renderTable({
                
                game_pks
                
        })
        
        output$p_pitchers <- renderTable({
                
                mlb_probables(input$game_ids) %>%
                        dplyr::select(Pitcher = fullName,team,home_plate_ump = home_plate_full_name)
                
        })
        
        output$lineups <- renderTable({
                
                mlb_batting_orders(input$game_ids) %>% 
                        dplyr::select(Name = fullName, Pos = abbreviation,
                                      order = batting_order,team = teamName)
                
        })
        
        
}

# Run the application 
shinyApp(ui = ui, server = server)


