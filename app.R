#
# For next time: multiple pitchers
# For next time: how to account for 0 of something (ie Josh Hader hasn't hit a batter)
# Down the Road: Pitcher Heatmaps
#

library(shiny)
library(baseballr)
library(DT)
library(tidyverse)
library(shinycssloaders)
library(jsonlite)
library(janitor)



chadwick <- baseballr::chadwick_player_lu() %>%
        mutate(Name = paste(name_first,name_last)) %>%
        dplyr::filter(mlb_played_last == 2023)

# game_pks <- baseballr::mlb_game_pks(Sys.Date())
# 
# if (is.data.frame(game_pks)==F) {
#   game_pks <- tibble(game_pk=NULL,
#                      away_team=NULL,
#                      home_team=NULL,
#                      field=NULL)
# } else {
#   game_pks <- game_pks %>% dplyr::select(game_pk,away_team=teams.away.team.name,
#                 home_team=teams.home.team.name,field=venue.name)
# }
# 
# if (is.null(game_pks$game_pk)) {
#     gids <- "No Games Today"
#     } else {
#       
#     gids <- game_pks %>% pull(game_pk)
#     }

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
                              AB = stat_at_bats,
                              H=stat_hits,BB=stat_base_on_balls,
                              HBP=stat_hit_by_pitch,SO=stat_strike_outs,
                              HR=stat_home_runs,SF=stat_sac_flies) %>%
                dplyr::mutate(contact_outs = PA - H - BB - HBP - SO) %>%
                dplyr::group_by(PlayerName) %>% dplyr::summarise_all(.,sum) %>%
                dplyr::ungroup() %>% dplyr::mutate(p_h = 100*round(H/PA,3),
                                                   p_bb = 100*round(BB/PA,3),
                                                   p_hbp = 100*round(HBP/PA,3),
                                                   p_so = 100*round(SO/PA,3),
                                                   p_cout = 100*round(contact_outs/PA,3),
                                                   BABIP = round(H-HR)/(AB-SO-HR+SF),3)
        
        # This is acquiring the fangraphs ID for the batter that we want to use
        p_mlb_id <- chadwick %>% dplyr::filter(Name %in% pitcher) %>% dplyr::pull(key_mlbam)
        
        # This is acquiring the game level stats for the batter
        pitcher_stats <- mlb_game_logs(p_mlb_id,"pitching",2023) %>%
                dplyr::select(PlayerName=player_full_name,TBF=stat_batters_faced,
                              AB = stat_at_bats,
                              H=stat_hits,BB=stat_base_on_balls,
                              HBP=stat_hit_by_pitch,SO=stat_strike_outs,
                              HR=stat_home_runs,SF=stat_sac_flies) %>%
                dplyr::mutate(contact_outs = TBF - H - BB - HBP - SO) %>%
                dplyr::group_by(PlayerName) %>% dplyr::summarise_all(.,sum) %>%
                dplyr::ungroup() %>% dplyr::mutate(p_h = 100*round(H/TBF,3),
                                                   p_bb = 100*round(BB/TBF,3),
                                                   p_hbp = 100*round(HBP/TBF,3),
                                                   p_so = 100*round(SO/TBF,3),
                                                   p_cout = 100*round(contact_outs/TBF,3),
                                                   BABIP = round(H-HR)/(AB-SO-HR+SF),3,
                                                   val = p_cout*BABIP,
                                                   p_cout=p_cout-val,
                                                   p_h=p_h+val) %>% select(-val)
        
        ### Part 2 ####
        # This gets the probabilities of each event
        probs <- bind_rows(batter_stats %>% select(p_h,p_bb,p_hbp,p_so,p_cout,BABIP),
                           pitcher_stats %>% select(p_h,p_bb,p_hbp,p_so,p_cout,BABIP)) %>%
                mutate(matchup = "Batter v Pitcher") %>%
                group_by(matchup) %>% summarise_all(.,mean) %>%
                ungroup() %>% mutate(#val = p_cout*BABIP,
                        #p_cout = p_cout - val,
                        #p_h = p_h + val,
                        total = round(p_h+p_bb+p_hbp+p_so+p_cout)) %>%
                select(-BABIP)
        
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
        
        includeCSS("Styles.css"),
        # Application title
        titlePanel("MLB Player Matchup Simulator"),
        
        # Sidebar with a slider input for number of bins 
        fluidRow(
                column(4,
                       dateInput("game_date",label = "Select Date:",
                                 value = Sys.Date(), min = as.Date("2015-04-01"),
                                 max = Sys.Date()),
                       uiOutput("date_game_ids"),
                       #selectInput("game_ids","Select Game ID:",choices = gids),
                       tableOutput('games')),
                column(4,
                       tableOutput('p_pitchers')),
                column(4,
                       tableOutput('lineups'))),
        fluidRow(
                column(3,
                       selectInput("batter","Select Batter:", choices = sort(batters),
                                   multiple = T),
                       selectInput("pitcher","Select Pitcher:", choices = sort(pitchers),
                                   selected = "Framber Valdez"),
                       selectInput("heat_batter","Select SPECIFIC Batter for Heatmap",
                                   choices = sort(batters),
                                   selected = "Corey Seager"),
                       actionButton("submit","Submit")
                ),
                
                # Show a plot of the generated distribution
                column(9,
                       shinycssloaders::withSpinner(
                               DT::dataTableOutput("matchup_table"), color = "red")
                )
        ),
        fluidRow(
                column(6,
                       shinycssloaders::withSpinner(
                               plotOutput("batter_heatmap"), color = "red")),
                column(6,
                       shinycssloaders::withSpinner(
                               plotOutput("pitcher_heatmap"),color="red"))
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        output$date_game_ids <- renderUI({
                req(input$game_date, cancelOutput = T)
                
                game_pks <- baseballr::mlb_game_pks(input$game_date)
                
                if (is.data.frame(game_pks)==F) {
                        game_pks <- tibble(game_pk=NULL,
                                           away_team=NULL,
                                           home_team=NULL,
                                           field=NULL)
                } else {
                        game_pks <- game_pks %>% dplyr::select(game_pk,away_team=teams.away.team.name,
                                                               home_team=teams.home.team.name,field=venue.name)
                }
                
                if (is.null(game_pks$game_pk)) {
                        gids <- "No Games Today"
                } else {
                        
                        gids <- game_pks %>% pull(game_pk)
                }
                
                selectInput("game_ids","Select Game ID:",choices = gids)
        })
        
        output$matchup_table <- renderDataTable({
                
                input$submit
                
                df <- isolate(1:length(input$batter) %>% purrr::map_df(function(x) matchup_model(input$batter[x],input$pitcher,1000)))
                
                DT::datatable(df, class = 'cell-border stripe')
                
        })
        
        output$games <- renderTable({
                
                req(input$game_date, cancelOutput = T)
                
                game_pks <- baseballr::mlb_game_pks(input$game_date)
                
                if (is.data.frame(game_pks)==F) {
                        game_pks <- tibble(game_pk=NULL,
                                           away_team=NULL,
                                           home_team=NULL,
                                           field=NULL)
                } else {
                        game_pks <- game_pks %>% dplyr::select(game_pk,away_team=teams.away.team.name,
                                                               home_team=teams.home.team.name,field=venue.name)
                }
                
        })
        
        output$p_pitchers <- renderTable({
                
                tryCatch({
                        mlb_probables(input$game_ids) %>%
                                dplyr::select(Pitcher = fullName,team,home_plate_ump = home_plate_full_name)
                }, error=function(e) tibble(game = "No Games Today"))
        })
        
        output$lineups <- renderTable({
                
                tryCatch({
                        mlb_batting_orders(input$game_ids) %>% 
                                dplyr::select(Name = fullName, Pos = abbreviation,
                                              order = batting_order,team = teamName)
                }, error=function(e) tibble(game = "No Games Today"))
                
        })
        output$batter_heatmap <- renderPlot({
                
                input$submit
                
                b_mlb_id <- chadwick %>% dplyr::filter(Name %in% input$heat_batter) %>% dplyr::pull(key_mlbam)
                
                b_logs <- statcast_search(start_date = "2023-03-30", end_date = Sys.Date(),
                                          playerid = b_mlb_id,
                                          player_type =  "batter") %>%
                        dplyr::filter(!is.na(plate_x),!is.na(plate_z))
                
                stand <- unique(b_logs$stand)[1]
                
                isolate(ggplot(b_logs,aes(x=plate_x,y=plate_z)) +
                                stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = F) +
                                scale_fill_distiller(type = "div") +
                                lims(x = c(-2.5,2.5), y = c(0,6)) +
                                annotate("rect", xmin = -0.75, xmax = 0.75, ymin = 1.5, ymax = 3.25, fill = 'black', alpha = 0.1) +
                                annotate('rect', xmin = -1, xmax = 1, ymin = 1.25, ymax = 3.5, colour = 'black', fill = NA) +
                                annotate('segment', x = -0.75, xend = -0.75, y = 1.5, yend = 3.25) +
                                annotate('segment', x = -0.25, xend = -0.25, y = 1.5, yend = 3.25) +
                                annotate('segment', x = 0.25, xend = 0.25, y = 1.5, yend = 3.25) +
                                annotate('segment', x = 0.75, xend = 0.75, y = 1.5, yend = 3.25) +
                                annotate('segment', x = -0.75, xend = 0.75, y = 3.25, yend = 3.25) +
                                annotate('segment', x = -0.75, xend = 0.75, y = 2.665, yend = 2.665) +
                                annotate('segment', x = -0.75, xend = 0.75, y = 2.085, yend = 2.085) +
                                annotate('segment', x = -0.75, xend = 0.75, y = 1.5, yend = 1.5) +
                                #element_blank() +
                                #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
                                theme_classic() +
                                ggtitle(paste0(input$heat_batter," (",stand,"HB) Pitch Heatmap Against (Catcher's Perspective)")))
        })
        
        output$pitcher_heatmap <- renderPlot({
                
                input$submit
                
                p_mlb_id <- chadwick %>% dplyr::filter(Name %in% input$pitcher) %>% dplyr::pull(key_mlbam)
                
                p_logs <- statcast_search(start_date = "2023-03-30", end_date = Sys.Date(),
                                          playerid = p_mlb_id,
                                          player_type =  "pitcher") %>%
                        dplyr::filter(!is.na(plate_x),!is.na(plate_z))
                
                hand <- unique(p_logs$p_throws)
                
                ggplot(p_logs,aes(x=plate_x,y=plate_z)) +
                        stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = F) +
                        scale_fill_distiller(type = "div") +
                        lims(x = c(-2.5,2.5), y = c(0,6)) +
                        annotate("rect", xmin = -0.75, xmax = 0.75, ymin = 1.5, ymax = 3.25, fill = 'black', alpha = 0.1) +
                        annotate('rect', xmin = -1, xmax = 1, ymin = 1.25, ymax = 3.5, colour = 'black', fill = NA) +
                        annotate('segment', x = -0.75, xend = -0.75, y = 1.5, yend = 3.25) +
                        annotate('segment', x = -0.25, xend = -0.25, y = 1.5, yend = 3.25) +
                        annotate('segment', x = 0.25, xend = 0.25, y = 1.5, yend = 3.25) +
                        annotate('segment', x = 0.75, xend = 0.75, y = 1.5, yend = 3.25) +
                        annotate('segment', x = -0.75, xend = 0.75, y = 3.25, yend = 3.25) +
                        annotate('segment', x = -0.75, xend = 0.75, y = 2.665, yend = 2.665) +
                        annotate('segment', x = -0.75, xend = 0.75, y = 2.085, yend = 2.085) +
                        annotate('segment', x = -0.75, xend = 0.75, y = 1.5, yend = 1.5) +
                        #element_blank() +
                        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
                        theme_classic() +
                        ggtitle(paste0(input$pitcher," (",hand,"HP) Pitch Heatmap (Catcher's Perspective)")) +
                        facet_wrap(~pitch_name)
        })
        
        
}

# Run the application 
shinyApp(ui = ui, server = server)
