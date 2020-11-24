
# All Packages Necessary
library(ncaahoopR)
library(shiny)
library(tidyverse)
library(rsconnect)
library(rvest)
library(shinyWidgets)
library(ggpubr)
library(plotly)
library(retractcheck)
library(DT)

# shots not available 
# (2-5, 9-15, 17-21, 23,27, 28, 31)

# Data Needed. 
# Need to get atleast one game of play-by-play to have game_ids to match with input box. 
# Exchhange the 401168161 w/ one game id of your teams own.
cuse_pbp <- get_pbp_game(401168161, extra_parse = T)
cuse <- cuse_pbp %>% filter(shot_team == "Syracuse")
cuse1 <- cuse %>% mutate(x = (shot_x-25), y = (94-shot_y))

# All data needed for the sortable tables
# You can pull this from basketball-reference.com and paste into excel 
# Then export all as csv files to be read into this script
roster <- read_csv("CuseRoster1920.csv")
non <- read_csv("CuseNonSche1920.csv")
acc <- read_csv("CuseACCSche1920.csv")
pergame <- read_csv("CusePerGame1920.csv")
totals <- read_csv("CuseTotals1920.csv")
adv <- read_csv("CuseAdvanced1920.csv")
standings <- read_csv("ACC1920.csv")


# Court Function 
# creating court and plotting
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

court_themes = list(
  light = list(
    court = 'floralwhite',
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#000000"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  ),
  ppt = list(
    court = 'gray20',
    lines = 'white',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "gray20"
  )
)


plot_court = function(ggplot = ggplot(), court_theme = court_themes$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }
  
  court_points = data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  
  court_points <- court_points
  
  ggplot +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0,51), xlim = c(-51, 51)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = 'gray20', color = 'gray20'),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

# Team Shot Chart Function
team_chart <- function(game_ids, team) {
  df <- get_shot_locs(game_ids)
  df <- df %>% mutate(x = (25-x), y = (94-y))
  df <- df %>% filter(team_name %in% c(team, dict$ESPN_PBP[dict$ESPN == team]))
  df <- 
    
    p1 <-
    plot_court(ggplot(data = df), use_short_three = TRUE) + theme(legend.position = "none") + labs(title = "Syracuse Orange", subtitle = "Shot Chart") + theme(plot.title = element_text(colour = "darkorange",size = 22, hjust = .5, vjust = -2, face = "bold"), plot.subtitle = element_text(colour = "white", size = 15, hjust = .5, vjust = -4, face = "bold"),panel.background = element_rect(fill = "gray20"))  + annotate("text", x = -22 , y = 44, label = "Make", size= 3, colour = "darkorange", family = "Helvetica", hjust = .5, vjust = -.5, fontface="bold") + annotate("text", x = -22.2 , y = 42, label = "Miss", size= 3, colour = "grey", family = "Helvetica", hjust = .5, vjust = -.5, fontface="bold") + geom_point(mapping = aes(x=x,y=y, color = outcome, group = shooter)) + scale_color_manual(values = c("darkorange","grey"))
  
  p2 <- ggplotly(p1)
  
  return(p2)
}

# Opp Shot Chart
opp_shot_chart <- function(game_ids, team, heatmap = F) {
  df <- get_shot_locs(game_ids)
  df <- df %>% mutate(x = (x-25), y = (94-y))
  df <- df %>% filter(team_name %in% c(team, dict$ESPN_PBP[dict$ESPN == team]))
  
  p1 <-
    plot_court(ggplot(data = df, aes(x=x,y=y)), use_short_three = TRUE) + theme(legend.position = "none") + labs(title = "Syracuse Orangemen", subtitle = "Shot Chart Heatmap") + theme(plot.title = element_text(colour = "darkorange", size = 18, hjust = .5, vjust = -4, face = "bold"), plot.subtitle = element_text(colour = "white", size = 11, hjust = .5, vjust = -4, face = "bold"))  + annotate("text", x = -22 , y = 44, label = "Make", size= 2.5, colour = "darkorange", family = "Helvetica", hjust = .5, vjust = -1, fontface="bold") +
    annotate("text", x = -22.2 , y = 42, label = "Miss", size= 2.5, colour = "grey", family = "Helvetica", hjust = .5, vjust = -1, fontface="bold") + geom_point(mapping = aes(x=x,y=y, color = outcome)) + scale_color_manual(values = c("darkorange","grey"))
  return(p1)
}

# Circle Assist Function
circle_assist_net <- function(team, season, highlight_player = NA, highlight_color = NA,
                              three_weights = T, threshold = 0, message = NA, return_stats = T) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(is.na(season[1])) {
    stop("season is missing with no default")
  }
  if(is.na(highlight_color) & !is.na(highlight_player)) {
    warning("Please provide highlight color")
  }
  
  text_team <- dict$ESPN_PBP[dict$ESPN == team]
  text_team <- text_team[!is.na(text_team)]
  
  ### Warnings
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    warning("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
    return(NULL)
  }
  if(threshold < 0 | threshold > 1) {
    warning("Threshold for display must be between 0 and 1")
    return(NULL)
  }
  
  ### Read Play-by-Play File
  if(grepl("-", season[1])) {
    x <- suppressWarnings(try(get_pbp(team, season[1], extra_parse = F)))
    if(class(x) == "try-error" | class(x) == "NULL") {
      warning("Play-by-Play Data Not Available for Assist Network")
      return(NULL)
    }
    text <- paste0(" Assist Network\n", season[1], " Season")
    year <- season[1]
    factor <- 0.75
  }else {
    x <- suppressWarnings(try(get_pbp_game(season, extra_parse = F), silent = T))
    if(class(x) == "try-error" | class(x) == "NULL") {
      warning("Play-by-Play Data Not Available for Assist Network")
      return(NULL)
    }
    date <- get_date(season[1])
    year <- lubridate::year(date)
    if(lubridate::month(date) <= 5) {
      year <- paste(year - 1, year - 2000, sep = "-")
    } else {
      year <- paste(year, year - 1999, sep = "-")
    }
    
    opp <- setdiff(c(x$away, x$home), text_team)
    if(length(season) == 1 & is.na(message)){
      text <- paste(" Assist Network vs. ", opp, sep = "")
    } else{
      text <- message
    }
    factor <- 1.25
  }
  
  ### Override plot title
  if(!is.na(message)) {
    text <- message
  }
  
  
  ### Get Roster
  roster <- try(get_roster(team, year))
  if(class(roster)[1] == "try-error") {
    warning("Unable to get roster. ESPN is updating CBB files. Check back again soon")
    return(NULL)
  }
  roster$name <- gsub("Jr.", "Jr", roster$name)
  games <- unique(x$game_id)
  ast <- grep("Assisted", x$description)
  x <- x[ast, ]
  
  ### Get Ast/Shot from ESPN Play Description
  splitplay <- function(description) {
    tmp <- strsplit(strsplit(description, "Assisted")[[1]], " ")
    n1 <- grep("made", tmp[[1]])
    n1 <- n1[length(n1)]
    n2 <- length(tmp[[2]])
    tmp[[2]][n2] <- substring(tmp[[2]][n2], 1, nchar(tmp[[2]][n2]) - 1)
    shot_maker <- paste(tmp[[1]][1:(n1-1)], collapse = " ")
    assister <- paste(tmp[[2]][3:n2], collapse = " ")
    return(list("shot_maker" = shot_maker, "assister" = assister))
  }
  
  x <- mutate(x, "ast" = NA, "shot" = NA)
  for(i in 1:nrow(x)) {
    play <- splitplay(x$description[i])
    x$ast[i] <- play$assister
    x$shot[i] <- play$shot_maker
  }
  
  ### Get only shots made by the team in question
  x$ast <- gsub("Jr.", "Jr", x$ast)
  x$shot <- gsub("Jr.", "Jr", x$shot)
  x <- x[is.element(x$ast, roster$name), ]
  
  sets <- 2 * choose(nrow(roster), 2)
  network <- data.frame("ast" = rep(NA, sets),
                        "shot" = rep(NA, sets),
                        "num" = rep(NA, sets))
  
  ### Adjust Three Point Weights in Network
  x$weights <- 1
  if(three_weights){
    threes <- grep("Three Point", x$description)
    x$weights[threes] <- 1.5
  }
  
  ### Aggregate Assists
  for(i in 1:nrow(roster)) {
    ast <- roster$name[i]
    tmp <- roster[roster$name != ast,]
    for(j in 1:nrow(tmp)) {
      index <- j + (i - 1) * nrow(tmp)
      network$ast[index] <- ast
      network$shot[index] <- tmp$name[j]
      network$num[index] <- sum(x$weights[x$ast == ast & x$shot == tmp$name[j]])
    }
  }
  
  network$a_freq <- network$num/sum(network$num)
  network <- dplyr::filter(network, a_freq > 0)
  player_asts <-
    sapply(roster$name, function(name) { sum(network$a_freq[network$ast == name | network$shot == name]) })
  
  ### Team Ast/Shot Distributions
  ast_data <- aggregate(a_freq ~ ast, data = network, sum)
  shot_data <- aggregate(a_freq ~ shot, data = network, sum)
  
  ### Create Temporary Directed Network For Stat Aggregation
  net <- igraph::graph.data.frame(network, directed = T)
  deg <- igraph::degree(net, mode="all")
  igraph::E(net)$weight <- network$num
  
  ### Compute Clustering Coefficient
  clust_coeff <- round(igraph::transitivity(net, type = "global"), 3)
  
  ### Compute Page Rank
  pagerank <- sort(igraph::page_rank(net)$vector, decreasing = T)
  
  ### Compute Hub Score
  hubscores <- sort(igraph::hub_score(net, scale = F)$vector, decreasing = T)
  
  ### Compute Authority Scores
  auth_scores <- sort(igraph::authority_score(net, scale = F)$vector, decreasing = T)
  
  ### Compute Assist Frequency Data
  ast_freq <- ast_data$a_freq
  names(ast_freq) <- ast_data$ast
  
  ### Compute Shot Frequency Data
  shot_freq <- shot_data$a_freq
  names(shot_freq) <- shot_data$shot
  
  ### Create/Plot Undirected Network
  if(max(player_asts) < threshold) {
    warning("Threshold is too large--no players exceed threshold")
    ### Return Results
    return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
                "hub_scores" = hubscores, "auth_scores" = auth_scores,
                "ast_freq" = ast_freq, "shot_freq" = shot_freq))
  }
  keep <- names(player_asts)[player_asts > threshold]
  network <- dplyr::filter(network, shot %in% keep, ast %in% keep)
  
  
  if(any(season %in% c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21"))) {
    labs <- NA
  }
  else{
    labs <- as.character(network$num)
  }
  
  plot_title <-
    ifelse(is.na(message), paste0(text_team, ifelse(three_weights, " Weighted", ""), text), text)
  if(length(unique(x$game_id)) == 1 & is.na(message)) {
    plot_title <- paste(plot_title, format(as.Date(x$date[1]), "%B %d, %Y"), sep = "\n")
  }
  
  players <- dplyr::group_by(network, ast) %>%
    dplyr::summarise("count" = sum(num)) %>%
    dplyr::rename("player" = ast) %>%
    rbind(
      dplyr::group_by(network, shot) %>%
        dplyr::summarise("count" = sum(num)) %>%
        dplyr::rename("player" = shot)
    ) %>%
    dplyr::group_by(player) %>%
    dplyr::summarise("count" = sum(count)) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::pull(player)
  
  if(is.na(highlight_player)) {
    cols <- gg_color_hue(length(players))
    names(cols) <- players
    circlize::chordDiagram(network[,-4], 
                           order = players,
                           grid.col = cols,
                           annotationTrack = "grid",
                           preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(network))))))
  }else {
    cols <- rep("grey", length(players))
    if(!gsub("Jr.", "Jr", highlight_player) %in% players) {
      warning(paste("Selected highlight_player not in given network.",
                    "Please select a player from the following list"))
      return(sort(players))
    }
    cols[grepl(highlight_player, players)] <- highlight_color
    names(cols) <- players
    borders <- filter(network, ast == highlight_player) %>%
      select(ast, shot) %>%
      mutate(graphical = 1)
    circlize::chordDiagram(network[,-4], 
                           order = players,
                           grid.col = cols,
                           link.lwd = 2,
                           link.border = borders,
                           annotationTrack = "grid",
                           preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(network))))))
  }
  
  for(si in circlize::get.all.sector.index()) {
    circlize::circos.axis(h = "top", labels.cex = 0.3, sector.index = si, track.index = 2)
  }
  par(cex = 0.6)
  circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
    circlize::circos.text(circlize::CELL_META$xcenter, circlize::CELL_META$ylim[1], circlize::CELL_META$sector.index,
                          facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA)
  par(cex = 1)
  title(paste("\n\n", plot_title))
  
  ### Return Results
  if(return_stats) {
    return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
                "hub_scores" = hubscores, "auth_scores" = auth_scores,
                "ast_freq" = ast_freq, "shot_freq" = shot_freq))
  }
}
# Home Boxscore Function
homeboxscore <- function(game_id) {
  url <- paste0("https://www.espn.com/mens-college-basketball/boxscore?gameId=", game_id)
  webpage <- xml2::read_html(url)
  
  # Grab team names. Away team is always listed first.
  pagetext <- rvest::html_text(webpage)
  matchup <- unlist(strsplit(pagetext, " - "))[[1]][1]
  away_name <- unlist(strsplit(matchup, " vs. "))[1]
  away_name <- stringr::str_trim(away_name)
  home_name <- unlist(strsplit(matchup, " vs. "))[2]
  home_name <- stringr::str_trim(home_name)
  
  # General tidying and splitting of columns.
  away <- rvest::html_table(webpage)[[2]]
  away <- away[1:(nrow(away) - 1),]
  away <- away[-6,]
  away <- tidyr::separate(away, 'FG', c("FGM", "FGA"), sep = "-")
  away <- tidyr::separate(away, '3PT', c("3PTM", "3PTA"), sep = "-")
  away <- tidyr::separate(away, 'FT', c("FTM", "FTA"), sep = "-")
  away_totals <- away[nrow(away):nrow(away),]
  away_totals$Position <- NA
  away <- head(away, -1)
  away$Position <- substr(away$Starters, nchar(away$Starters), nchar(away$Starters))
  away$Starters <- substr(away$Starters, 0, (nchar(away$Starters)-1)/2)
  away <- rbind(away, away_totals)
  rownames(away) <- NULL
  colnames(away)[1] <- "player"
  colnames(away)[18] <- "position"
  away <- away[, c(1, 18, 2:(ncol(away)-1))]
  away$starter <- F
  away$starter[1:5] <- T
  
  home <- rvest::html_table(webpage)[[3]]
  home <- home[1:(nrow(home) - 1),]
  home <- home[-6,]
  home <- tidyr::separate(home, 'FG', c("FGM", "FGA"), sep = "-")
  home <- tidyr::separate(home, '3PT', c("3PTM", "3PTA"), sep = "-")
  home <- tidyr::separate(home, 'FT', c("FTM", "FTA"), sep = "-")
  home_totals <- home[nrow(home):nrow(home),]
  home_totals$Position <- NA
  home <- head(home, -1)
  home$Position <- substr(home$Starters, nchar(home$Starters), nchar(home$Starters))
  home$Starters <- substr(home$Starters, 0, (nchar(home$Starters)-1)/2)
  home <- rbind(home, home_totals)
  rownames(home) <- NULL
  colnames(home)[1] <- "Player"  
  colnames(home)[18] <- "Position"
  colnames(home)[2] <- "Min"
  colnames(home)[3] <- "FGM"
  colnames(home)[4] <- "FGA"
  colnames(home)[5] <- "3PM"
  colnames(home)[6] <- "3PA"
  colnames(home)[7] <- "FTM"
  colnames(home)[8] <- "FTA"
  colnames(home)[9] <- "ORB"
  colnames(home)[10] <- "DRB"
  colnames(home)[11] <- "TRB"
  colnames(home)[12] <- "AST"
  colnames(home)[13] <- "STL"
  colnames(home)[14] <- "BLK"
  colnames(home)[15] <- "TOV"
  colnames(home)[16] <- "PF"
  colnames(home)[17] <- "PTS"
  home <- home[, c(1, 18, 2:(ncol(home)-1))]
  home$starter <- F
  home$starter[1:5] <- T
  
  for(i in 3:18) {
    home[,i] <- as.numeric(home[,i])
    away[,i] <- as.numeric(away[,i])
  }
  
  results <- as.data.frame(home)
  
  
  p1 <-  DT::datatable(results, options = list(lengthMenu = c(5, 30, 50), pageLength = 20, width = 1))
  
  return(p1)
}

# Away Boxscore Function
awayboxscore <- function(game_id) {
  url <- paste0("https://www.espn.com/mens-college-basketball/boxscore?gameId=", game_id)
  webpage <- xml2::read_html(url)
  
  # Grab team names. Away team is always listed first.
  pagetext <- rvest::html_text(webpage)
  matchup <- unlist(strsplit(pagetext, " - "))[[1]][1]
  away_name <- unlist(strsplit(matchup, " vs. "))[1]
  away_name <- stringr::str_trim(away_name)
  home_name <- unlist(strsplit(matchup, " vs. "))[2]
  home_name <- stringr::str_trim(home_name)
  
  # General tidying and splitting of columns.
  away <- rvest::html_table(webpage)[[2]]
  away <- away[1:(nrow(away) - 1),]
  away <- away[-6,]
  away <- tidyr::separate(away, 'FG', c("FGM", "FGA"), sep = "-")
  away <- tidyr::separate(away, '3PT', c("3PTM", "3PTA"), sep = "-")
  away <- tidyr::separate(away, 'FT', c("FTM", "FTA"), sep = "-")
  away_totals <- away[nrow(away):nrow(away),]
  away_totals$Position <- NA
  away <- head(away, -1)
  away$Position <- substr(away$Starters, nchar(away$Starters), nchar(away$Starters))
  away$Starters <- substr(away$Starters, 0, (nchar(away$Starters)-1)/2)
  away <- rbind(away, away_totals)
  rownames(away) <- NULL
  colnames(away)[1] <- "player"
  colnames(away)[18] <- "position"
  away <- away[, c(1, 18, 2:(ncol(away)-1))]
  away$starter <- F
  away$starter[1:5] <- T
  
  home <- rvest::html_table(webpage)[[3]]
  home <- home[1:(nrow(home) - 1),]
  home <- home[-6,]
  home <- tidyr::separate(home, 'FG', c("FGM", "FGA"), sep = "-")
  home <- tidyr::separate(home, '3PT', c("3PTM", "3PTA"), sep = "-")
  home <- tidyr::separate(home, 'FT', c("FTM", "FTA"), sep = "-")
  home_totals <- home[nrow(home):nrow(home),]
  home_totals$Position <- NA
  home <- head(home, -1)
  home$Position <- substr(home$Starters, nchar(home$Starters), nchar(home$Starters))
  home$Starters <- substr(home$Starters, 0, (nchar(home$Starters)-1)/2)
  home <- rbind(home, home_totals)
  rownames(home) <- NULL
  colnames(home)[1] <- "Player"  
  colnames(home)[18] <- "Position"
  colnames(home)[2] <- "Min"
  colnames(home)[3] <- "FGM"
  colnames(home)[4] <- "FGA"
  colnames(home)[5] <- "3PM"
  colnames(home)[6] <- "3PA"
  colnames(home)[7] <- "FTM"
  colnames(home)[8] <- "FTA"
  colnames(home)[9] <- "ORB"
  colnames(home)[10] <- "DRB"
  colnames(home)[11] <- "TRB"
  colnames(home)[12] <- "AST"
  colnames(home)[13] <- "STL"
  colnames(home)[14] <- "BLK"
  colnames(home)[15] <- "TOV"
  colnames(home)[16] <- "PF"
  colnames(home)[17] <- "PTS"
  home <- home[, c(1, 18, 2:(ncol(home)-1))]
  home$starter <- F
  home$starter[1:5] <- T
  
  for(i in 3:18) {
    home[,i] <- as.numeric(home[,i])
    away[,i] <- as.numeric(away[,i])
  }
  
  results <- as.data.frame(away)
  
  
  p1 <- DT::datatable(results, options = list(lengthMenu = c(5, 30, 50), pageLength = 20, width = 1))
  
  return(p1)
}

# Game Flow Function
game_flow <- function(game_id, home_col, away_col) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }
  if(is.na(home_col)) {
    stop("home_col is missing with no default")
  }
  if(is.na(away_col)) {
    stop("away_col is missing with no default")
  }
  
  ### Get Data
  data <- get_pbp_game(game_id, extra_parse = F)
  if(is.null(data)) {
    warning("PBP Data Not Available for Game Flow Chart")
    return(NULL)
  }
  home_team <- data$home[1]
  away_team <- data$away[1]
  plot_lines <- 1200
  msec <- max(data$secs_remaining_absolute)
  sec <- msec - 2400
  ot_counter <- 0
  while(sec > 0) {
    sec <- sec - 300
    plot_lines <- c(plot_lines, 2400 + ot_counter * 300)
    ot_counter <- ot_counter + 1
  }
  date <- format(as.Date(data$date[1]), "%B %d, %Y")
  
  ### Get into Appropriate Format
  x <- rbind(
    dplyr::select(data, secs_remaining_absolute, home_score) %>%
      dplyr::mutate("score" = home_score, team = "home") %>%
      dplyr::select(-home_score),
    dplyr::select(data, secs_remaining_absolute, away_score) %>%
      dplyr::mutate("score" = away_score,
                    "team" = "away") %>%
      dplyr::select(-away_score)
  ) %>%
    dplyr::mutate("secs_elapsed" = max(secs_remaining_absolute) - secs_remaining_absolute)
  
  
  ### Message
  avg_sd <- round(sum(data$play_length * data$score_diff/max(data$secs_remaining_absolute)), 2)
  home_win <- data$home_score[nrow(data)] > data$away_score[nrow(data)]
  avg_sd <- ifelse(home_win, avg_sd, -avg_sd)
  avg_sd <- paste0("Average Score Differential for ",
                   ifelse(home_win, home_team, away_team), ": ", avg_sd)
  max_score <- max(c(data$home_score, data$away_score))
  
  ### Make Plot
  ggplot2::ggplot(x, aes(x = secs_elapsed/60, y = score, group = team, col = team)) +
    ggplot2::geom_step(size = 1.25) +
    ggplot2::theme(plot.background = element_rect(fill = "gray20"),
                   panel.background = element_rect(fill = "gray20"),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_line(colour = "gray40"),
                   axis.line = element_line(colour = "lightgrey"),
                   axis.ticks = element_line(colour = "white"),
                   axis.text = element_text(colour = "white")) +
    ggplot2::geom_vline(xintercept = plot_lines/60, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::labs(x = "Minutes Elapsed",
                  y = "Score",
                  col = "",
                  title = paste("Game Flow Chart for", home_team, "vs.", away_team),
                  subtitle = date,
                  caption = "Dominic Samangy (@DSamangy) | Inspired By ncaahoopR") +
    ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5, colour = "white", face = "bold"),
                   plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "white"),
                   axis.title = element_text(size = 14, colour = "white"),
                   legend.background = element_rect(fill = "lightgrey"),
                   legend.key = element_rect(fill = "lightgrey"),
                   plot.caption = element_text(size = 8, hjust = 0, colour = "white"),
                   legend.position = "bottom",) +
    ggplot2::scale_x_continuous(breaks = seq(0, msec/60, 5)) +
    ggplot2::scale_color_manual(values = c(away_col, home_col),
                                labels = c(away_team, home_team)) +
    ggplot2::annotate("text", x = 10, y = max_score - 10, label = avg_sd, color = "white")
}

# Win Probability Function
winprob_chart <- function(game_id, home_col, away_col, include_spread = T, show_labels = T) {
  ### Error Testing
  if(is.na(game_id)) {
    stop("game_id is missing with no default")
  }
  if(is.na(home_col)) {
    stop("home_col is missing with no default")
  }
  if(is.na(away_col)) {
    stop("away_col is missing with no default")
  }
  
  ### Get Data
  data <- get_pbp_game(game_id, extra_parse = F)
  if(is.null(data)) {
    warning("PBP Data Not Available for Win Probability Chart")
    return(NULL)
  }
  home_team <- data$home[1]
  away_team <- data$away[1]
  plot_lines <- 1200
  msec <- max(data$secs_remaining_absolute)
  sec <- msec - 2400
  ot_counter <- 0
  while(sec > 0) {
    sec <- sec - 300
    plot_lines <- c(plot_lines, 2400 + ot_counter * 300)
    ot_counter <- ot_counter + 1
  }
  date <- format(as.Date(data$date[1]), "%B %d, %Y")
  
  ### Naive WP if Spread Not Included
  if(!include_spread) {
    data$win_prob <- data$naive_win_prob
  }
  
  ### Get into Appropriate Format
  x <- rbind(
    dplyr::select(data, secs_remaining_absolute, win_prob) %>%
      dplyr::mutate(team = "home"),
    dplyr::select(data, secs_remaining_absolute, win_prob) %>%
      dplyr::mutate("win_prob" = 1 - win_prob,
                    team = "away")
  ) %>%
    dplyr::mutate("secs_elapsed" = max(secs_remaining_absolute) - secs_remaining_absolute)
  
  ### Game Excitement Index
  data$wp_delta <- 0
  for(i in 2:nrow(data)) {
    data$wp_delta[i] <- abs(data$win_prob[i] - data$win_prob[i-1])
  }
  gei <- sum(data$wp_delta, na.rm = T)
  gei <- paste("Game Excitement Index:", round(gei, 2))
  
  ### Minimum Win Probability
  if(data$score_diff[nrow(data)] > 0) {
    min_prob <- min(data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", home_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  } else {
    min_prob <- min(1 - data$win_prob)
    min_prob <- paste0("Minimum Win Probability for ", away_team, ": ",
                       ifelse(100 * min_prob < 1, "< 1%",
                              paste0(round(100 * min_prob), "%")))
  }
  
  home_score <- data$home_score[nrow(data)]
  away_score <- data$away_score[nrow(data)]
  st <- paste0(home_team, ": ", home_score, "  ", away_team, ": ", away_score, "\n", date)
  
  p <- ggplot2::ggplot(x, aes(x = secs_elapsed/60, y = win_prob, group = team, col = team)) +
    ggplot2::geom_line(size = 1.25) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(xintercept = plot_lines/60, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::geom_hline(yintercept = .5, lty = 2, alpha = 0.5, size = 0.8) +
    ggplot2::labs(x = "Minutes Elapsed",
                  y = "Win Probability",
                  col = "",
                  title = paste0(ifelse(include_spread, "", "Naive "), "Win Probability Chart for ", home_team,
                                 " vs. ", away_team),
                  subtitle = st, caption = "Dominic Samangy (@DSamangy) | Inspired By ncaahoopR") +
    ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5, colour = "white", face = "bold"),
                   plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", colour = "white"),
                   axis.title = element_text(size = 14, colour = "white"),
                   plot.caption = element_text(size = 8, hjust = 0, colour = "white"),
                   legend.position = "bottom",
                   legend.background = element_rect(fill = "lightgrey"),
                   legend.key = element_rect(fill = "lightgrey"),
                   plot.background = element_rect(fill = "gray20"),
                   panel.background = element_rect(fill = "gray20"),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_line(colour = "gray40"),
                   axis.line = element_line(colour = "lightgrey"),
                   axis.ticks = element_line(colour = "white"),
                   axis.text = element_text(colour = "white")) +
    ggplot2::scale_x_continuous(breaks = seq(0, msec/60, 5)) +
    ggplot2::scale_y_continuous(labels = function(x) {paste(100 * x, "%")}) +
    ggplot2::scale_color_manual(values = c(away_col, home_col),
                                labels = c(away_team, home_team))
  
  
  if(show_labels) {
    p <- p +
      ggplot2::annotate("text", x = 10, y = 0.05, label = min_prob, color = "white")
  }
  
  p
}

# Assist Function
assist_net <- function(team, node_col, season, three_weights = T, threshold = 0, message = NA, return_stats = T) {
  ### Error Testing
  if(is.na(team)) {
    stop("team is missing with no default")
  }
  if(is.na(season[1])) {
    stop("season is missing with no default")
  }
  if(is.na(node_col)) {
    stop("node_col is missing with no default")
  }
  
  text_team <- dict$ESPN_PBP[dict$ESPN == team]
  text_team <- text_team[!is.na(text_team)]
  
  ### Warnings
  if(!"ncaahoopR" %in% .packages()) {
    ids <- create_ids_df()
  }
  if(!team %in% ids$team) {
    warning("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
    return(NULL)
  }
  if(threshold < 0 | threshold > 1) {
    warning("Threshold for display must be between 0 and 1")
    return(NULL)
  }
  
  ### Read Play-by-Play File
  if(grepl("-", season[1])) {
    x <- suppressWarnings(try(get_pbp(team, season[1], extra_parse = F)))
    if(class(x) == "try-error" | class(x) == "NULL") {
      warning("Play-by-Play Data Not Available for Assist Network")
      return(NULL)
    }
    text <- paste(" Assist Network for", season[1], "Season")
    year <- season[1]
    factor <- 0.75
  }else {
    x <- suppressWarnings(try(get_pbp_game(season, extra_parse = F), silent = T))
    if(class(x) == "try-error" | class(x) == "NULL") {
      warning("Play-by-Play Data Not Available for Assist Network")
      return(NULL)
    }
    date <- get_date(season[1])
    year <- lubridate::year(date)
    if(lubridate::month(date) <= 5) {
      year <- paste(year - 1, year - 2000, sep = "-")
    } else {
      year <- paste(year, year - 1999, sep = "-")
    }
    
    opp <- setdiff(c(x$away, x$home), text_team)
    if(length(season) == 1 & is.na(message)){
      text <- paste(" Assist Network vs. ", opp, sep = "")
    } else{
      text <- message
    }
    factor <- 1.25
  }
  
  ### Override plot title
  if(!is.na(message)) {
    text <- message
  }
  
  ### Get Roster
  roster <- try(get_roster(team, year))
  if(class(roster)[1] == "try-error") {
    warning("Unable to get roster. ESPN is updating CBB files. Check back again soon")
    return(NULL)
  }
  roster$name <- gsub("Jr.", "Jr", roster$name)
  games <- unique(x$game_id)
  ast <- grep("Assisted", x$description)
  x <- x[ast, ]
  
  ### Get Ast/Shot from ESPN Play Description
  splitplay <- function(description) {
    tmp <- strsplit(strsplit(description, "Assisted")[[1]], " ")
    n1 <- grep("made", tmp[[1]])
    n1 <- n1[length(n1)]
    n2 <- length(tmp[[2]])
    tmp[[2]][n2] <- substring(tmp[[2]][n2], 1, nchar(tmp[[2]][n2]) - 1)
    shot_maker <- paste(tmp[[1]][1:(n1-1)], collapse = " ")
    assister <- paste(tmp[[2]][3:n2], collapse = " ")
    return(list("shot_maker" = shot_maker, "assister" = assister))
  }
  
  x <- mutate(x, "ast" = NA, "shot" = NA)
  for(i in 1:nrow(x)) {
    play <- splitplay(x$description[i])
    x$ast[i] <- play$assister
    x$shot[i] <- play$shot_maker
  }
  
  ### Get only shots made by the team in question
  x$ast <- gsub("Jr.", "Jr", x$ast)
  x$shot <- gsub("Jr.", "Jr", x$shot)
  x <- x[is.element(x$ast, roster$name), ]
  
  sets <- 2 * choose(nrow(roster), 2)
  network <- data.frame("ast" = rep(NA, sets),
                        "shot" = rep(NA, sets),
                        "num" = rep(NA, sets))
  
  ### Adjust Three Point Weights in Network
  x$weights <- 1
  if(three_weights){
    threes <- grep("Three Point", x$description)
    x$weights[threes] <- 1.5
  }
  
  ### Aggregate Assists
  for(i in 1:nrow(roster)) {
    ast <- roster$name[i]
    tmp <- roster[roster$name != ast,]
    for(j in 1:nrow(tmp)) {
      index <- j + (i - 1) * nrow(tmp)
      network$ast[index] <- ast
      network$shot[index] <- tmp$name[j]
      network$num[index] <- sum(x$weights[x$ast == ast & x$shot == tmp$name[j]])
    }
  }
  
  network$a_freq <- network$num/sum(network$num)
  network <- dplyr::filter(network, a_freq > 0)
  player_asts <-
    sapply(roster$name, function(name) { sum(network$a_freq[network$ast == name | network$shot == name]) })
  
  ### Team Ast/Shot Distributions
  ast_data <- aggregate(a_freq ~ ast, data = network, sum)
  shot_data <- aggregate(a_freq ~ shot, data = network, sum)
  
  ### Create Temporary Directed Network For Stat Aggregation
  net <- igraph::graph.data.frame(network, directed = T)
  deg <- igraph::degree(net, mode = "all")
  igraph::E(net)$weight <- network$num
  
  ### Compute Clustering Coefficient
  clust_coeff <- round(igraph::transitivity(net, type = "global"), 3)
  
  ### Compute Page Rank
  pagerank <- sort(igraph::page_rank(net)$vector, decreasing = T)
  
  ### Compute Hub Score
  hubscores <- sort(igraph::hub_score(net, scale = F)$vector, decreasing = T)
  
  ### Compute Authority Scores
  auth_scores <- sort(igraph::authority_score(net, scale = F)$vector, decreasing = T)
  
  ### Compute Assist Frequency Data
  ast_freq <- ast_data$a_freq
  names(ast_freq) <- ast_data$ast
  
  ### Compute Shot Frequency Data
  shot_freq <- shot_data$a_freq
  names(shot_freq) <- shot_data$shot
  
  ### Create/Plot Undirected Network
  if(max(player_asts) < threshold) {
    warning("Threshold is too large--no players exceed threshold")
    ### Return Results
    return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
                "hub_scores" = hubscores, "auth_scores" = auth_scores,
                "ast_freq" = ast_freq, "shot_freq" = shot_freq))
  }
  keep <- names(player_asts)[player_asts > threshold]
  network <- dplyr::filter(network, shot %in% keep, ast %in% keep)
  
  net <- igraph::graph.data.frame(network, directed = T)
  deg <- igraph::degree(net, mode="all")
  igraph::E(net)$weight <- network$num
  igraph::E(net)$arrow.size <- 0.7
  igraph::E(net)$edge.color <- "white"
  igraph::E(net)$width <- igraph::E(net)$weight * factor
  igraph::V(net)$color <- node_col
  
  if(any(season %in% c("2016-17", "2017-18", "2018-19"))) {
    labs <- NA
  }
  else{
    labs <- as.character(network$num)
  }
  
  
  title <-
    ifelse(is.na(message), paste0(text_team, ifelse(three_weights, " Weighted", ""), text), text)
  if(length(unique(x$game_id)) == 1 & is.na(message)) {
    title <- paste(title, format(as.Date(x$date[1]), "%B %d, %Y"), sep = "\n")
  }
  
  plot(net, vertex.label.color= "black", vertex.label.cex = 1,
       edge.curved = 0.3, edge.label = labs, edge.label.cex = 1.2,
       edge.label.color = "black",
       layout = igraph::layout_in_circle,
       vertex.label.family = "Arial Black")
  
  par(cex = 0.6)
  title(main = paste("\n", title))
  par(cex = 1)
  
  ### Add Text to Network
  text(-1.5, 1.0, paste(ifelse(three_weights, "Weighted ", ""), "Assist Frequency Leader: ",
                        ast_data$ast[which.max(ast_data$a_freq)], " (",
                        round(100 * max(ast_data$a_freq), 1), "%)", sep = ""),
       cex = 0.6)
  text(-1.5, 0.9, paste(ifelse(three_weights, "Weighted ", ""), "(Assisted) Shot Frequency Leader: ",
                        shot_data$shot[which.max(shot_data$a_freq)], " (",
                        round(100 * max(shot_data$a_freq), 1), "%)", sep = ""),
       cex = 0.6)
  text(-1.5, 0.8, paste("PageRank MVP: ", names(which.max(pagerank)), " (",
                        round(max(pagerank), 3), ")", sep = ""), cex = 0.6)
  text(-1.5, 0.7, paste("Hub Score MVP: ", names(which.max(hubscores)), " (",
                        round(max(hubscores), 3), ")", sep = ""), cex = 0.6)
  text(-1.5, 0.6, paste("Authority Score MVP: ", names(which.max(auth_scores)), " (",
                        round(max(auth_scores), 3), ")", sep = ""), cex = 0.6)
  text(-1.5, 0.5, paste("Team Clustering Coefficient: ", clust_coeff, sep = ""),
       cex = 0.6)
  
  if(three_weights){
    text(0, -1.4, cex = 0.7,
         paste("Weighted Assist Network: Assisted 2 point shots are given weight 1, ",
               "Assisted 3 point shots are given weight 1.5", sep = ""))
  }
  
  
  ### Return Results
  if(return_stats) {
    return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
                "hub_scores" = hubscores, "auth_scores" = auth_scores,
                "ast_freq" = ast_freq, "shot_freq" = shot_freq))
  }
}

# Creating the select function for the app

selectgame_id <- cuse1$game_id



  # Final code to create the shiny app
  # Appearance will need to be tweaked to fit othere teams
  # Under sidebar panel, all games will need to be changed to reflect games played and their respective game ids
  # These game ids can be found on espn.com
  # Team name woll need to be changed in functions towards and as well with colors

# Define UI for application 
ui <- fluidPage(
  
  #setBackgroundImage(
  #img(src = "small_s.png", height = 75, width = 75, align = "left")),
  
  titlePanel(title=div(img(src="cuse.png", height = 80, width =80, align = "right"))),
  
  h1(p(strong("Syracuse 2019-20 Season Dashboard"), align = "center", style = "color:darkorange2")),
  
  # --  Sidebar label with input and output definitions
  sidebarPanel(selectInput("selectgame_id", h1("select box"),
                           label = "Game",
                           choices = c("11-6 vs. Virginia" = 401168161,
                                       "11-3 vs. Colgate" = 401168180,
                                       "11-16 vs. Seattle" = 401168190,
                                       "11-20 vs. Cornell" = 401168202,
                                       "11-23 vs. Bucknell" = 401168211,
                                       "11-27 vs. Oklahoma St." = 401168223,
                                       "11-29 vs. Penn State" = 401182559,
                                       "12-3 vs. Iowa" = 401168238,
                                       "12-7 vs. Georgia Tech" = 401168533,
                                       "12-14 vs. Georgetown" = 401168248,
                                       "12-18 vs. Oakland" = 401168263,
                                       "12-21 vs. North Florida" = 401168272,
                                       "12-28 vs. Niagara" = 401168281,
                                       "1-4 vs. Notre Dame" = 401168534,
                                       "11-7 vs. Virginia Tech" = 401168535,
                                       "1-11 at Virginia" = 401168475,
                                       "1-15 vs. Boston College" = 401168536,
                                       "1-18 at Virginia Tech" = 401168537,
                                       "1-22 at Notre Dame" = 401168538,
                                       "1-25 vs. Pittsburgh" = 401168539,
                                       "1-28 at Clemson" = 401168540,
                                       "2-1 vs. Duke" = 401168362,
                                       "2-8 vs. Wake Forest" = 401168541,
                                       "2-11 vs. NC State" = 401168542,
                                       "2-15 at Florida State" = 401168527,
                                       "2-19 at Louisville" = 401168517,
                                       "2-22 vs. Georgia Tech" = 401168543,
                                       "2-26 at Pittsburgh" = 401168544,
                                       "2-29 vs. North Carolina" = 401168503,
                                       "3-3 vs. Boston College" = 401168545,
                                       "3-7 at Miami" = 401168546,
                                       "3-11 vs. North Carolina" = 401211590
                           )),
               h4("Shot Charts Available", size = 2, align = "center"),
               h5("11-6 vs. Virginia"),
               h5("11-27 vs. Oklahoma St."),
               h5("12-3 vs. Iowa"),
               h5("11-27 vs. Duke"),
               h5("2-11 vs. NC State"),
               h5("2-29 vs. North Carolina"),
               h5("3-11 vs. North Carolina (ACC)"),
               h5(),
               h5("All Games Are Available for Other Charts/Tables", size = 10, align = "center"),
               h6("Data Accesed From NCAAHoopR", align = "center"),
               h6("Assist Networks Currently Unailable", align = "center")
  )
  ,
  # -- Main panel for displaying outputs
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Shot Charts", plotlyOutput(outputId = "ShotChart")),
                tabPanel("Home Box Score", DT::dataTableOutput("homeboxscore")),
                tabPanel("Away Box Score", DT::dataTableOutput("awayboxscore")),
                tabPanel("Assist Networks", plotOutput("assist")),
                tabPanel("Game Flow Chart", plotOutput(outputId = "FlowChart")),
                tabPanel("Win Probability Chart", plotOutput("WinProbChart")),
                tabPanel("Per Game Player Stats", DT::dataTableOutput("pergame")),
                tabPanel("Total Player Stats", DT::dataTableOutput("totals")),
                tabPanel("Advanced Player Stats", DT::dataTableOutput("adv")),
                tabPanel("Roster", plotOutput("roster")),
                tabPanel("ACC Standings", plotOutput("standings")),
                tabPanel("ACC Schedule", plotOutput("accschedule")),
                tabPanel("Non-Conf. Schedule", plotOutput("nonschedule")),
                setBackgroundColor("lightgrey")
    )
  ))

server <- function(input, output) {
  # shot charts
  SelectedGameReactive <- reactive({
    cuse1 %>% filter(game_id == input$selectgame_id)
  })
  
  # Interactive shot chart  
  output$ShotChart <- renderPlotly({
    SelectedGameReactive() 
    team_chart(game_id = as.numeric(input$selectgame_id), "Syracuse")
  })
  
  output$homeboxscore <- DT::renderDataTable({
    SelectedGameReactive() 
    homeboxscore(game_id = as.numeric(input$selectgame_id))
  })
  
  output$awayboxscore <- DT::renderDataTable({
    SelectedGameReactive() 
    awayboxscore(game_id = as.numeric(input$selectgame_id))
  })
  
  output$assist <- renderPlot({
    SelectedGameReactive() 
    circle_assist_net(team = "Syracuse", season = as.numeric(input$selectgame_id))
  })
  
  output$FlowChart <- renderPlot({
    SelectedGameReactive() 
    game_flow(game_id = as.numeric(input$selectgame_id), "orange", "navy")
  })  
  
  output$WinProbChart <- renderPlot({
    SelectedGameReactive() 
    winprob_chart(game_id = as.numeric(input$selectgame_id), "orange", "navy")
  })  

  output$pergame <- DT::renderDataTable({
    DT::datatable(pergame, options = list(lengthMenu = c(5, 30, 50), pageLength = 20, width = 1))
  })
  
  output$totals <- DT::renderDataTable({
    DT::datatable(totals, options = list(lengthMenu = c(5, 30, 50), pageLength = 20, width = 1))
  })
  
  output$adv <- DT::renderDataTable({
    DT::datatable(adv, options = list(lengthMenu = c(5, 30, 50), pageLength = 20, width = 1))
  })
  
  output$roster <- renderPlot({
    ggtexttable(roster, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "darkorange", size = 13), tbody.style = tbody_style(color = "black", fill = c("grey", "grey"), size = 13))) + theme(panel.background = element_rect(fill = "grey"))
  })
  
  output$standings <- renderPlot({
    ggtexttable(standings, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "darkorange", size = 15), tbody.style = tbody_style(color = "black", fill = c("grey", "grey"), size = 15))) + theme(panel.background = element_rect(fill = "grey"))
  })
  
  output$accschedule <- renderPlot({
    ggtexttable(acc, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "darkorange", size = 9), tbody.style = tbody_style(color = "black", fill = c("grey", "grey"), size = 9))) + theme(panel.background = element_rect(fill = "grey"))
  })
  
  output$nonschedule <- renderPlot({
    ggtexttable(non, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "darkorange", size = 12), tbody.style = tbody_style(color = "black", fill = c("grey", "grey"), size = 12))) + theme(panel.background = element_rect(fill = "grey"))
  })
}

#game_id = as.numeric(input$selectgame_id)

# Run the application 
shinyApp(ui = ui, server = server)
