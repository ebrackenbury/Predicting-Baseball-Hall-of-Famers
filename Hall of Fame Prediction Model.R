library(Lahman)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(gridExtra)
library(caret)

voting <- Lahman::HallOfFame
people <- Lahman::People

str(voting)
voting$votedBy <- as.factor(voting$votedBy)

summary(voting$votedBy)
summary(voting$inducted)
summary(voting$category)

voting <- voting %>%
  filter(category == "Player")

voting <- voting %>%
  filter(votedBy == "BBWAA")

inducted_players <- voting %>%
  filter(inducted == "Y")

str(People)

people$name <- paste(people$nameFirst, people$nameLast)

people <- people %>%
  filter((playerID %in% voting$playerID))

fielding <- Lahman::Fielding

str(fielding)

fielding %>%
  group_by(playerID) %>%
  summarize(positions = n_distinct(POS)) %>%
  filter(positions > 1)

player_position <- Fielding %>%
  group_by(playerID, POS) %>%
  summarize(games = sum(G)) %>%
  slice(which.max(games)) %>%
  arrange(desc(games))

people <- merge(people, player_position[c("playerID", "POS")],
                by.x = "playerID")

names(people)[names(people) == "POS"] <- "position"

people$position <- as.factor(people$position)

summary(people$position)

fielders <- people %>%
  filter(position != "P")

batting <- Lahman::Batting
str(batting)

colSums(is.na(batting))

na_to_zero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

na_cols <- unique(which(is.na(batting), arr.ind = TRUE)[, 2])
print(na_cols)

batting[na_cols] <- lapply(batting[na_cols], na_to_zero)

colSums(is.na(batting))

batting_stats <- batting %>%
  group_by(playerID) %>%
  summarize(
    batting_seasons = length(unique(yearID)),
    games = sum(G),
    at_bats = sum(AB),
    runs = sum(R),
    hits = sum(H),
    singles = sum(H - (X2B + X3B + HR)),
    doubles = sum(X2B),
    triples = sum(X3B),
    home_runs = sum(HR),
    rbis = sum(RBI),
    stolen_bases = sum(SB),
    caught_stealing = sum(CS),
    walks = sum(BB),
    strikeouts = sum(SO),
    int_walks = sum(IBB),
    hbp = sum(HBP),
    sac_hit = sum(SH),
    sac_fly = sum(SF),
    gidp = sum(GIDP),
    plate_app = sum(AB + BB + SH + SF + HBP)
  )


batting_stats <- batting_stats %>%
  mutate(
    average = hits / at_bats,
    on_base = (hits + walks + hbp) / (at_bats + walks + hbp + sac_fly),
    slugging = (singles + 2 * doubles + 3 * triples + 4 * home_runs) /
      at_bats,
    ops = on_base + slugging
  )

fielders <- fielders[c("playerID", "name", "position")]

fielders <- merge(fielders, batting_stats, by.x = "playerID")

# Add Inducted
fielders <-
  merge(fielders,
        inducted_players[c("playerID", "inducted")],
        by.x = "playerID",
        all.x = TRUE)

fielders <- fielders %>%
  mutate(inducted = if_else(is.na(inducted), "N", "Y"))

fielders$inducted <- as.factor(fielders$inducted)

str(fielders)

player_decade <- batting %>%
  mutate(decade = yearID - (yearID %% 10)) %>%
  group_by(playerID, decade) %>%
  summarize(games = sum(G)) %>%
  filter(games == max(games))

fielders <-
  merge(fielders, player_decade[c("playerID", "decade")], by.x = "playerID")

awards <- Lahman::AwardsPlayers
str(awards)

unique(awards$awardID)

awards$one <- 1

awards <- awards %>%
  spread(awardID, one)

awards$lgID <- as.character(awards$lgID)

awards <- data.frame(lapply(awards, na_to_zero))

# SUM AWARDS
awards <- awards %>%
  group_by(playerID) %>%
  summarize(
    All_Star_MVP = sum(All.Star.Game.MVP),
    MVP = sum(Most.Valuable.Player),
    Gold_Glove = sum(Gold.Glove),
    Silver_Slugger = sum(Silver.Slugger),
    Triple_Crown = sum(Triple.Crown),
    World_Series_MVP = sum(World.Series.MVP)
  )

# CREATE AWARD FLAGS
awards <- awards %>%
  mutate(
    All_Star_MVP = if_else(All_Star_MVP > 0, 1, 0),
    MVP = if_else(MVP > 0, 1, 0),
    Gold_Glove = if_else(Gold_Glove > 0, 1, 0),
    Silver_Slugger = if_else(Silver_Slugger > 0, 1, 0),
    Triple_Crown = if_else(Triple_Crown > 0, 1, 0),
    World_Series_MVP = if_else(World_Series_MVP > 0, 1, 0)
  )

fielders <- merge(fielders, awards, by.x = "playerID")

# Set Colors
colnames(fielders)
colors <- c("Y" = "orange2",
            "N" = "blue2")

# Create Plot Function
plotfunc <- function(var1){
  hist <- ggplot(data = fielders, aes_string(x = var1, fill = "inducted")) +
    geom_histogram() +
    scale_fill_manual(values = colors)
  box <- ggplot(data = fielders, aes_string(x = "inducted", y = var1,
                                            fill = "inducted")) +
    geom_boxplot() +
    coord_flip() +
    scale_fill_manual(values = colors)
  grid.arrange(box, hist)
}

plotfunc("games")

fielders %>% 
  filter(inducted == "N" & games > 2750) %>% 
  select(name, games) %>% 
  arrange(games)

fielders <- fielders[fielders$playerID != "rosepe01",]

