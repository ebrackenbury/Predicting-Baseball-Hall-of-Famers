Hall of Fame Prediction Analysis
================
Evan Brackenbury
11/15/2021

## Data Cleaning and Wrangling

The Lahman packages contains many different tables of baseball data.
There are 2 tables we will start with.

``` r
voting <- Lahman::HallOfFame
people <- Lahman::People
```

First, let’s took a look at the structure of the voting table and some
variables of interest.

``` r
str(voting)
```

    ## 'data.frame':    4191 obs. of  9 variables:
    ##  $ playerID   : chr  "cobbty01" "ruthba01" "wagneho01" "mathech01" ...
    ##  $ yearID     : int  1936 1936 1936 1936 1936 1936 1936 1936 1936 1936 ...
    ##  $ votedBy    : chr  "BBWAA" "BBWAA" "BBWAA" "BBWAA" ...
    ##  $ ballots    : int  226 226 226 226 226 226 226 226 226 226 ...
    ##  $ needed     : int  170 170 170 170 170 170 170 170 170 170 ...
    ##  $ votes      : int  222 215 215 205 189 146 133 111 105 80 ...
    ##  $ inducted   : Factor w/ 2 levels "N","Y": 2 2 2 2 2 1 1 1 1 1 ...
    ##  $ category   : Factor w/ 4 levels "Manager","Pioneer/Executive",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ needed_note: chr  NA NA NA NA ...

``` r
voting$votedBy <- as.factor(voting$votedBy)

summary(voting$votedBy)
```

    ##            BBWAA       Centennial     Final Ballot     Negro League 
    ##             3756                6               21               26 
    ##  Nominating Vote       Old Timers          Run Off Special Election 
    ##               76               30               81                2 
    ##         Veterans 
    ##              193

``` r
summary(voting$inducted)
```

    ##    N    Y 
    ## 3868  323

``` r
summary(voting$category)
```

    ##           Manager Pioneer/Executive            Player            Umpire 
    ##                74                41              4066                10

“Inducted” will be the variable we are trying to predict. “Category” and
“votedBy” are relevant because we want to use statistics to build a
model. This means we will want to limit out data to players only. Next,
we will limit the data to include only players who were elected through
the Baseball Writers of America, as players who have been elected
through other avenues had non-statistical factors involved. Last, we can
create a separate data frame for inducted players only.

``` r
voting <- voting %>%
  filter(category == "Player")

voting <- voting %>%
  filter(votedBy == "BBWAA")

inducted_players <- voting %>%
  filter(inducted == "Y")
```

Next, let’s take a look at the “People” table:

``` r
str(People)
```

    ## 'data.frame':    20093 obs. of  26 variables:
    ##  $ playerID    : chr  "aardsda01" "aaronha01" "aaronto01" "aasedo01" ...
    ##  $ birthYear   : int  1981 1934 1939 1954 1972 1985 1850 1877 1869 1866 ...
    ##  $ birthMonth  : int  12 2 8 9 8 12 11 4 11 10 ...
    ##  $ birthDay    : int  27 5 5 8 25 17 4 15 11 14 ...
    ##  $ birthCountry: chr  "USA" "USA" "USA" "USA" ...
    ##  $ birthState  : chr  "CO" "AL" "AL" "CA" ...
    ##  $ birthCity   : chr  "Denver" "Mobile" "Mobile" "Orange" ...
    ##  $ deathYear   : int  NA 2021 1984 NA NA NA 1905 1957 1962 1926 ...
    ##  $ deathMonth  : int  NA 1 8 NA NA NA 5 1 6 4 ...
    ##  $ deathDay    : int  NA 22 16 NA NA NA 17 6 11 27 ...
    ##  $ deathCountry: chr  NA "USA" "USA" NA ...
    ##  $ deathState  : chr  NA "GA" "GA" NA ...
    ##  $ deathCity   : chr  NA "Atlanta" "Atlanta" NA ...
    ##  $ nameFirst   : chr  "David" "Hank" "Tommie" "Don" ...
    ##  $ nameLast    : chr  "Aardsma" "Aaron" "Aaron" "Aase" ...
    ##  $ nameGiven   : chr  "David Allan" "Henry Louis" "Tommie Lee" "Donald William" ...
    ##  $ weight      : int  215 180 190 190 184 235 192 170 175 169 ...
    ##  $ height      : int  75 72 75 75 73 74 72 71 71 68 ...
    ##  $ bats        : Factor w/ 3 levels "B","L","R": 3 3 3 3 2 2 3 3 3 2 ...
    ##  $ throws      : Factor w/ 3 levels "L","R","S": 2 2 2 2 1 1 2 2 2 1 ...
    ##  $ debut       : chr  "2004-04-06" "1954-04-13" "1962-04-10" "1977-07-26" ...
    ##  $ finalGame   : chr  "2015-08-23" "1976-10-03" "1971-09-26" "1990-10-03" ...
    ##  $ retroID     : chr  "aardd001" "aaroh101" "aarot101" "aased001" ...
    ##  $ bbrefID     : chr  "aardsda01" "aaronha01" "aaronto01" "aasedo01" ...
    ##  $ deathDate   : Date, format: NA "2021-01-22" ...
    ##  $ birthDate   : Date, format: "1981-12-27" "1934-02-05" ...

Since first and last name are stored in separate columns, we can create
a variable for “full name” which will help identify players easier.
We’ll also filter down the data set so it only includes the players that
have appeared on a hall of fame ballot.

``` r
people$name <- paste(people$nameFirst, people$nameLast)

people <- people %>% 
  filter((playerID %in% voting$playerID))
```

The “People” table doesn’t include position, which could be a variable
of interest. This is likely due to players playing multiple positions
over the course of their career. Luckily, we can find position in the
“Fielding” table.

``` r
fielding <- Lahman::Fielding

str(fielding)
```

    ## 'data.frame':    144768 obs. of  18 variables:
    ##  $ playerID: chr  "abercda01" "addybo01" "addybo01" "allisar01" ...
    ##  $ yearID  : int  1871 1871 1871 1871 1871 1871 1871 1871 1871 1871 ...
    ##  $ stint   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ teamID  : Factor w/ 149 levels "ALT","ANA","ARI",..: 136 111 111 39 39 142 111 111 111 111 ...
    ##  $ lgID    : Factor w/ 7 levels "AA","AL","FL",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ POS     : chr  "SS" "2B" "SS" "2B" ...
    ##  $ G       : int  1 22 3 2 29 27 1 2 20 5 ...
    ##  $ GS      : int  1 22 3 0 29 27 0 1 19 4 ...
    ##  $ InnOuts : int  24 606 96 18 729 681 15 30 555 93 ...
    ##  $ PO      : int  1 67 8 1 51 68 7 3 38 10 ...
    ##  $ A       : int  3 72 14 4 3 15 0 4 52 0 ...
    ##  $ E       : int  2 42 7 0 7 20 0 1 28 8 ...
    ##  $ DP      : int  0 5 0 0 1 4 0 0 2 0 ...
    ##  $ PB      : int  NA NA NA NA NA 18 NA NA NA 7 ...
    ##  $ WP      : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SB      : int  NA NA NA NA NA 0 NA NA NA 0 ...
    ##  $ CS      : int  NA NA NA NA NA 0 NA NA NA 0 ...
    ##  $ ZR      : int  NA NA NA NA NA NA NA NA NA NA ...

The table contains aggregated fielding statistics by year. Let’s see if
there are any players that have multiple positions in the data:

``` r
fielding %>% 
  group_by(playerID) %>% 
  summarize(positions = n_distinct(POS)) %>% 
  filter(positions > 1)
```

    ## # A tibble: 6,689 x 2
    ##    playerID  positions
    ##    <chr>         <int>
    ##  1 aaronha01         4
    ##  2 aaronto01         4
    ##  3 abadan01          2
    ##  4 abbated01         4
    ##  5 abbeych01         2
    ##  6 abbotfr01         2
    ##  7 abbotku01         5
    ##  8 abernbr01         2
    ##  9 abramca01         2
    ## 10 abreujo01         2
    ## # ... with 6,679 more rows

Lots of players have multiple positions! To address this, we will take
the position with the most games played for each individual player and
store that in a new data frame. Then we will add “position” as a
variable in our “People” table.

``` r
player_position <- Fielding %>%
  group_by(playerID, POS) %>% 
  summarize(games = sum(G)) %>% 
  slice(which.max(games)) %>% 
  arrange(desc(games))

people <- merge(people, player_position[c("playerID", "POS")],
                by.x = "playerID")

names(people)[names(people) == "POS"] <- "position"

people$position <- as.factor(people$position)
```

``` r
summary(people$position)
```

    ##  1B  2B  3B   C  OF   P  SS 
    ## 100  85  88  99 300 393  84

Pitchers are non-pitchers are obviously judged on a different set of
statistics. For now, we will focus on fielders and develop a separate
model for pitchers at some point in the future.

``` r
fielders <- people %>% 
  filter(position != "P")
```

Now we can pull some stats. Hitting stats can be found in the “Batting”
table.

``` r
batting <- Lahman::Batting
str(batting)
```

    ## 'data.frame':    108789 obs. of  22 variables:
    ##  $ playerID: chr  "abercda01" "addybo01" "allisar01" "allisdo01" ...
    ##  $ yearID  : int  1871 1871 1871 1871 1871 1871 1871 1871 1871 1871 ...
    ##  $ stint   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ teamID  : Factor w/ 149 levels "ALT","ANA","ARI",..: 136 111 39 142 111 56 111 24 56 24 ...
    ##  $ lgID    : Factor w/ 7 levels "AA","AL","FL",..: 4 4 4 4 4 4 4 4 4 4 ...
    ##  $ G       : int  1 25 29 27 25 12 1 31 1 18 ...
    ##  $ AB      : int  4 118 137 133 120 49 4 157 5 86 ...
    ##  $ R       : int  0 30 28 28 29 9 0 66 1 13 ...
    ##  $ H       : int  0 32 40 44 39 11 1 63 1 13 ...
    ##  $ X2B     : int  0 6 4 10 11 2 0 10 1 2 ...
    ##  $ X3B     : int  0 0 5 2 3 1 0 9 0 1 ...
    ##  $ HR      : int  0 0 0 2 0 0 0 0 0 0 ...
    ##  $ RBI     : int  0 13 19 27 16 5 2 34 1 11 ...
    ##  $ SB      : int  0 8 3 1 6 0 0 11 0 1 ...
    ##  $ CS      : int  0 1 1 1 2 1 0 6 0 0 ...
    ##  $ BB      : int  0 4 2 0 2 0 1 13 0 0 ...
    ##  $ SO      : int  0 0 5 2 1 1 0 1 0 0 ...
    ##  $ IBB     : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ HBP     : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SH      : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ SF      : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ GIDP    : int  0 0 1 0 0 0 0 1 0 0 ...

There are some NA’s present in some of the columns. Let’s see how many
NA values there are in each column.

``` r
colSums(is.na(batting))
```

    ## playerID   yearID    stint   teamID     lgID        G       AB        R 
    ##        0        0        0        0        0        0        0        0 
    ##        H      X2B      X3B       HR      RBI       SB       CS       BB 
    ##        0        0        0        0      756     2368    23541        0 
    ##       SO      IBB      HBP       SH       SF     GIDP 
    ##     2100    36650     2816     6068    36103    25441

There are different ways to handle missing values. In this situation, we
can substitute in 0 where there is NA. First, we can create a function
that converts the NA’s to 0. Then we can identify the column indices
where there are missing values present. Finally we can apply our
function to those columns so there are no longer missing values in our
data.

``` r
na_to_zero <- function(x){
  x[is.na(x)] <- 0
  return(x)
}

na_cols <- unique(which(is.na(batting), arr.ind = TRUE)[,2])
print(na_cols)
```

    ## [1] 13 14 15 17 18 19 20 21 22

``` r
batting[na_cols] <- lapply(batting[na_cols], na_to_zero)
```

Let’s confirm the function worked:

``` r
colSums(is.na(batting))
```

    ## playerID   yearID    stint   teamID     lgID        G       AB        R 
    ##        0        0        0        0        0        0        0        0 
    ##        H      X2B      X3B       HR      RBI       SB       CS       BB 
    ##        0        0        0        0        0        0        0        0 
    ##       SO      IBB      HBP       SH       SF     GIDP 
    ##        0        0        0        0        0        0

Similar to the “Fielding” table, the “Batting” table is aggregated
statistics for each player and year. Players on the Hall of Fame ballot
are judged on their career stats, so we’ll need to aggregate the data up
to the player level.

``` r
batting_stats <- batting %>%
  group_by(playerID) %>% 
  summarize(batting_seasons = length(unique(yearID)),
            games = sum(G),
            at_bats = sum(AB),
            runs = sum(R),
            hits = sum(H),
            singles = sum(H-(X2B + X3B + HR)),
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
            plate_app = sum(AB + BB + SH + SF + HBP))
```

We can also create new variables from those stats.

``` r
batting_stats <- batting_stats %>%
  mutate(
    average = hits / at_bats,
    on_base = (hits + walks + hbp) / (at_bats + walks + hbp + sac_fly),
    slugging = (singles + 2 * doubles + 3 * triples + 4 * home_runs) /
      at_bats,
    ops = on_base + slugging
  )
```

We can combine our data into a single data frame for use. Let’s remove
some columns which are not relevant to this analysis and add in batting
stats and the inducted flag.

``` r
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
```

    ## 'data.frame':    756 obs. of  28 variables:
    ##  $ playerID       : chr  "aaronha01" "adamsbo03" "adamssp01" "ageeto01" ...
    ##  $ name           : chr  "Hank Aaron" "Bobby Adams" "Sparky Adams" "Tommie Agee" ...
    ##  $ position       : Factor w/ 7 levels "1B","2B","3B",..: 5 3 2 5 2 1 7 2 4 5 ...
    ##  $ batting_seasons: int  23 14 13 12 12 15 11 17 20 17 ...
    ##  $ games          : int  3298 1281 1424 1129 1139 1749 1195 2379 1377 2082 ...
    ##  $ at_bats        : int  12364 4019 5557 3912 3404 6332 3927 9073 4530 7339 ...
    ##  $ runs           : int  2174 591 844 558 357 1099 442 1508 520 985 ...
    ##  $ hits           : int  3771 1082 1588 999 815 1848 999 2724 1236 2101 ...
    ##  $ singles        : int  2294 808 1282 672 581 1098 760 1930 865 1487 ...
    ##  $ doubles        : int  624 188 249 170 140 320 140 504 249 359 ...
    ##  $ triples        : int  98 49 48 27 21 79 44 80 10 49 ...
    ##  $ home_runs      : int  755 37 9 130 73 351 55 210 112 206 ...
    ##  $ rbis           : num  2297 303 394 433 352 ...
    ##  $ stolen_bases   : num  240 67 154 167 13 133 63 474 25 107 ...
    ##  $ caught_stealing: num  73 30 50 81 16 52 30 114 24 67 ...
    ##  $ walks          : int  1402 414 453 342 370 894 300 1032 212 423 ...
    ##  $ strikeouts     : num  1383 447 223 918 424 ...
    ##  $ int_walks      : num  293 1 0 26 52 138 62 62 15 67 ...
    ##  $ hbp            : num  32 17 28 34 8 16 27 50 41 57 ...
    ##  $ sac_hit        : num  21 78 136 21 24 19 56 148 48 33 ...
    ##  $ sac_fly        : num  121 5 0 15 17 53 22 97 34 56 ...
    ##  $ gidp           : num  328 62 14 99 60 164 95 206 132 135 ...
    ##  $ plate_app      : num  13940 4533 6174 4324 3823 ...
    ##  $ average        : num  0.305 0.269 0.286 0.255 0.239 ...
    ##  $ on_base        : num  0.374 0.34 0.343 0.32 0.314 ...
    ##  $ slugging       : num  0.555 0.368 0.353 0.412 0.357 ...
    ##  $ ops            : num  0.928 0.708 0.695 0.732 0.671 ...
    ##  $ inducted       : Factor w/ 2 levels "N","Y": 2 1 1 1 1 1 1 2 1 1 ...

There are some more variables that might be relevant to the analysis.
Considering players are generally compared to other players from the
same era, it might make sense to add a variable representing what era
each player played in. We’ll calculate which decade each player played
in the most games and use that as our variable.

``` r
player_decade <- batting %>%
  mutate(decade = yearID - (yearID %% 10)) %>%
  group_by(playerID, decade) %>%
  summarize(games = sum(G)) %>%
  filter(games == max(games))

fielders <-
  merge(fielders, player_decade[c("playerID", "decade")], by.x = "playerID")
```

Winning awards signifies a great season from a player and enough great
seasons could mean induction into the Hall of Fame. The Lahmen package
provides this data as well.

``` r
awards <- Lahman::AwardsPlayers
str(awards)
```

    ## 'data.frame':    6236 obs. of  6 variables:
    ##  $ playerID: chr  "bondto01" "hinespa01" "heckegu01" "radboch01" ...
    ##  $ awardID : chr  "Pitching Triple Crown" "Triple Crown" "Pitching Triple Crown" "Pitching Triple Crown" ...
    ##  $ yearID  : int  1877 1878 1884 1884 1887 1888 1889 1894 1894 1901 ...
    ##  $ lgID    : Factor w/ 4 levels "AA","AL","ML",..: 4 4 1 4 1 4 4 4 4 2 ...
    ##  $ tie     : chr  NA NA NA NA ...
    ##  $ notes   : chr  NA NA NA NA ...

``` r
unique(awards$awardID)
```

    ##  [1] "Pitching Triple Crown"               "Triple Crown"                       
    ##  [3] "Baseball Magazine All-Star"          "Most Valuable Player"               
    ##  [5] "TSN All-Star"                        "TSN Guide MVP"                      
    ##  [7] "TSN Major League Player of the Year" "TSN Pitcher of the Year"            
    ##  [9] "TSN Player of the Year"              "Rookie of the Year"                 
    ## [11] "Babe Ruth Award"                     "Lou Gehrig Memorial Award"          
    ## [13] "World Series MVP"                    "Cy Young Award"                     
    ## [15] "Gold Glove"                          "TSN Fireman of the Year"            
    ## [17] "All-Star Game MVP"                   "Hutch Award"                        
    ## [19] "Roberto Clemente Award"              "Rolaids Relief Man Award"           
    ## [21] "NLCS MVP"                            "ALCS MVP"                           
    ## [23] "Silver Slugger"                      "Branch Rickey Award"                
    ## [25] "Hank Aaron Award"                    "TSN Reliever of the Year"           
    ## [27] "Comeback Player of the Year"         "Outstanding DH Award"               
    ## [29] "Reliever of the Year Award"

For now, we will grab the most recognizable awards. This requires some
data wrangling. Since awards are in a single variable, we can make a
column for each award and sum up the amount of awards per player.

``` r
awards$one <- 1

awards <- awards %>%
  spread(awardID, one)

awards <- data.frame(lapply(awards, na_to_zero))
```

    ## Warning in `[<-.factor`(`*tmp*`, is.na(x), value = 0): invalid factor level, NA
    ## generated

``` r
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
```

Next we’ll add is batting titles. Like the Triple Crown, this award is
decided strictly by statistics. The awards is giving to the player in
each league (American League and National League) with the highest
batting average, given they have the minimum number of plate
appearances.

Note: Melky Cabrera was suspended in 2012 and was deemed ineligible for
the batting title. He is manually excluded in the code below.

Note2: Due to Covid-19, the 2020 was a shortened season. This meant the
plate appearances requirement had to be adjusted, though we won’t be
looking at active players in this analysis.

``` r
batting_titles <- batting %>%
  filter(paste(yearID, playerID, sep = "") != "2012cabreme01") %>%
  mutate(PA = (AB + BB + SH + SF + HBP),
         BA = H / AB) %>%
  group_by(yearID, playerID, lgID) %>%
  mutate(BA = ifelse(yearID != 2020, ifelse(PA >= 502, BA, H / ((
    502 - PA
  ) + AB)),
  ifelse(PA >= 186, BA, H / ((
    186 - PA
  ) + AB)))) %>%
  ungroup() %>%
  group_by(yearID, lgID) %>%
  filter(BA == max(BA)) %>%
  group_by(playerID) %>%
  summarize(titles = n()) %>%
  arrange(desc(titles))


fielders <-
  merge(fielders, batting_titles, by = "playerID", all.x =  TRUE)

fielders$batting_titles <- na_to_zero(fielders$titles)
```

The last variable we will add for now is World Series Titles.

``` r
world_series_by_player <- batting %>%
  group_by(yearID, playerID) %>%
  filter(stint == max(stint))

world_series_by_player <-
  merge(
    world_series_by_player,
    SeriesPost[SeriesPost$round == "WS",
               c("yearID", "teamIDwinner",
                 "round")],
    by.x = c("yearID", "teamID"),
    by.y = c("yearID", "teamIDwinner"),
    all.x = TRUE
  )

world_series_by_player <- world_series_by_player %>%
  mutate(round = if_else(is.na(world_series_by_player$round), 0, 1)) %>%
  rename(WS = round) %>%
  group_by(playerID) %>%
  summarize(WS_wins = sum(WS)) %>%
  select(playerID, WS_wins)

fielders <-
  merge(fielders,
        world_series_by_player,
        by = "playerID",
        all.x = TRUE)
```

We are now ready to start exploring our variables to see which ones are
correlated with being inducted into the Hall of Fame.

## Exploritory Data Analysis
