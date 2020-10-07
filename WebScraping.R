## Let's explore Indian Premier League - https://www.iplt20.com/
## Let scrape the Higest Scorer player Name, Score, Innings etc, as per all IPL seasons from 2008-2020.

# Importing the required libraries

library(tidyverse)
library(tidyverse)
library(rvest)
library(tidytext)
library(polite) 
library(magrittr) # better handling of pipes
library(purrr) # to work with lists and map functionsmusic_text
library(glue) # to paste strings
library(stringr) # to hand strings
library(ggplot2)

# Let's take www.ipl20.com to scrape the data for seasons from 2008-2020. 
# The next few blocks of R code has created to collect the data from ipl20.com and then converted it into required data frames using tibble. 

#### To get the name of Top Player, class name in HTML - 'top-players__player-name'
#### To get the score of the Top Player in all season, class name in HTML - 'top-players__r.is-active'
#### To get the innings played by the top scorer in a sesaon, class name in HTML - 'top-players__inns'
#### To get the ball faced by a player in a sesaon, class name in HTML - 'top-players__b'

# a tibble is created with all IPL seasons from 2008 to 2020
ipl_season <- tibble(season = c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))
# URL to get the stats for the year 2019
url_string = "https://www.iplt20.com/stats/2019/most-runs"
page_html <- read_html(url_string)

ipl_season  # printing the 'ipl_season'
# A tibble: 13 × 1
# season
# <dbl>
# 2008
# 2009
# 2010
# 2011
# 2012
# 2013
# 2014
# 2015
# 2016
# 2017
# 2018
# 2019
# 2020

## The next few blocks of code is scraping the data for year 2019.

###### The code in next block is used to scrape the top player name in year 2019 using 'url_string'and class name 'top-players__player-name'.
# Top Player Name - .top-players__player-name
name_2019 <- page_html %>%
   html_node(".top-players__player-name")  %>%
   html_text()

# Top Player Name 
name_2019
# '\n                        \n                            David\n                            Warner\n                        \n                '

###### The code in next block is used to scrape the top player score in year 2019 using 'url_string' and class name 'top-players__r.is-active'.
# Top player score - top-players__r.is-active
score_2019 <- page_html %>%
   html_node(".top-players__r.is-active")  %>%
  html_text() 

# Top Player Score
score_2019
# '\n                    692\n                '

###### The code in next block is used to scrape the number of innings played by the all the top scorers in year 2019 using 'url_string' and class name 'top-players__inns'.
# Top player Innings - top-players__inns
innings_2019 <- page_html %>%
   html_nodes(".top-players__inns ")  %>%
  html_text()  

innings_2019
#'\n                                Inns\n                            ''\n                    12\n                ''\n                    14\n                ''\n                    16\n                ''\n                    16\n                ''\n                    13\n                ''\n                    13\n                ''\n                    16\n                ''\n                    14\n                ''\n                    16\n                ''\n                    10\n                ''\n                    13\n                ''\n                    15\n                ''\n                    12\n                ''\n                    13\n                ''\n                    15\n                ''\n                    15\n                ''\n                    17\n                ''\n                    12\n                ''\n                    13\n                ''\n                    17\n                ''\n                    14\n                ''\n                    16\n                ''\n                    11\n                ''\n                    11\n                ''\n                    12\n                ''\n                    13\n                ''\n                    10\n                ''\n                    8\n                ''\n                    13\n                ''\n                    17\n                ''\n                    11\n                ''\n                    14\n                ''\n                    13\n                ''\n                    14\n                ''\n                    10\n                ''\n                    10\n                ''\n                    10\n                ''\n                    12\n                ''\n                    15\n                ''\n                    5\n                ''\n                    6\n                ''\n                    12\n                ''\n                    12\n                ''\n                    5\n                ''\n                    9\n                ''\n                    9\n                ''\n                    7\n                ''\n                    9\n                ''\n                    7\n                ''\n                    12\n                ''\n                    9\n                ''\n                    6\n                ''\n                    4\n                ''\n                    3\n                ''\n                    8\n                ''\n                    5\n                ''\n                    5\n                ''\n                    4\n                ''\n                    3\n                ''\n                    9\n                ''\n                    7\n                ''\n                    4\n                ''\n                    5\n                ''\n                    5\n                ''\n                    7\n                ''\n                    2\n                ''\n                    7\n                ''\n                    5\n                ''\n                    3\n                ''\n                    4\n                ''\n                    5\n                ''\n                    6\n                ''\n                    4\n                ''\n                    8\n                ''\n                    3\n                ''\n                    8\n                ''\n                    6\n                ''\n                    2\n                ''\n                    4\n                ''\n                    7\n                ''\n                    4\n                ''\n                    1\n                ''\n                    1\n                ''\n                    6\n                ''\n                    3\n                ''\n                    4\n                ''\n                    1\n                ''\n                    2\n                ''\n                    1\n                ''\n                    3\n                ''\n                    4\n                ''\n                    5\n                ''\n                    4\n                ''\n                    4\n                ''\n                    4\n                ''\n                    2\n                ''\n                    3\n                ''\n                    1\n                ''\n                    4\n                ''\n                    1\n

###### The code in next block is used to scrape the ball faced by all the top scorers in year 2019 using 'url_string'and class name 'top-players__b'.
# Top Player Ball faced- top-players__b  
balls_2019 <- page_html %>%
   html_nodes(".top-players__b ")  %>%
   html_text() 

balls_2019
# '\n                                BF\n                            ''\n                    481\n                ''\n                    438\n                ''\n                    398\n                ''\n                    384\n                ''\n                    249\n                ''\n                    319\n                ''\n                    300\n                ''\n                    328\n                ''\n                    386\n                ''\n                    283\n                ''\n                    287\n                ''\n                    324\n                ''\n                    309\n                ''\n                    290\n                ''\n                    315\n                ''\n                    210\n                ''\n                    312\n                ''\n                    321\n                ''\n                    285\n                ''\n                    314\n                ''\n                    268\n                ''\n                    264\n                ''\n                    263\n                ''\n                    235\n                ''\n                    230\n                ''\n                    234\n                ''\n                    275\n                ''\n                    205\n                ''\n                    238\n                ''\n                    303\n                ''\n                    245\n                ''\n                    178\n                ''\n                    173\n                ''\n                    193\n                ''\n                    133\n                ''\n                    164\n                ''\n                    156\n                ''\n                    154\n                ''\n                    150\n                ''\n                    143\n                ''\n                    107\n                ''\n                    120\n                ''\n                    169\n                ''\n                    126\n                ''\n                    130\n                ''\n                    86\n                ''\n                    118\n                ''\n                    99\n                ''\n                    76\n                ''\n                    88\n                ''\n                    88\n                ''\n                    100\n                ''\n                    75\n                ''\n                    70\n                ''\n                    55\n                ''\n                    73\n                ''\n                    53\n                ''\n                    70\n                ''\n                    53\n                ''\n                    66\n                ''\n                    54\n                ''\n                    48\n                ''\n                    40\n                ''\n                    40\n                ''\n                    63\n                ''\n                    61\n                ''\n                    46\n                ''\n                    57\n                ''\n                    52\n                ''\n                    49\n                ''\n                    37\n                ''\n                    28\n                ''\n                    33\n                ''\n                    45\n                ''\n                    34\n                ''\n                    23\n                ''\n                    37\n                ''\n                    23\n                ''\n                    22\n                ''\n                    25\n                ''\n                    24\n                ''\n                    12\n                ''\n                    24\n                ''\n                    24\n                ''\n                    17\n                ''\n                    19\n                ''\n                    17\n                ''\n                    13\n                ''\n                    13\n                ''\n                    7\n                ''\n                    15\n                ''\n                    19\n                ''\n                    8\n                ''\n                    11\n                ''\n                    16\n                ''\n                    10\n                ''\n                    3\n                ''\n                    10\n                ''\n                    12\n                ''\n                    9\n                '

# Let's do the conversion of scrapped data into their respective data frames. 
# The next few blocks are showing the transactions for the year 2019, where the collected data has transferred into an individual data frame and further joined into one data frame using the 'mutate' function.

ipl_season_2019 <- ipl_season %>% slice(12)
ipl_season_2019
# A tibble: 1 × 1
# season
# <dbl>
# 2019

# removal of escape characters using 'gsub' function.
player_name_2019 <- tibble(PlayerName = str_trim(gsub("[\r\n\t]","",name_2019)))
player_name_2019
# A tibble: 1 × 1
# PlayerName
# <chr>
# David Warner

player_score_2019 <- tibble(PlayerScore = str_trim(gsub("[\r\n\t]","",score_2019)))
player_score_2019
# A tibble: 1 × 1
# PlayerScore
# <chr>
# 692

innings_2019 <- tibble(innings_2019 = str_trim(gsub("[\r\n\t]","",innings_2019)))
innings_2019 <- innings_2019 %>% slice(2)
innings_2019
# A tibble: 1 × 1
# innings_2019
# <chr>
# 12

balls_2019 <- tibble(balls = str_trim(gsub("[\r\n\t]","",balls_2019)))
balls_2019 <- balls_2019 %>% slice(2)
balls_2019
# A tibble: 1 × 1
# balls
# <chr>
# 481

# Merging data into DF with the addition of new column.
ipl_season_2019 <- ipl_season_2019 %>% add_column(Name =player_name_2019$PlayerName)
ipl_season_2019
# A tibble: 1 × 2
# season	Name
# <dbl>	<chr>
# 2019	David Warner

# Merging data into DF with the addition of new columns as Name, Player_Score, Innings, and Balls_Played.
ipl_season_result <- ipl_season_2019 %>% add_column(Player_Score =player_score_2019$PlayerScore)
ipl_season_result <- ipl_season_result %>% add_column(Innings =innings_2019$innings_2019)
ipl_season_result <- ipl_season_result %>% add_column(Balls_Played =balls_2019$balls)

ipl_season_result
# A tibble: 1 × 5
# season	Name	Player_Score	Innings	Balls_Played
# <dbl>	<chr>	<chr>	<chr>	<chr>
# 2019	David Warner	692	12	481

# The next block of code is performing the conversion from character to numeric and calculating the Average and Strike Rate of a player.
#Covnersion of data type from character to numeric
ipl_season_result$Player_Score <- as.numeric(unlist(ipl_season_result$Player_Score))
ipl_season_result$Innings <- as.numeric(unlist(ipl_season_result$Innings))
ipl_season_result$Balls_Played <- as.numeric(unlist(ipl_season_result$Balls_Played))
#ipl_season_result

#Average calculation

ipl_season_result %>%
  mutate(Average = (ipl_season_result$Player_Score/ipl_season_result$Innings),
        Strike_Rate = (ipl_season_result$Player_Score/ipl_season_result$Balls_Played)*100)

# A tibble: 1 × 7
# season	Name	Player_Score	Innings	Balls_Played	Average	Strike_Rate
# <dbl>	<chr>	<dbl>	<dbl>	<dbl>	<dbl>	<dbl>
# 2019	David Warner	692	12	481	57.66667	143.8669

#Automation of code for all seasons

## Six user defined function created as described below to scrape data from different pages of website.
# a glimpse, https://www.iplt20.com/stats/2008/most-runs  or https://www.iplt20.com/stats/2009/most-runs or https://www.iplt20.com/stats/2012/most-runs etc.

## 'delay()' to introduce the delay of seconds while scrapping the data from the website. This will help us to avoid access blockage from the website.
## 'get_player_name()' is used to get the player name.
## 'get_player_score()' is used to get the player score.
## 'get_player_innings()' is used to get the played innings.
## 'get_player_bf()' is used to get the balls played.
## 'genericfunction()' is a generic user defined function with three three arguments and return the data frame using less than coomparision.

# function delay
delay <- function(x)
{
    Sys.sleep(x)
}

# function get_player_name

# Top Player Name - top-players__player-name
get_player_name <- function(season) {
    
    url_string <- glue("https://www.iplt20.com/stats/{season}/most-runs")
    page_html <- read_html(url_string)
		page <- page_html %>%
		html_node(".top-players__player-name")  %>%
		html_text()
    
   name <- tibble(PlayerName = str_trim(gsub("[\r\n\t]","",page)))
   # additon on 2 seconds delay
   delay(2)
   
  return (name)
}

# function get_player_score
get_player_score <- function(year) {
    
    url_string <- glue("https://www.iplt20.com/stats/{year}/most-runs")
    page_html <- read_html(url_string)
    
    page <- page_html %>%
		html_node(".top-players__r.is-active")  %>%
		html_text()
    # additon on 2 seconds delay
    delay(2)
    score <- tibble(PlayerScore = str_trim(gsub("[\r\n\t]","",page))) 
    
    return (score)
}

# function get_player_innings
get_player_innings <- function(year)
    {
    url_string <- glue("https://www.iplt20.com/stats/{year}/most-runs")
    page_html <- read_html(url_string)
    
    page <- page_html %>%
		html_nodes(".top-players__inns ")  %>%
		html_text()  

    innings <- tibble(innings = str_trim(gsub("[\r\n\t]","",page)))
    innings <- innings %>% slice(2)
    # additon on 2 seconds delay
    delay(2)

    return (innings)
}

# function get_player_bf
 
get_player_bf <- function(year)
    {
    url_string <- glue("https://www.iplt20.com/stats/{year}/most-runs")
    page_html <- read_html(url_string)
    
    page <- page_html %>%
		html_nodes(".top-players__b ")  %>%
		html_text()
 
    balls <- tibble(ballFaced = str_trim(gsub("[\r\n\t]","",page)))
    balls <- balls %>% slice(2)
    # additon on 2 seconds delay
    delay(2)

    return(balls)
}

# generic function to do 'less than' or 'greater than' comparsion
genericfunction <- function(data,myargument,anotherargument,comparsion)  {
    if(comparsion =="G"){
            filter_data <- data %>% filter(myargument > anotherargument)
        } else {
        filter_data <- data %>% filter(myargument < anotherargument)
    }
    
    return(filter_data)
}

# The 'get_player_name_possibly' function is created to avoid any NULL value while reteriving the player name from 'get_player_name'.
get_player_name_possibly <- possibly(get_player_name,
                                   tibble(Player_Name = NA),
                                   quiet = TRUE)


##### Using MAP and MUTATE fucntion coverting data into a relational data frame.
##### creation of a DF (ipl_season_player) with sesaon and name of the highest scorer player in particular season.

ipl_season_player <- ipl_season %>%
mutate(Player_Name = map(season,
                           get_player_name_possibly))

ipl_season_player

# A tibble: 13 × 2
# season	Player_Name
# <dbl>	<list>
# 2008	Shaun Marsh
# 2009	Matthew Hayden
# 2010	Sachin Tendulkar
# 2011	Chris Gayle
# 2012	Chris Gayle
# 2013	Michael Hussey
# 2014	Robin Uthappa
# 2015	David Warner
# 2016	Virat Kohli
# 2017	David Warner
# 2018	Kane Williamson
# 2019	David Warner
# 2020	KL Rahul

# The 'get_player_score_possibly' function is created to avoid any NULL value while reteriving the player score from 'get_player_score'.
get_player_score_possibly <- possibly(get_player_score,
                                   tibble(Player_Score = NA),
                                   quiet = TRUE)

##### creating a another DF (ipl_season_result) with season, scores, ball played by the highest scorer player in particular season.
ipl_season_result <- ipl_season %>%
mutate(Player_Score = map(season,
                           get_player_score_possibly))

ipl_season_result

# A tibble: 13 × 2
# season	Player_Score
# <dbl>	<list>
# 2008	616
# 2009	572
# 2010	618
# 2011	608
# 2012	733
# 2013	733
# 2014	660
# 2015	562
# 2016	973
# 2017	641
# 2018	735
# 2019	692
# 2020	302

# Addition of player innings into ipl_season_result using 'get_player_innings' function.

ipl_season_result <- ipl_season_result %>%
mutate(Player_Innings = map(season,
                           get_player_innings))

ipl_season_result
# A tibble: 13 × 3
# season	Player_Score	Player_Innings
# <dbl>	<list>	<list>
# 2008	616	11
# 2009	572	12
# 2010	618	15
# 2011	608	12
# 2012	733	14
# 2013	733	17
# 2014	660	16
# 2015	562	14
# 2016	973	16
# 2017	641	14
# 2018	735	17
# 2019	692	12
# 2020	302	5

# Addition of the balls faced by the player into ipl_season_result using 'get_player_bf' function.
ipl_season_result <- ipl_season_result %>%
mutate(Player_BF = map(season,
                           get_player_bf))

ipl_season_result
# A tibble: 13 × 4
# season	Player_Score	Player_Innings	Player_BF
# <dbl>	<list>	<list>	<list>
# 2008	616	11	441
# 2009	572	12	395
# 2010	618	15	466
# 2011	608	12	332
# 2012	733	14	456
# 2013	733	17	566
# 2014	660	16	479
# 2015	562	14	359
# 2016	973	16	640
# 2017	641	14	452
# 2018	735	17	516
# 2019	692	12	481
# 2020	302	5	213

##### Creating a new DF and converting all character data type into numeric data type. 
ipl_df <- ipl_season_result
# data type conversion
ipl_df$Player_Score <- as.numeric(unlist(ipl_df$Player_Score))
ipl_df$Player_Innings <- as.numeric(unlist(ipl_df$Player_Innings))
ipl_df$Player_BF <- as.numeric(unlist(ipl_df$Player_BF))
ipl_df

# A tibble: 13 × 4
# season	Player_Score	Player_Innings	Player_BF
# <dbl>	<dbl>	<dbl>	<dbl>
# 2008	616	11	441
# 2009	572	12	395
# 2010	618	15	466
# 2011	608	12	332
# 2012	733	14	456
# 2013	733	17	566
# 2014	660	16	479
# 2015	562	14	359
# 2016	973	16	640
# 2017	641	14	452
# 2018	735	17	516
# 2019	692	12	481
# 2020	302	5	213

##### some functions are applied on DF 'ipl_df' and 'ipl_player_name'.
ipl_df %>% arrange(desc(season)) %>% head()

season_player <- ipl_season_player %>% inner_join(ipl_df, by="season")
season_player %>% select(season,Player_Name,Player_Score)

# A tibble: 13 × 3
# season	Player_Name	Player_Score
# <dbl>	<list>	<dbl>
# 2008	Shaun Marsh	616
# 2009	Matthew Hayden	572
# 2010	Sachin Tendulkar	618
# 2011	Chris Gayle	608
# 2012	Chris Gayle	733
# 2013	Michael Hussey	733
# 2014	Robin Uthappa	660
# 2015	David Warner	562
# 2016	Virat Kohli	973
# 2017	David Warner	641
# 2018	Kane Williamson	735
# 2019	David Warner	692
# 2020	KL Rahul	302

##### To get the list of Players who scored more than 650 in all IPL seasons, we are calling 'genericfunction' by passing 'myargument' as column name (ipl_df$Player_Score) and 'anotherargument' as numeric value 650. The comparasion values is passed as 'G' to check greater than values.
top_scorer <- season_player %>%  
    genericfunction(season_player$Player_Score,650,"G")    # int column name and value
top_scorer %>% head()

# A tibble: 6 × 5
# season	Player_Name	Player_Score	Player_Innings	Player_BF
# <dbl>	<list>	<dbl>	<dbl>	<dbl>
# 2012	Chris Gayle	733	14	456
# 2013	Michael Hussey	733	17	566
# 2014	Robin Uthappa	660	16	479
# 2016	Virat Kohli	973	16	640
# 2018	Kane Williamson	735	17	516
# 2019	David Warner	692	12	481

##### Calculating 'Average' and 'Strike Rate' for each player.
ipl_t <- ipl_df %>%
  mutate(Average = (ipl_df$Player_Score/ipl_df$Player_Innings),
        Strike_Rate = (ipl_df$Player_Score/ipl_df$Player_BF)*100)
ipl_t
# A tibble: 13 × 6
# season	Player_Score	Player_Innings	Player_BF	Average	Strike_Rate
# <dbl>	<dbl>	<dbl>	<dbl>	<dbl>	<dbl>
# 2008	616	11	441	56.00000	139.6825
# 2009	572	12	395	47.66667	144.8101
# 2010	618	15	466	41.20000	132.6180
# 2011	608	12	332	50.66667	183.1325
# 2012	733	14	456	52.35714	160.7456
# 2013	733	17	566	43.11765	129.5053
# 2014	660	16	479	41.25000	137.7871
# 2015	562	14	359	40.14286	156.5460
# 2016	973	16	640	60.81250	152.0312
# 2017	641	14	452	45.78571	141.8142
# 2018	735	17	516	43.23529	142.4419
# 2019	692	12	481	57.66667	143.8669
# 2020	302	5	213	60.40000	141.7840

##### Joining of the 'ipl_t' with 'ipl_season_player' to get the name of players with their average and strike rate.
player_avg <- ipl_season_player %>% inner_join(ipl_t, by="season")
player_avg %>% select(season,Player_Name,Average, Strike_Rate)

# A tibble: 13 × 4
# season	Player_Name	Average	Strike_Rate
# <dbl>	<list>	<dbl>	<dbl>
# 2008	Shaun Marsh	56.00000	139.6825
# 2009	Matthew Hayden	47.66667	144.8101
# 2010	Sachin Tendulkar	41.20000	132.6180
# 2011	Chris Gayle	50.66667	183.1325
# 2012	Chris Gayle	52.35714	160.7456
# 2013	Michael Hussey	43.11765	129.5053
# 2014	Robin Uthappa	41.25000	137.7871
# 2015	David Warner	40.14286	156.5460
# 2016	Virat Kohli	60.81250	152.0312
# 2017	David Warner	45.78571	141.8142
# 2018	Kane Williamson	43.23529	142.4419
# 2019	David Warner	57.66667	143.8669
# 2020	KL Rahul	60.40000	141.7840

player_avg
# A tibble: 13 × 7
# season	Player_Name	Player_Score	Player_Innings	Player_BF	Average	Strike_Rate
# <dbl>	<list>	<dbl>	<dbl>	<dbl>	<dbl>	<dbl>
# 2008	Shaun Marsh	616	11	441	56.00000	139.6825
# 2009	Matthew Hayden	572	12	395	47.66667	144.8101
# 2010	Sachin Tendulkar	618	15	466	41.20000	132.6180
# 2011	Chris Gayle	608	12	332	50.66667	183.1325
# 2012	Chris Gayle	733	14	456	52.35714	160.7456
# 2013	Michael Hussey	733	17	566	43.11765	129.5053
# 2014	Robin Uthappa	660	16	479	41.25000	137.7871
# 2015	David Warner	562	14	359	40.14286	156.5460
# 2016	Virat Kohli	973	16	640	60.81250	152.0312
# 2017	David Warner	641	14	452	45.78571	141.8142
# 2018	Kane Williamson	735	17	516	43.23529	142.4419
# 2019	David Warner	692	12	481	57.66667	143.8669
# 2020	KL Rahul	302	5	213	60.40000	141.7840

#### Group by Player Name and summarising the mean of Average and Player Score.
player_avg %>% 
group_by(Player_Name) %>%
 summarise(mean_score = mean(Player_Score),mean_average=mean(Average))  
 
## A tibble: 10 × 3
# Player_Name	mean_score	mean_average
# <list>	<dbl>	<dbl>
# Shaun Marsh	616.0000	56.00000
# Matthew Hayden	572.0000	47.66667
# Sachin Tendulkar	618.0000	41.20000
# Chris Gayle	670.5000	51.51190
# Michael Hussey	733.0000	43.11765
# Robin Uthappa	660.0000	41.25000
# David Warner	631.6667	47.86508
# Virat Kohli	973.0000	60.81250
# Kane Williamson	735.0000	43.23529
# KL Rahul	302.0000	60.40000

## Few Plots 

# Plot 1
player_avg %>% 
      ggplot( #data = .  , # where do we take the data from
         mapping = aes( # the mapping defines the _strutcture_ of the plot:
             x = season, # what to put on the x
             y = Player_Score, # what to put on the y
             colour = Player_Score)) + # and eventually what to group together
    geom_point( # the geometry defines what sorts of "objects" to plot, in this case dots
        ) + # and it can have some structural elements: we want the size of the dot being given by the some variable
    theme_minimal() + # themes are otherall stylistic features
    xlab("Sesaon") + ylab("Score") +
    labs(title = "Season vs High Score")
	
# Plot 2
ggplot(player_avg,aes(x=season,y=Player_Score))+
geom_line(aes(size=Player_Innings),col="pink")+ xlab("Sesaon") + ylab("Score") +
    labs(title = "Season - High Score")
