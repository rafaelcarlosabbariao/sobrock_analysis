#### Import Libraries ####
library(dplyr)
library(DataExplorer)
library(dotenv)
library(fmsb)
library(genius)
library(ggplot2)
library(GGally)
library(ggridges)
library(httr)
library(kableExtra)
library(patchwork)
library(plotly)
library(psych)
library(rvest)
library(shiny)
library(shinydashboard)
library(SmartEDA)
library(SnowballC)
library(summarytools)
library(spotifyr)
library(stringr)
library(tidyr)
library(tidytext)
library(textclean)
library(textstem)
library(tm)
library(xml2)

source("functions.R")

'###############################################################################'
#### Spotify API Connection ####

# For future use, load_dot_env() to load token
load_dot_env()

# App Client ID and Client Secret
clientID = Sys.getenv("SPOTIFY_ID")
clientSecret = Sys.getenv("SPOTIFY_SECRET")

# Request Access Token
tokenRequest = httr::POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, clientSecret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

myToken = httr::content(tokenRequest)$access_token
headerValue = paste0('Bearer ', myToken)

if (length(headerValue) > 0) {
  message("Spotify Connection Ready")
}

'###############################################################################'

#### Data Gathering ####

jmData <- getArtistData("0hEurMDQu99nJRq8pTxO14")  # This is JM's artist ID

# Retrieve John Mayer data, then Sob Rock Data
Sob_Rock_Album <- jmData$Tracks[jmData$Tracks$album == "Sob Rock",]
Sob_Rock_Album <- Sob_Rock_Album %>%
  dplyr::mutate(genre = cbind("SobRock", nrow(Sob_Rock_Album))[1]) 

# saveRDS(Sob_Rock_Album, "sobrockalbum.rds")
Sob_Rock_Album <- readRDS("sobrockalbum.rds")

# Retrieve John Mayer NON Sob Rock Data
Other_JM_Albums <- jmData$Tracks[jmData$Tracks$album != "Sob Rock", ]
Other_JM_Albums <- Other_JM_Albums %>%
  dplyr::mutate(genre = cbind("JohnMayer", nrow(Other_JM_Albums))[1])

# saveRDS(Other_JM_Albums, "otherjmalbums.rds")
Other_JM_Albums <- readRDS("otherjmalbums.rds")

# Retrieve 'Sob Rock' Playlist data
Sob_Rock_Playlist <- getPlaylistInfo("2PYTcc4vVjuQJYbFbdgspr")
Sob_Rock_Playlist <- Sob_Rock_Playlist %>%
  dplyr::mutate(genre = cbind(c(rep("Sob", 35), rep("Rock", 35))))

# saveRDS(Sob_Rock_Playlist, "sobrockplaylist.rds")
Sob_Rock_Playlist <- readRDS("sobrockplaylist.rds")

# Run this
Sob_Rock_Album <- readRDS("sobrockalbum.rds")
Other_JM_Albums <- readRDS("otherjmalbums.rds")
Sob_Rock_Playlist <- readRDS("sobrockplaylist.rds")

'###############################################################################'

#### Data Summary Part 1 - Album vs Playlist ####

# Merge both album and playlist
sob_rock_all <- dplyr::bind_rows(Sob_Rock_Album, Sob_Rock_Playlist)
str(sob_rock_all)
summary(sob_rock_all)

##### Different Methods of Summarization #####

###### Using base::summary ######
summary_album <- base::summary(Sob_Rock_Album[12:20]) %>%  # only taking music features
  base::as.data.frame() %>%
  dplyr::mutate(Column = sub("\\:.*", "", Freq),
                Freq = sub(".*:", "", Freq)) %>%
  dplyr::rename(Feature = Var2) %>%
  tidyr::pivot_wider(id_cols = Feature,
                     names_from = Column,
                     values_from = Freq) %>%
  kbl() %>%
  kable_classic("striped") %>%
  row_spec(0, bold=TRUE) 

print(summary_album)

summary_playlist <- base::summary(Sob_Rock_Playlist[12:20]) %>%
  base::as.data.frame() %>%
  dplyr::mutate(Column = sub("\\:.*", "", Freq),
                Freq = sub(".*:", "", Freq)) %>%
  dplyr::rename(Feature = Var2) %>%
  tidyr::pivot_wider(id_cols = Feature,
                     names_from = Column,
                     values_from = Freq) %>%
  kbl() %>%
  kable_classic("striped") %>%
  row_spec(0, bold=TRUE)
  
print(summary_playlist)

###### Using DataExplorer ######
DataExplorer::create_report(
  sob_rock_all[12:21],
  output_file = "DataExplorer_album_and_playlist.html",
  report_title = "Sob Rock Music Analysis"
)

'
Correlation Analysis
  - Energy: Sob Rock is literally the neutral against Rock and Sob
  - Danceability: Sob Rock is most danceable
'

###### Using GGally ######
GGally::ggpairs(sob_rock_all[12:20], aes(color = sob_rock_all$genre, alpha=0.5))

###### Using SmartEDA ######
SmartEDA::ExpReport(
  sob_rock_all[12:21],
  op_file = "SmartEDA_album_and_playlist.html"
)

###### Using summarytools ######
summarytools::dfSummary(sob_rock_all[12:21])

###### Using psych ######
psych::describeBy(sob_rock_all[12:20], group=sob_rock_all$genre)

'###############################################################################'

#### Data Summary Part 2  - SobRock Album vs Other JM Albums ####

# Merge Sob Rock album and other John Mayer Albums
john_mayer_all <- dplyr::bind_rows(Sob_Rock_Album, Other_JM_Albums)
str(john_mayer_all)
summary(john_mayer_all)

##### Different Methods of Summarization #####

###### Using base::summary #####
summary_sob_rock_album <- base::summary(Sob_Rock_Album[12:20]) %>%
  base::as.data.frame() %>%
  dplyr::mutate(Column = sub("\\:.*", "", Freq),
                Freq = sub(".*:", "", Freq)) %>%
  dplyr::rename(Feature = Var2) %>%
  tidyr::pivot_wider(id_cols = Feature,
                     names_from = Column,
                     values_from = Freq) %>%
  kbl() %>%
  kable_classic("striped") %>%
  row_spec(0, bold=TRUE)

print(summary_sob_rock_album)

summary_other_jm_albums <- base::summary(Other_JM_Albums[12:20]) %>%
  base::as.data.frame() %>%
  dplyr::mutate(Column = sub("\\:.*", "", Freq),
                Freq = sub(".*:", "", Freq)) %>%
  dplyr::rename(Feature = Var2) %>%
  tidyr::pivot_wider(id_cols = Feature,
                     names_from = Column,
                     values_from = Freq) %>%
  kbl() %>%
  kable_classic("striped") %>%
  row_spec(0, bold=TRUE)

print(summary_other_jm_albums)

###### Using DataExplorer ###### 
DataExplorer::create_report(
  john_mayer_all[12:21],
  output_file = "DataExplorer_jm_and_sobrock.html",
  report_title = "John Mayer vs Sob Rock Music Analysis"
)

'
Correlation Analysis
  - Energy: Sob Rock is literally the neutral against Rock and Sob
  - Danceability: Sob Rock is most danceable
'

###### Using GGally ######
john_mayer_all %>%
  dplyr::select(c(12:20)) %>%
  GGally::ggpairs(aes(color = john_mayer_all$genre, alpha = 0.5))

###### Using SmartEDA ######
SmartEDA::ExpReport(
  john_mayer_all[12:21],
  op_file = "SmartEDA_jm_and_sobrock.html"
)

###### Using summarytools ######
summarytools::dfSummary(john_mayer_all[12:21])

###### Using psych (can summarize by a grouping variable) ######
psych::describeBy(john_mayer_all[12:20], group=john_mayer_all$genre)

'###############################################################################'

#### Data Wrangling ####

album <- Sob_Rock_Album %>%
  tidyr::pivot_longer(cols = c(9:20),
                      names_to = "audio_feature",
                      values_to = "feature_value") %>%
  dplyr::mutate(audio_feature = as.factor(audio_feature),
                genre = as.factor(genre))

playlist <- Sob_Rock_Playlist %>%
  tidyr::pivot_longer(cols = c(9:20),
                      names_to = "audio_feature",
                      values_to = "feature_value") %>%
  dplyr::mutate(audio_feature = as.factor(audio_feature),
                genre = as.factor(genre))

songBoxplotDF <- dplyr::bind_rows(album, playlist)
songRadarDF <- dplyr::bind_rows(Sob_Rock_Album, Sob_Rock_Playlist)

'###############################################################################'

#### Data Visualization Part 1 - Album vs SobRock ####

##### Boxplot comparing audio features of "Sob" and "Rock" songs #####
boxplot <- ggplot(songBoxplotDF %>%
                    filter(!audio_feature %in% c("key", "mode", "time_signature", "instrumentalness")),
                  aes(x = audio_feature, y = feature_value)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(color = genre)) +
  facet_wrap(vars(audio_feature), scales = "free", ncol = 2) + 
  theme_classic() +
  theme(axis.text.y = element_blank()) +
  coord_flip()

boxplot

#------------------------------------------------------------------------------#

##### Ridgeline Plot (to display distribution of several numeric variables)
ridgelineDF <- dplyr::bind_rows(Sob_Rock_Album, Sob_Rock_Playlist)

ridgelineDF <- ridgelineDF %>%
  dplyr::select(-c('key', 'mode', 'time_signature')) %>%
  dplyr::mutate_at(.vars = c("loudness", "tempo"), .funs = normalize) %>%
  tidyr::pivot_longer(cols = c(9:17),
                      names_to = "Feature",
                      values_to = "Value")

'
In Normalization, values are put to a [0,1] scale without distorting the differences in values.

  - Used when performing some type of analysis in which we have multiple
    variables that are measured on different scales and we want each of the 
    variables to have the same range.

  - This prevents one variable from being overly influential, esp if measured in different units. 

In standardization, the data is assumed to be in a Gaussian distribution and measures the variable at different scales,
making all the variables equally contribute to the analysis.

  - We do this when we want to know how many standard deviations each value in a dataset lies from the mean.
  
  - Mean of 0. SD of 1.

'

ggplot(ridgelineDF, aes(x = Value, y = Feature, fill = genre)) + 
  geom_density_ridges()

#------------------------------------------------------------------------------#

##### Radar Chart #####
# to plot one+ series of values over multiple quantitative variables

playlist <- songRadarDF %>%
  dplyr::group_by(genre) %>%
  dplyr::summarise_at(vars(11:20), normalize)

playlist <- playlist %>%
  dplyr::summarise_at(-1, mean)

playlist <- rbind(c("2", rep(0, 10)), playlist)
playlist <- rbind(c("1", rep(1, 10)), playlist)

playlist <- playlist %>%
  dplyr::mutate_at(c(1), as.factor) %>%
  dplyr::mutate_at(c(-1), as.numeric) %>%
  as.data.frame()

colors_fill <- c(scales::alpha("red", 0.2),
                 scales::alpha("blue", 0.2),
                 scales::alpha("green", 0.2))

colors_line <-  c(scales::alpha("darkred", 0.9),
                  scales::alpha("darkblue", 0.9),
                  scales::alpha("darkgreen", 0.9))

playlistDF <- playlist[, -1]
rownames(playlistDF) <- playlist[, 1]

radarchart(playlistDF, 
           seg = 10,  # Number of axis segments
           title = "Sob Rockness Chart",
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           plty = 1,
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 2)

# Add a legend
legend(x=0.75, 
       y=1.35, 
       legend = rownames(playlistDF[-c(1, 2), ]), 
       bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 1.5)

#------------------------------------------------------------------------------#

##### Scatter Plot on Valence/Energy Quadrants (to display the relationship bw 2 variables) #####

ggplot(sob_rock_all, aes(x = valence, y = energy, color = genre)) + 
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_vline(xintercept = 0.5, colour = "gray", linetype = "longdash") +  
  ggplot2::geom_hline(yintercept = 0.5, colour = "gray", linetype = "longdash") +
  ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  ggplot2::annotate('text', 0.30 / 2, 0.95, label = "Crazy Train Vibes") +
  ggplot2::annotate('text', 1.75 / 2, 0.95, label = "Heppy Heppy") +
  ggplot2::annotate('text', 1.75 / 2, 0.05, label = "Euphoria") +
  ggplot2::annotate('text', 0.25 / 2, 0.05, label = ":'(") +
  ggplot2::labs(x= "Valence", y= "Energy")
  # ggplot2::theme(plot.margin = margin(15,15,15,15))

'###############################################################################'

#### Data Visualization Part 2 - SobRock Album vs Other JM Albums ####

# Visualize Sob Rock Album against Other John Mayer Albums

album <- Sob_Rock_Album %>%
  tidyr::pivot_longer(cols = c(9:20),
                      names_to = "audio_feature",
                      values_to = "feature_value") %>%
  dplyr::mutate(audio_feature = as.factor(audio_feature),
                genre = as.factor(genre))

john <- Other_JM_Albums %>%
  tidyr::pivot_longer(cols = c(9:20),
                      names_to = "audio_feature",
                      values_to = "feature_value") %>%
  dplyr::mutate(audio_feature = as.factor(audio_feature),
                genre = as.factor(genre))

##### Boxplot comparing audio features of "Sob" and "Rock" songs #####
boxplotDF <- dplyr::bind_rows(album, john) %>%
  dplyr::filter(!audio_feature %in% c("key", "mode", "time_signature", "instrumentalness"))

boxplot <- ggplot(boxplotDF, aes(x = audio_feature, y = feature_value)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(color = genre)) +
  facet_wrap(vars(audio_feature), scales = "free", ncol = 2) + 
  theme_classic() +
  theme(axis.text.y = element_blank()) +
  coord_flip()

boxplot


#------------------------------------------------------------------------------#

##### Ridgeline Plot (to display distribution of several numeric variables) #####

ridgelineDF <- dplyr::bind_rows(Sob_Rock_Album, Other_JM_Albums)

ridgelineDF <- ridgelineDF %>%
  dplyr::select(-c('key', 'mode', 'time_signature')) %>%
  dplyr::mutate_at(.vars = c("loudness", "tempo"), .funs = normalize) %>%
  tidyr::pivot_longer(cols = c(9:17),
                      names_to = "Feature",
                      values_to = "Value")

ggplot(ridgelineDF, aes(x = Value, y = Feature, fill = genre)) +
  geom_density_ridges()

#------------------------------------------------------------------------------#

##### Radar Chart (to plot one+ series of values over multiple quantitative variables) #####

radarDF <- dplyr::bind_rows(Sob_Rock_Album, Other_JM_Albums)

playlist <- radarDF %>%
  dplyr::group_by(genre) %>%
  dplyr::summarise_at(vars(11:20), normalize) %>%
  dplyr::summarise_at(-1, mean)

playlist <- rbind(c("2", rep(0, 10)), playlist)
playlist <- rbind(c("1", rep(1, 10)), playlist)

playlist <- playlist %>%
  dplyr::mutate_at(c(1), as.factor) %>%
  dplyr::mutate_at(c(-1), as.numeric) %>%
  as.data.frame()

colors_fill <- c(scales::alpha("red", 0.2),
                 scales::alpha("blue", 0.2),
                 scales::alpha("green", 0.2))

colors_line <-  c(scales::alpha("darkred", 0.9),
                  scales::alpha("darkblue", 0.9),
                  scales::alpha("darkgreen", 0.9))

playlistDF <- playlist[, -1]
rownames(playlistDF) <- playlist[, 1]

radarchart(playlistDF, 
           seg = 10,  # Number of axis segments
           title = "Sad-Boy Mopery Chart",
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           plty = 1,
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 2)

# Add a legend
legend(x=0.75, 
       y=1.35, 
       legend = rownames(playlistDF[-c(1, 2), ]), 
       bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 1.5)

#------------------------------------------------------------------------------#

##### Scatter Plot on Valence/Energy Quadrants (to display the relationship bw 2 variables) #####

ggplot(john_mayer_all, aes(x = valence, y = energy, color = genre)) + 
  geom_point(size = 3) +
  geom_vline(xintercept = 0.5, colour = "gray", linetype = "longdash") +  
  geom_hline(yintercept = 0.5, colour = "gray", linetype = "longdash") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.30 / 2, 0.95, label = "Crazy Train Vibes") +
  annotate('text', 1.75 / 2, 0.95, label = "Heppy Heppy") +
  annotate('text', 1.75 / 2, 0.05, label = "Euphoria") +
  annotate('text', 0.25 / 2, 0.05, label = ":'(") +
  labs(x= "Valence", y= "Energy") +
  theme(plot.margin = margin(15,15,15,15))

#------------------------------------------------------------------------------#

#### Statistical Analysis - t-Test - Sob Rock vs. Other JM Albums####