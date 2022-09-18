#### Import Libraries ####
library(dplyr)
library(dotenv)
library(ggplot2)
library(httr)
library(rvest)
library(shiny)
library(SnowballC)
library(stringr)
library(tidyr)
library(tidytext)
library(textclean)
library(textstem)
library(tm)
library(xml2)

source("functions.R")

# load datasets
Sob_Rock_Album <- readRDS("sobrockalbum.rds")
Other_JM_Albums <- readRDS("otherjmalbums.rds")
Sob_Rock_Playlist <- readRDS("sobrockplaylist.rds")

'###############################################################################'
#### Genius API Connection ####

load_dot_env()

# App Client ID and Client Secret
clientID = Sys.getenv("GENIUS_ID")
clientSecret = Sys.getenv("GENIUS_SECRET")

# Token
token <- Sys.getenv("GENIUS_TOKEN")

if (length(token) > 0) {
  message("Genius Connection Ready")
}

'###############################################################################'

#### Lyric Analysis Part 1 - SobRock Album ####

##### Compile Genius URLs  ####

# Get Artist ID from Genius
genius_jm <- geniusGetArtist("John Mayer")

# Retrieve Artist's Track URL
baseURL <- 'https://api.genius.com/artists/'
requestURL <- paste0(baseURL, genius_jm$artist_id[1], '/songs')

track_lyric_urls <- list()

i <- 1
while (i > 0) {
  tmp <- httr::GET(requestURL,
                   query = list(access_token = token,
                                per_page = 50, page = i)) %>%
    httr::content() %>%
    .$response
  
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

# URL of Songs in SobRock Album
sobrock_album_urls <- data.frame(title = character(),
                                 url = character(),
                                 stringsAsFactors = FALSE)

for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$artist_names == "John Mayer") {
    if (track_lyric_urls[[i]]$title %in% Sob_Rock_Album$name) {
      df = data.frame(title = track_lyric_urls[[i]]$title,
                      url = track_lyric_urls[[i]]$url)
      
      sobrock_album_urls <- rbind(sobrock_album_urls, df)
    }
  }
}

# missing one song - Shouldn't Matter But it Does, ironically enough
missing_song <- data.frame(title = track_lyric_urls[[274]]$title, 
                           url = track_lyric_urls[[274]]$url)

sobrock_album_urls <- rbind(sobrock_album_urls, missing_song)

sobrock_album_urls$url <- as.character(sobrock_album_urls$url)

# URL of Songs in Sob Rock Playlist

# sobrock_playlist_urls <- lapply(setNames(Sob_Rock_Playlist$artist, Sob_Rock_Playlist$name), geniusGetURL, Sob_Rock_Playlist$name)
# sobrock_playlist_urls <- lapply(Sob_Rock_Playlist$artist, geniusGetURL, Sob_Rock_Playlist$name)

sobrock_playlist_urls <- Sob_Rock_Playlist %>%
  dplyr::mutate(url = gsub('[[:punct:]]+', '', name)) %>%
  dplyr::mutate(url = paste0("https://genius.com/", paste(.$artist, .$name, sep = "-"))) %>%
  dplyr::mutate(url = str_replace_all(url, " - ", " ")) %>%
  dplyr::mutate(url = str_replace_all(url, " ", "-")) %>%
  dplyr::mutate(url = paste0(url, "-lyrics")) %>%
  dplyr::select(title = name, url, genre)


# URL of Songs in other John Mayer Albums

# other_album_urls <- subset(track_lyric_urls, !(title %in% Sob_Rock_Album$name))
# other_album_urls <- subset(other_album_tracks, primary_artist$name == "John Mayer")

# other_album_urls <- track_lyric_urls[which(!sapply(track_lyric_urls, '[[', 'title') %in% Sob_Rock_Album$name)]
# other_album_urls <- other_album_urls[which(sapply(track_lyric_urls, '[[', 'artist_names') == 'John Mayer')]

other_album_urls <- Other_JM_Albums %>%
  dplyr::mutate(url = paste0("https://genius.com/", paste(.$artist, .$name, sep = "-"))) %>%
  dplyr::mutate(url = str_replace_all(url, " ", "-")) %>%
  dplyr::select(title = name, url, genre)


##### Retrieve Lyrics from SobRock Album ####

'
Read this about lemmatization
https://stackoverflow.com/questions/42522024/does-stemdocument-in-r-remove-ed-ending

https://towardsdatascience.com/stemming-vs-lemmatization-2daddabcb221

Instead of tm::_map(stemDocument) use SnowballC::wordStem(, language = "english")

Stemming: 
  - eds removed from word without regard for the meaning or context
  - reduces past-tense or plurals to the singular
  - simply trims back to the assumed root
  - use SnowballC package
  
Lemmatizing:
  - true root of the word is the lexeme
  - sometimes this is lost in stemming
  - lemmatization uses context to decide what the meaning of the root is
  - use Wordnet package
  - use textstem
'

###### Apply getLyrics function #####
sobrock_lyrics <- stack(lapply(setNames(sobrock_album_urls$url, sobrock_album_urls$title), getLyrics)) %>%
  select("title" = 2, "lyrics" = 1)

# sobrock_lyrics <- stack(lapply(setNames(sobrock_album_urls$url, sobrock_album_urls$title), cleanLyrics))

###### Cleaning using tidytext, textclean ####
library(tidytext)
library(textclean) # for contractions
library(wordcloud) 

'
tidytext::unnest_tokens()

unnest_tokens splits a column of lyrics into tokens
- one token per row
- strips punctuation
- words converted to lowercase
- also allows for different units of tokenizing (ngrams, skip_ngrams)
- only issue is that the function does not handle contractions
'

tidy_lyrics <- sobrock_lyrics %>%
  dplyr::mutate(lyrics = textclean::replace_contraction(lyrics, contraction.key = lexicon::key_contractions)) %>%
  tidytext::unnest_tokens(output = "word", input = "lyrics") %>%
  dplyr::anti_join(tidytext::get_stopwords()) %>%
  dplyr::filter(!word %in% c("ooh", "ah", "da", "oh", "uh")) # remove oohs and ahs

# count of words
tidy_lyrics %>%
  dplyr::count(word, sort = TRUE)

##### Wordcloud ####
wordcloud_df <- tidy_lyrics %>%
  dplyr::count(word)

wordcloud(wordcloud_df$word, wordcloud_df$n, max.words = 50)

'
Zipfs law states that the frequency that a word appears is inversely proportional to its rank
'

term_freq <- tidy_lyrics %>%
  dplyr::count(word, sort=TRUE) %>%
  dplyr::mutate(TotalWords = sum(n),
                rank = row_number(),
                tf = n / TotalWords)

term_freq %>%
  ggplot(aes(x = rank, y = tf)) + 
  geom_line(size = 1, show.legend = FALSE) + 
  scale_x_log10() + 
  scale_y_log10()

##### TD-IDF ####

'
bind_tf_idf() adds tf-idf values to a tidy text dataset

Top tf-idf values are not affected by stop words as IDF values of such stop words are very small
(due to their presence in almost every document)
'

tfidf_df <- tidy_lyrics %>%
  dplyr::group_by(title) %>%
  dplyr::count(word, sort=TRUE) %>%
  dplyr::arrange(title)
  
tfidf_df <- tfidf_df %>%
  tidytext::bind_tf_idf(term = word,
                        document = title, 
                        n = n) %>%
  dplyr::arrange(desc(tf_idf))

# plot results
plot_tfidf <- tfidf_df %>%
  dplyr::group_by(title) %>%
  dplyr::slice_head(n = 5)

ggplot(plot_tfidf, aes(x = reorder(word, tf_idf), y = tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  facet_wrap( ~ title, ncol = 3, scales = "free") + 
  labs(x = NULL, y = "TF-IDF")

##### Quantifying Sentiment ####

'
Join tidy_lyrics with lexicon of sad words
'

library(textdata)

sad_words <- tidytext::get_sentiments("nrc") %>%
  dplyr::filter(sentiment == 'sadness') %>%
  dplyr::select(word) %>%
  dplyr::mutate(sad = TRUE)

sentiments <- tidy_lyrics %>%
  dplyr::left_join(sad_words, by = 'word') %>%
  dplyr::group_by(title) %>%
  dplyr::summarise(sadness = round(sum(sad, na.rm = TRUE) / n(), 2),
                   word_count = n()) %>%
  dplyr::select(title, sadness) %>%
  dplyr::arrange(desc(sadness))

##### Lyrical Density #### 

'
Lyrical Density - the number of lyrics per song over the track length 

- how important lyrics are to a given song
- maybe a measure of "potency" 
'

library(scales)

lyr_dens_df <- Sob_Rock_Album %>%
  dplyr::left_join(sentiments, by = c("name" = "title"))
