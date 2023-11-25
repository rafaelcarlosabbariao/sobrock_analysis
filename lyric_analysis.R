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
John_Mayer_Songs <- readRDS("john_mayer_songs.rds")
# saveRDS(John_Mayer_Albums, "john_mayer_songs.rds")

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

# Loop thru all songs returned by jm urls
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
                                 genre = character(),
                                 stringsAsFactors = FALSE)

for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$artist_names == "John Mayer") {
    if (track_lyric_urls[[i]]$title %in% Sob_Rock_Album$name) {
      df = data.frame(title = track_lyric_urls[[i]]$title,
                      url = track_lyric_urls[[i]]$url,
                      genre = "SobRock")
      
      sobrock_album_urls <- rbind(sobrock_album_urls, df)
    }
  }
}

# missing one song - Shouldn't Matter But it Does, ironically enough

missing_song <- data.frame(title = character(),
                           url = character(),
                           genre = character(),
                           stringsAsFactors = FALSE)

for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$title == "Shouldn’t Matter but It Does") {
    missing_song <- data.frame(title = track_lyric_urls[[i]]$title,
                               url = track_lyric_urls[[i]]$url,
                               genre = "SobRock")
    print("Found it.")
    sobrock_album_urls <- rbind(sobrock_album_urls, missing_song)
  }
}

sobrock_album_urls <- rbind(sobrock_album_urls, missing_song)

# manually update these artist names that have punctuation
Sob_Rock_Playlist[c(67, 70), ] <- mutate(Sob_Rock_Playlist[c(67, 70), ], artist = gsub('/', ' ', artist))

# manually update these songs that has punctuation
Sob_Rock_Playlist[c(15, 18), ] <- mutate(Sob_Rock_Playlist[c(15, 18), ], name = gsub(' - ', ' ', name))

# URL of Songs in Sob Rock Playlist
sobrock_playlist_urls <- Sob_Rock_Playlist %>%
  dplyr::mutate(artist = gsub('[[:punct:]]+', '', tolower(artist))) %>%  # remove punctuation from artist names like R.E.M. or Guns N' Roses
  dplyr::mutate(url = gsub("'", '', name)) %>%  # remove apostrophe from contractions, possessives first
  dplyr::mutate(url = gsub('\\(feat. .*)', '', tolower(url))) %>%  # remove any features in the name
  dplyr::mutate(url = gsub(' - .*', '', url)) %>%  # remove extra text for different versions
  dplyr::mutate(url = trimws(gsub('[[:punct:]]+', '', url))) %>%  # remove punctuation and trim white space 
  dplyr::mutate(url = paste0("https://genius.com/", paste(.$artist, .$url, sep = "-"))) %>%
  dplyr::mutate(url = str_replace_all(url, " ", "-")) %>%
  dplyr::mutate(url = paste0(url, "-lyrics")) %>%
  dplyr::select(title = name, url, genre)

# URL of Songs in other John Mayer Albums (excluding repeats and different versions, just keep album version)
remove <- c(90, 102, 154, 122, 166, 126, 156, 182, 106, 111, 131, 140, 108, 128, 59, 35, 157, 89, 100, 104, 124, 85, 109, 129, 135, 121, 143, 92, 112, 132, 167, 82, 120, 107, 127, 50, 99, 118, 65, 91, 141, 66, 161, 164, 80, 119, 98, 185, 93, 113, 133, 47, 168, 138, 117, 183, 81, 105, 125, 134, 155, 96, 79, 94, 115, 101, 61, 87, 97, 136, 158, 116, 83, 137, 48, 63, 58, 60, 103, 139, 62, 162, 20, 86, 110, 130, 163, 114)
remove <- sort(remove)
other_album_urls <- Other_JM_Albums[-remove,]

other_album_urls <- other_album_urls %>%
  dplyr::mutate(url = gsub("'", '', tolower(name))) %>%
  dplyr::mutate(url = gsub('\\(feat. .*)', '', url)) %>%
  dplyr::mutate(url = gsub(' - .*', '', url)) %>%
  dplyr::mutate(url = str_squish(gsub('[[:punct:]]+', '', url))) %>%
  dplyr::mutate(url = paste0("https://genius.com/", paste(.$artist, .$url, sep = "-"))) %>%
  dplyr::mutate(url = str_replace_all(url, " ", "-")) %>%
  dplyr::mutate(url = paste0(url, "-lyrics")) %>%
  dplyr::select(title = name, url, genre)

# let's bind these but mark Sob Rock songs as different to see if they really are
all_jm_songs <- rbind(other_album_urls, sobrock_album_urls)

##### Retrieve Lyrics from SobRock Album ####

###### Apply getLyrics function #####

# Sobrock album lyrics
sobrock_album_urls <- sobrock_album_urls %>%
  dplyr::mutate_at(c(1, 2), as.character)  # otherwise you get no applicable method error for read_xml to factor

sobrock_lyrics <- stack(lapply(setNames(sobrock_album_urls$url, sobrock_album_urls$title), getLyrics)) %>%
  select("title" = 2, "lyrics" = 1) %>%
  mutate("genre" = sobrock_album_urls$genre)

# # Sobrock playlist lyrics
# sobrock_playlist_lyrics <- stack(lapply(setNames(sobrock_playlist_urls$url, sobrock_playlist_urls$title), getLyrics)) %>%
#   select("title" = 2, "lyrics" = 1) %>%
#   mutate("genre" = sobrock_playlist_urls$genre)
# 
# sobrock_playlist_lyrics <- data.frame(title = character(),
#                                       lyrics = character(),
#                                       genre = character(), 
#                                       stringsAsFactors = FALSE)
# 
# for (i in 1:nrow(sobrock_playlist_urls)) {
#   tryCatch({
#     print(sobrock_playlist_urls$title[i])
#     sobrock_playlist_lyrics[i, "title"] <- sobrock_playlist_urls$title[i]
#     
#     print(sobrock_playlist_urls$url[i])
#     sobrock_playlist_lyrics[i, "lyrics"] <- getLyrics(sobrock_playlist_urls$url[i]) 
#     
#     print(sobrock_playlist_urls$genre[i])
#     sobrock_playlist_lyrics[i, "genre"] <- sobrock_playlist_urls$genre[i]
#   },
#   error = function(e) {
#     sobrock_playlist_lyrics[i, "title"] <- sobrock_playlist_urls$title[i]
#     sobrock_playlist_lyrics[i, "lyrics"] <- "HTTP error 404"
#     sobrock_playlist_lyrics[i, "genre"] <- sobrock_playlist_urls$genre[i]
#   })
# }
# 
# # JM lyrics
# # try to compile all John Mayer song lyrics one by one, if error, just put "HTTP error 404"
# jm_lyrics <- data.frame(title = character(),
#                         lyrics = character(),
#                         genre = character(), 
#                         stringsAsFactors = FALSE)
# 
# for (i in 1:nrow(all_jm_songs)) {
#   tryCatch({
#     print(all_jm_songs$title[i])
#     jm_lyrics[i, "title"] <- all_jm_songs$title[i]
#     
#     print(all_jm_songs$url[i])
#     jm_lyrics[i, "lyrics"] <- getLyrics(all_jm_songs$url[i]) 
#     
#     print(all_jm_songs$genre[i])
#     jm_lyrics[i, "genre"] <- all_jm_songs$genre[i]
#   },
#   error = function(e) {
#     jm_lyrics[i, "title"] <- all_jm_songs$title[i]
#     jm_lyrics[i, "lyrics"] <- "HTTP error 404"
#     jm_lyrics[i, "genre"] <- all_jm_songs$genre[i]
#   })
# }
# 
# jm_lyrics <- jm_lyrics[complete.cases(jm_lyrics), ]


# write.csv(sobrock_lyrics, "sobrock_album_lyrics.csv", row.names = FALSE)
# write.csv(sobrock_playlist_lyrics, "sobrock_playlist_lyrics.csv", row.names = FALSE)
# write.csv(jm_lyrics, "jm_vs_sobrock.csv", row.names = FALSE)

sobrock_playlist_lyrics <- 

###### Cleaning using tidytext, textclean ####
library(tidytext) # for tidy NLP methods
library(textclean) # for contractions
library(wordcloud) 

'
Read this about lemmatization
https://stackoverflow.com/questions/42522024/does-stemdocument-in-r-remove-ed-ending

https://towardsdatascience.com/stemming-vs-lemmatization-2daddabcb221

Instead of tm::_map(stemDocument) use SnowballC::wordStem(, language = "english")

Stemming: 
  - eds removed from word without regard for the meaning or context
  - reduces past-tense or plurals to the singular
  - simply trims back to the assumed root using suffix mapping rules
  - use SnowballC package
  - Options
    - tm::stemDocument() --> Porter Stemming
    - SnowballC::wordStem() --> Porter Stemming
  
Lemmatizing:
  - true root of the word is the lexeme
  - sometimes this is lost in stemming
  - lemmatization uses context to decide what the meaning of the root is
  - Uses vocabulary and morphological analysis 
  - use Wordnet package
  - use textstem
  - Options
    - wordnet::getLemma()
    - textstem::lemmatize_words() --> hash_lemmas

---

tidytext::unnest_tokens()

unnest_tokens splits a column of lyrics into tokens
- one token per row
- strips punctuation
- words converted to lowercase
- also allows for different units of tokenizing (ngrams, skip_ngrams)
- only issue is that the function does not handle contractions

This gives us TIDY DATA
- Each variable forms a column
- Each observation forms a row
- Each type of observational unit forms a table
'

tidy_lyrics <- sobrock_lyrics %>%
  dplyr::mutate(lyrics = textclean::replace_contraction(lyrics, contraction.key = lexicon::key_contractions)) %>%
  tidytext::unnest_tokens(output = "word", input = "lyrics") %>%
  textstem::lemmatize_words(word, dictionary = lexicon::hash_lemmas) %>%
  dplyr::anti_join(tidytext::get_stopwords()) %>%
  dplyr::filter(!word %in% c("ooh", "ah", "da", "oh", "uh")) # remove oohs and ahs

# count of words
tidy_lyrics %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::top_n(50)

##### Wordcloud ####
wordcloud_df <- tidy_lyrics %>%
  dplyr::count(word)

wordcloud(wordcloud_df$word, wordcloud_df$n, max.words = 50, 
          random.order = F, colors = brewer.pal(8, 'Dark2'))

'
As corny as it is, with the 3 most frequently used words of "JUST", "WANT", and "LOVE" adorns this album with some sort of a forthright desperation
that is as rock in its candidness as it is sad in its, well desperation. 

Looking into Zipfs Law provides a closer look into the making of this wordcloud.
'

##### Zipf's Law ####
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
tidytext::bind_tf_idf() adds tf-idf values to a TIDY TEXT DATASET

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

words <- tidytext::get_sentiments("nrc") 

unique(words$sentiment)

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

ggplot(sentiments) + 
  geom_col(aes(x = reorder(title, sadness), y = sadness, fill = title), show.legend = FALSE) + 
  coord_flip() + 
  labs(x = NULL)


##### Lyrical Density #### 

'
Lyrical Density - the number of lyrics per song over the track length 

- how important lyrics are to a given song
- maybe a measure of "potency" 
'

library(scales)

lyr_dens_df <- Sob_Rock_Album %>%
  dplyr::left_join(sentiments, by = c("name" = "title"))

'###############################################################################'

#### JM Songwriter ####
library(tensorflow)
library(reticulate)
# install_tensorflow(version = "2.10.0")
library(keras)
library(purrr)

# sobrock_lyrics as input
lyrics <- unlist(sobrock_lyrics$lyrics)

##### Preprocessing ####

# Use Keras Preprocessing layers to teokenize the text and turn it into arrays of numbers to be used as input
# Specify vocab size so tokenizer can map each word to a number from the lyrics
vocab_size <- 5000
oov_token <- "<OOV>"

tokenizer <- text_tokenizer(num_words = vocab_size, oov_token = oov_token)
fit_text_tokenizer(tokenizer, lyrics)
word_index = tokenizer$word_index

# transform the text into a sequence of integers
sequences <- texts_to_sequences(tokenizer, lyrics)

# n-gram sequences (could have also done this before when we unnest_tokens)
n_gram_sequences <- c()

# Let's use 
for (line in sequences) {
  for (index in 1:(length(line) - 7)) {
    n_gram_sequences <- append(n_gram_sequences, list(line[index:(index+7)]))
  }
}

length(n_gram_sequences)

n_gram_sequences[1:7]

# Need to find max length of any sequence and pad all other sequences to be the same length
maxlen <- 0

for (i in n_gram_sequences) {
  if (length(i) > maxlen) {
   maxlen <- length(i) 
  }
}

padded <- keras::pad_sequences(n_gram_sequences, maxlen)

# Now pad all sequences and set up labels
# For any sequence, the label is the last word
# So we can input one word and the model should learn the phrase that precedes it
labels <- padded[, ncol(padded)]

inputs <- padded[, (-ncol(padded))]

# one-hot encode the labels in the context of the vocabulary
encoded_labels <- keras::to_categorical(labels, num_classes = vocab_size)

##### Building and Training the Model #### 
'
Sequential model with Embedding, Dense and LSTM layers. Embedding layers are very
important for language and text models. 

https://towardsdatascience.com/deep-learning-4-embedding-layers-f9a02d55ac12
https://colah.github.io/posts/2015-08-Understanding-LSTMs/

https://blog.paperspace.com/bidirectional-rnn-keras/#:~:text=A%20Bidirectional%20RNN%20is%20a,the%20beginning%20of%20a%20sequence.
A Bidirectional RNN is a combination of two RNNs training the network in opposite
directions, one from the beginning to the end of a sequence, and the other, from
the end to the beginning of a sequence. 
'

model <- keras::keras_model_sequential()

model %>%
  layer_embedding(vocab_size, 64, input_length = maxlen-1) %>%
  bidirectional(layer_lstm(units = 20)) %>%
  layer_dense(units = vocab_size) %>%
  layer_activation('softmax') 

summary(model)

model %>%
  compile(optimizer = optimizer_adam(learning_rate = 0.05),
          loss = 'categorical_crossentropy',
          metrics = list('accuracy'))

history <- model %>%
  fit(inputs,
      encoded_labels,
      epoch = 100,
      batch_size = 400,
      verbose = 0)

history


model %>% evaluate(inputs, encoded_labels)

plot(history)

##### Generating Lyrics ####

write_lyric <- function(seed_text, length = 7) {
  for (i in 1:length + 1) {
    token_list <- texts_to_sequences(tokenizer, list(seed_text))
    padded <- pad_sequences(token_list, maxlen = maxlen - 1)
    pred_probs <- model %>% predict(padded)
    
    output_arg <- sample(seq_len(length(pred_probs)), 1, prob = pred_probs)
    
    output_word <- names(word_index)[output_arg]
    seed_text <- paste(seed_text, output_word)
  }
  return(seed_text)
}

write_lyric("just want love")

write_lyric("I am")
