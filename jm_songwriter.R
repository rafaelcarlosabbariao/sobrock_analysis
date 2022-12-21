#### Import Libraries #### 
library(dotenv)
library(tidyverse)
library(httr)
library(tensorflow)
library(keras)

source("functions.R")

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

#### Get John Mayer Genius Lyrics #### 

# Get artist ID from Genius
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

# URLs of John Mayer Songs (where he is primary artist)
jm_songs_urls <- data.frame(title = character(),
                            url = character(),
                            stringsAsFactors = FALSE)

for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$artist_names == "John Mayer") {
    df = data.frame(title = track_lyric_urls[[i]]$title,
                    url = track_lyric_urls[[i]]$url)
    
     jm_songs_urls <- rbind(jm_songs_urls, df)
  }
}

# get rid of repeats
jm_songs_urls <- jm_songs_urls[-c(2,3,5:7,16,17,20,21,23,24,29,38,39,41:44,47:49,51,52,55:59,72,77,78,82,84,86,92,96,99,100,107,109:111,119,121,125:128,130:134,137:139,142:147,150,151,159,161,165,168,172,174,176:179,181,183,186,193,197,203,205,206,208,209,216,221:223,227,230:233),]

jm_songs_urls$url <- as.character(jm_songs_urls$url)

# apply getLyrics() function 
jm_lyrics <- lapply(setNames(jm_songs_urls$url, jm_songs_urls$title), getLyrics)

jm_lyrics <- stack(jm_lyrics)

jm_lyrics <- select(jm_lyrics, "title"=2, "lyrics"=1)

#### Clean Lyrics #### 
library(tidytext) # for tidy NLP methods
library(textclean) # for contractions
library(wordcloud)

tidy_lyrics <- jm_lyrics %>%
  dplyr::mutate(lyrics = textclean::replace_contraction(lyrics, contraction.key = lexicon::key_contractions)) %>%
  tidytext::unnest_tokens(output = "word", input = "lyrics") %>%
  textstem::lemmatize_words(word, dictionary = lexicon::hash_lemmas) %>%
  dplyr::anti_join(tidytext::get_stopwords()) %>%
  dplyr::filter(!word %in% c("oh", "da", "uh"))

# count of words
tidy_lyrics %>%
  count(word, sort=TRUE) %>%
  top_n(100)

# wordcloud
wordcloud_df <- tidy_lyrics %>%
  count(word)

wordcloud(wordcloud_df$word, wordcloud_df$n, max.words = 100,
          random.order = F, colors = brewer.pal(8, 'Dark2'))

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
