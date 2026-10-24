# Re-runs the Rmd's computations and writes aggregates only to findings.json.
# No lyric text leaves this script; the page reads the JSON.
suppressMessages({ library(dplyr); library(tidyr); library(tidytext); library(syuzhet); library(jsonlite) })

read_lyrics <- function(p) {
  d <- read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")
  d$lyrics <- iconv(d$lyrics, "UTF-8", "UTF-8", sub = " ")
  d$title <- iconv(d$title, "UTF-8", "UTF-8", sub = "")
  d
}
alb <- read_lyrics("data/clean/sobrock_album_lyrics.csv")
pl  <- read_lyrics("data/clean/sobrock_playlist_lyrics.csv")
jm  <- read_lyrics("data/clean/jm_vs_sobrock.csv")

tj <- read_lyrics("data/clean/tom_and_jerry_lyrics.csv")
pl <- pl %>% filter(!is.na(lyrics), trimws(lyrics) != "", genre %in% c("Sob", "Rock"))
corpus <- bind_rows(
  alb %>% mutate(grp = "Sob Rock"),
  pl  %>% mutate(grp = genre),
  jm  %>% filter(genre == "JohnMayer") %>% mutate(grp = "John Mayer (other)"),
  tj  %>% mutate(grp = "Tom and Jerry")
) %>% distinct(title, grp, .keep_all = TRUE)

# audio
clean_name <- function(d) { d$name <- iconv(d$name, "UTF-8", "UTF-8", sub = ""); d }
aud <- bind_rows(
  read.csv("data/sob_rock_album.csv", stringsAsFactors = FALSE) %>% clean_name() %>% mutate(grp = "Sob Rock"),
  read.csv("data/sob_rock_playlist.csv", stringsAsFactors = FALSE) %>% clean_name() %>% mutate(grp = genre)
)
feats <- c("acousticness","danceability","energy","instrumentalness","liveliness","loudness","speechiness","valence","tempo")
audio_tests <- bind_rows(lapply(feats, function(f) {
  sr <- aud[[f]][aud$grp == "Sob Rock"]
  data.frame(feature = f,
             mean_sobrock = mean(sr), mean_sob = mean(aud[[f]][aud$grp == "Sob"]), mean_rock = mean(aud[[f]][aud$grp == "Rock"]),
             p_vs_sob = t.test(sr, aud[[f]][aud$grp == "Sob"])$p.value,
             p_vs_rock = t.test(sr, aud[[f]][aud$grp == "Rock"])$p.value)
}))
audio_pts <- aud %>% filter(grp %in% c("Sob Rock", "Sob", "Rock")) %>%
  transmute(title = name, grp, energy, valence, loudness, danceability, acousticness, tempo)

# lyrics
ck <- lexicon::key_contractions; hl <- lexicon::hash_lemmas
expand_contractions <- function(x) { for (i in seq_len(nrow(ck))) x <- gsub(ck$contraction[i], ck$expanded[i], x, ignore.case = TRUE); x }
tidy_lyrics <- corpus %>%
  mutate(lyrics = expand_contractions(lyrics)) %>%
  unnest_tokens(word, lyrics) %>%
  left_join(hl, by = c("word" = "token")) %>%
  mutate(word = ifelse(is.na(lemma), word, lemma)) %>% select(-lemma) %>%
  anti_join(get_stopwords(), by = "word") %>%
  filter(!word %in% c("ooh","ah","da","oh","uh"))
nrc <- get_sentiment_dictionary("nrc")
sad_words <- nrc %>% filter(sentiment == "sadness") %>% distinct(word) %>% mutate(sad = TRUE)
sentiments <- tidy_lyrics %>% left_join(sad_words, by = "word") %>%
  group_by(title, grp) %>% summarise(sadness = sum(sad, na.rm = TRUE) / n(), words = n(), .groups = "drop")
sr <- sentiments$sadness[sentiments$grp == "Sob Rock"]
lyric_tests <- bind_rows(lapply(c("Sob","Rock","John Mayer (other)"), function(g)
  data.frame(comparison = g, p = t.test(sr, sentiments$sadness[sentiments$grp == g])$p.value)))
lyric_summary <- sentiments %>% group_by(grp) %>%
  summarise(n = n(), mean = mean(sadness), median = median(sadness), sd = sd(sadness), .groups = "drop")

per_track <- tidy_lyrics %>% inner_join(nrc, by = "word", relationship = "many-to-many") %>%
  count(title, grp, sentiment) %>% group_by(title, grp) %>% mutate(share = n / sum(n)) %>% ungroup() %>%
  select(title, grp, sentiment, share) %>% pivot_wider(names_from = sentiment, values_from = share, values_fill = 0)
emos <- c("anger","anticipation","disgust","fear","joy","negative","positive","sadness","surprise","trust")
emo_tests <- bind_rows(lapply(emos, function(e) {
  s <- per_track[[e]][per_track$grp == "Sob Rock"]
  data.frame(emotion = e,
             mean_sobrock = mean(s),
             mean_jm = mean(per_track[[e]][per_track$grp == "John Mayer (other)"]),
             mean_sob = mean(per_track[[e]][per_track$grp == "Sob"]),
             mean_rock = mean(per_track[[e]][per_track$grp == "Rock"]),
             p_vs_sob = t.test(s, per_track[[e]][per_track$grp == "Sob"])$p.value,
             p_vs_rock = t.test(s, per_track[[e]][per_track$grp == "Rock"])$p.value,
             p_vs_jm = t.test(s, per_track[[e]][per_track$grp == "John Mayer (other)"])$p.value)
}))

joined <- sentiments %>% inner_join(aud %>% select(name, grp, valence), by = c("title" = "name", "grp" = "grp"))
ct <- cor.test(joined$sadness, joined$valence)

ptile <- function(v, ref) mean(ref <= v)
tjv <- sentiments$sadness[sentiments$grp == "Tom and Jerry"]
out <- list(
  words = sentiments$words[sentiments$grp == "Tom and Jerry"],
  sadness = tjv,
  sadness_percentile = lapply(c("Sob Rock","Sob","Rock","John Mayer (other)"), function(g) ptile(tjv, sentiments$sadness[sentiments$grp == g])) %>% setNames(c("Sob Rock","Sob","Rock","John Mayer (other)")),
  group_median = lyric_summary %>% filter(grp != "Tom and Jerry") %>% select(grp, mean, median),
  emotions = bind_rows(lapply(emos, function(e) data.frame(emotion = e,
      tj = per_track[[e]][per_track$grp == "Tom and Jerry"],
      jm = mean(per_track[[e]][per_track$grp == "John Mayer (other)"]),
      sobrock = mean(per_track[[e]][per_track$grp == "Sob Rock"]),
      pct_jm = ptile(per_track[[e]][per_track$grp == "Tom and Jerry"], per_track[[e]][per_track$grp == "John Mayer (other)"]),
      pct_sobrock = ptile(per_track[[e]][per_track$grp == "Tom and Jerry"], per_track[[e]][per_track$grp == "Sob Rock"]))))
)
write_json(out, "tom_and_jerry.json", auto_unbox = TRUE, digits = 4, pretty = TRUE)
cat("ok\n")
