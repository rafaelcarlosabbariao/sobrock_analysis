#### Function to Retrieve Artist Album/Track Info ####
'
Returns album info and track features
'
getArtistData <- function(artistID) {
  
  # Request Artist Information
  URI = paste0('https://api.spotify.com/v1/artists/', artistID)
  artistRequest = httr::GET(url = URI, add_headers(Authorization = headerValue))
  artist = httr::content(artistRequest)
  
  message(paste0("Retrieving ", artist$name, " Data"))
  
  # Request Artist Album information
  URI = paste0('https://api.spotify.com/v1/artists/', artistID, '/albums')
  albumRequest = httr::GET(url = URI, add_headers(Authorization = headerValue))
  albums = httr::content(albumRequest)
  
  numAlbums = length(albums$items)
  albumList <- data.frame(
    name = character(numAlbums),
    id = character(numAlbums),
    type = character(numAlbums),
    total_tracks = character(numAlbums),
    release_date = character(numAlbums),
    stringsAsFactors = FALSE
  )
  
  for (i in seq(numAlbums)) {
    albumList[i,]$id <- unlist(albums$items[[i]]$id)
    albumList[i,]$name <- unlist(albums$items[[i]]$name)
    albumList[i,]$type <- unlist(albums$items[[i]]$type)
    albumList[i,]$total_tracks <- unlist(albums$items[[i]]$total_tracks)
    albumList[i,]$release_date <- unlist(albums$items[[i]]$release_date)
  }
  
  # Request Album and Track Information
  trackList <-data.frame(name=character(0),id=character(0),
                         artist=character(0),disc_number=numeric(0),
                         track_number=numeric(0),duration_ms=numeric(0),
                         explicit=character(0),album=character(0),
                         stringsAsFactors=FALSE)
  
  for (albumID in albumList$id) {
    URI = paste0('https://api.spotify.com/v1/albums/', albumID, '/tracks')
    trackRequest = httr::GET(url = URI, add_headers(Authorization = headerValue))
    tracks = httr::content(trackRequest)
    
    numTracks = length(tracks$items)
    
    trackList1 <- data.frame(
      name = character(numTracks),
      id = character(numTracks),
      artist = character(numTracks),
      disc_number = numeric(numTracks),
      track_number = numeric(numTracks),
      duration_ms = numeric(numTracks),
      explicit = character(numTracks),
      album = character(numTracks),
      stringsAsFactors = FALSE
    )
    
    for(i in seq(numTracks)) {
      trackList1[i,]$id <- unlist(tracks$items[[i]]$id)
      trackList1[i,]$name <- unlist(tracks$items[[i]]$name)
      trackList1[i,]$album <- albumList[albumList$id==albumID,]$name
      trackList1[i,]$artist <- unlist(tracks$items[[i]]$artists[[1]]$name)
      trackList1[i,]$disc_number <- unlist(tracks$items[[i]]$disc_number)
      trackList1[i,]$track_number <- unlist(tracks$items[[i]]$track_number)
      trackList1[i,]$duration_ms <- format(as.POSIXct(Sys.Date())+unlist(tracks$items[[i]]$duration_ms)/1000, "%M:%S")
    }
    
    trackList <- rbind(trackList, trackList1)
  }
  
  # Get Additional Track Features
  for (i in 1:nrow(trackList)) {
    Sys.sleep(0.10)  # back to back API requests leads to code capturing wrong response content
    URI = paste0('https://api.spotify.com/v1/audio-features/',
                 trackList$id[i])
    trackInfoRequest = httr::GET(url = URI,
                                 add_headers(Authorization = headerValue))
    trackFeatures = httr::content(trackInfoRequest)
    
    trackList$key[i] <- trackFeatures$key
    trackList$mode[i] <- trackFeatures$mode
    trackList$time_signature[i] <- trackFeatures$time_signature
    trackList$acousticness[i] <- trackFeatures$acousticness
    trackList$danceability[i] <- trackFeatures$danceability
    trackList$energy[i] <- trackFeatures$energy
    trackList$instrumentalness[i] <- trackFeatures$instrumentalness
    trackList$liveliness[i] <- trackFeatures$liveness
    trackList$loudness[i] <- trackFeatures$loudness
    trackList$speechiness[i] <- trackFeatures$speechiness
    trackList$valence[i] <- trackFeatures$valence
    trackList$tempo[i] <- trackFeatures$tempo
  }
  
  # For more detailed audio analysis
  # GET https://api.spotify.com/v1/audio-analysis/{id}
  
  message("Done")
  return(list("Albums" = albumList,
              "Tracks" = trackList))
}

'###############################################################################'

# Function to Retrieve Info from Spotify Playlist
'
Returns track features
'
getPlaylistInfo <- function(playlistID) {
  #playlistID = "2PYTcc4vVjuQJYbFbdgspr"
  URI = paste0("https://api.spotify.com/v1/playlists/", playlistID,
               "/tracks")
  playlistInfoRequest <- httr::GET(url = URI,
                                   add_headers(Authorization = headerValue))
  playlistInfo = httr::content(playlistInfoRequest)
  
  numTracks = playlistInfo$total
  playlistTracks <- data.frame(
    name = character(numTracks),
    id = character(numTracks),
    artist = character(numTracks),
    disc_number = numeric(numTracks),
    track_number = numeric(numTracks),
    duration_ms = numeric(numTracks),
    explicit = character(numTracks),
    album = character(numTracks),
    stringsAsFactors = FALSE
  )
  
  for (i in seq(numTracks)) {
    playlistTracks$name[i] <- unlist(playlistInfo$items[[i]]$track$name)
    playlistTracks$id[i] <- unlist(playlistInfo$items[[i]]$track$id)
    playlistTracks$artist[i] <- unlist(playlistInfo$items[[i]]$track$artists[[1]]$name)
    playlistTracks$disc_number[i] <- unlist(playlistInfo$items[[i]]$track$disc_number)
    playlistTracks$track_number[i] <- unlist(playlistInfo$items[[i]]$track$track_number)
    playlistTracks$duration_ms[i] <- format(as.POSIXct(Sys.Date())+unlist((playlistInfo$items[[i]]$track$duration_ms))/1000, "%M:%S")
    playlistTracks$explicit[i] <- unlist(playlistInfo$items[[i]]$track$explicit)
    playlistTracks$album[i] <- unlist(playlistInfo$items[[i]]$track$album$name)
  }
  
  # Get Additional Track Features
  for (i in 1:nrow(playlistTracks)) {
    Sys.sleep(0.10)  # back to back API requests leads to code capturing wrong response content
    URI = paste0('https://api.spotify.com/v1/audio-features/',
                 playlistTracks$id[i])
    trackInfoRequest = httr::GET(url = URI,
                                 add_headers(Authorization = headerValue))
    trackFeatures = httr::content(trackInfoRequest)
    
    playlistTracks$key[i] <- trackFeatures$key
    playlistTracks$mode[i] <- trackFeatures$mode
    playlistTracks$time_signature[i] <- trackFeatures$time_signature
    playlistTracks$acousticness[i] <- trackFeatures$acousticness
    playlistTracks$danceability[i] <- trackFeatures$danceability
    playlistTracks$energy[i] <- trackFeatures$energy
    playlistTracks$instrumentalness[i] <- trackFeatures$instrumentalness
    playlistTracks$liveliness[i] <- trackFeatures$liveness
    playlistTracks$loudness[i] <- trackFeatures$loudness
    playlistTracks$speechiness[i] <- trackFeatures$speechiness
    playlistTracks$valence[i] <- trackFeatures$valence
    playlistTracks$tempo[i] <- trackFeatures$tempo
  }
  
  message("Done")
  return(playlistTracks)
}

'###############################################################################'

# Function to normalize data (values between 0 and 1)
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

'###############################################################################'

# Function to Retrieve Genius Artist Info
geniusGetArtist <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q='
  
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page', n_results,
                       '&access_token=', token)
  
  # res <- httr::GET(requestURL) %>% content %>% .$response %>% .$hits
  
  res <- httr::GET(requestURL) %>%
    httr::content(., as = "parsed") %>%
    # jsonlite::fromJSON(.) %>%
    .$response %>%
    .$hits
  
  purrr::map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    
    list(artist_id = tmp$id,
         artist_name = tmp$name) 
  }) %>% unique()
}

'###############################################################################'

# Function to generate url for given artist and song
geniusGetURL <- function(artist, song) {
  baseURL <- "https://genius.com/"
  
  query <- paste(artist, song, "lyrics", sep = "-") %>%
    str_replace_all("-", " ") %>%
    str_replace_all(" ", "-")
  
  url <- paste0(baseURL, query)
  return(url)
}

'###############################################################################'

# Function to Retrieve Genius Lyrics by Song
getLyrics <-  function(url) {
  divs <- rvest::read_html(url) %>%
    rvest::html_nodes(xpath = '//*[@id="lyrics-root"]/div[2]') %>%
    rvest::html_text()
  
  # remove verse|intro|outro and brackets, carriage returns replaced with spaces
  lyrics <- gsub("\\[[^\\]]*]", " ",
                 gsub("([^ ])([A-Z])", "\\1 \\2", divs[1]), perl=TRUE)
  
  return(lyrics)
}

# Function to Retrieve Genius Lyrics by Song & Clean using tm library
cleanLyrics <-  function(url) {
  divs <- rvest::read_html(url) %>%
    rvest::html_nodes(xpath = '//*[@id="lyrics-root"]/div[2]') %>%
    rvest::html_text()
  
  # remove verse|intro|outro and brackets, carriage returns replaced with spaces
  lyrics <- gsub("\\[[^\\]]*]", " ",
                 gsub("([^ ])([A-Z])", "\\1 \\2", divs[1]), perl=TRUE)
  
  corpus = tm::VCorpus(VectorSource(lyrics)) %>%
    tm::tm_map(tm::content_transformer(tolower)) %>%
    tm::tm_map(removeNumbers) %>% 
    tm::tm_map(tm::content_transformer(replace_contraction)) %>%
    tm::tm_map(removePunctuation) %>%
    tm::tm_map(removeWords, stopwords("english")) %>%
    tm::tm_map(textstem::lemmatize_words) %>%
    # tm::tm_map(tm::content_transformer(wordStem)) %>%
    # tm::tm_map(stemDocument) %>%
    tm::tm_map(stripWhitespace)
  
  dtm = tm::DocumentTermMatrix(corpus)
  
  dtm = tm::removeSparseTerms(dtm, 0.999)
  
  return(dtm[["dimnames"]][["Terms"]])
  
  # lemmatize_words(word, dictionary = lexicon::hash_lemmas)
  # return(SnowballC::wordStem(dtm[["dimnames"]][["Terms"]], language="english"))
}
  
'###############################################################################'
  

