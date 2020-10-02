library(spotifyr)
library(geniusr)
library(tidyverse)
library(tibble)
library(knitr)
library(furrr)
library(ggplot2)
library(ggpubr)
library(ggjoy)
library(plotly)
library(htmlwidgets)
library(httr)
library(purrr)
library(forcats)
library(caTools)
library(randomForest)

Sys.setenv(SPOTIFY_CLIENT_ID = '5550202656634c518231763c0dc43058')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '0119634663a5465ab14553a00802a4b1')
username <- "1245595776"
redirect_uri <- 'http://localhost:1410/'

access_token <- get_spotify_access_token()

beatles <- get_artist_audio_features('the beatles')


features <- get_user_audio_features(username)
jack_johnson <- get_artist_audio_features('Jack Johnson')
jack_johnson<-jack_johnson[!(grepl("Live", jack_johnson$album_name)),]

jack_johnson %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(5) %>% 
  kable()

ggplot(jack_johnson, aes(x = valence, y = album_name)) + 
  geom_joy() + 
  theme_joy() +
  ggtitle("Plot of Jack Johnson's joy distributions")


##Dad vibes analysis for adam
my_playlists <- get_user_playlists(username, limit = 50)
dadVibes <- my_playlists[my_playlists$name == "Dad Vibes",]
dadVibesSongs <- list()
j <- 1
for(i in 0:(get_playlist(dadVibes$id)$tracks$total)%%100) { #num tracks to use for rbind
  song <- map_dfr(dadVibes$id, offset = i*100, get_playlist_tracks)
  dadVibesSongs[[j]] <- song
  j <- j + 1
}

dadVibesSongs <- do.call(rbind, dadVibesSongs)

DV <- map_dfr(dadVibesSongs$track.id, get_track_audio_features)
dadVibesFinal <- left_join(dadVibesSongs, DV, by = c("track.id" = "id")) %>% 
  mutate(artist = sapply(track.artists, "[[", 3),
         track.length = track.duration_ms/60000)

p <- ggplot(dadVibesFinal, aes(energy, 
                               valence,
                               colour = track.popularity,
                               text = paste0("Song: ", track.name, 
                                             "\nArtist: ", artist,
                                             "\nLength: ", paste0(
                                               ifelse(floor(track.length) < 10,
                                                      paste0("0",floor(track.length)),
                                                      floor(track.length)), ":",
                                               ceiling(track.length[1] %% 1*60)),
                                             ifelse(is.na(track.preview_url),
                                                    "\nPreview Unavailable",
                                                    "\nClick for Preview")))) + 
  geom_point() + 
  labs(
    title = "Dad Vibes",
    subtitle = "Click a point to hear the song!")

p1 <- ggplotly(p, tooltip=c("label","text")) %>%
  layout(title = list(text = paste0('Dad Vibes Analysis',
                                    '<br>',
                                    '<sup>',
                                    'Click the point to hear the song!',
                                    '</sup>')))

p1$x$data[[1]]$customdata <- dadVibesFinal$track.preview_url

p2 <- onRender(p1, "
                function(el, x) {
                el.on('plotly_click', function(d) {
                var url = d.points[0].customdata;
                //url
                window.open(url);
                });
                }
                ")
p2


##Grateful Dead analysis for Andy
my_playlists <- get_user_playlists(username, limit = 50)
gratefulDead <- my_playlists[my_playlists$name == "The Grateful Dead live: A complete chronological playlist",]
gratefulDeadSongs <- list()
j <- 1
for(i in 0:floor(get_playlist(gratefulDead$id)$tracks$total)) { #num tracks to use for rbind
  song <- map_dfr(gratefulDead$id, offset = i*100, get_playlist_tracks)
  gratefulDeadSongs[[j]] <- song
  j <- j + 1
}

gratefulDeadSongs <- do.call(rbind, gratefulDeadSongs)

GD <- rbind(map_dfr(gratefulDeadSongs$track.id[1:500], get_track_audio_features),
            map_dfr(gratefulDeadSongs$track.id[501:1000], get_track_audio_features),
            map_dfr(gratefulDeadSongs$track.id[1001:1500], get_track_audio_features),
            map_dfr(gratefulDeadSongs$track.id[1501:2000], get_track_audio_features),
            map_dfr(gratefulDeadSongs$track.id[2001:2500], get_track_audio_features),
            map_dfr(gratefulDeadSongs$track.id[2501:2769], get_track_audio_features))

gratefulDeadFinal <- left_join(gratefulDeadSongs, GD, by = c("track.id" = "id")) %>% 
  mutate(artist = sapply(track.artists, "[[", 3),
         track.length = track.duration_ms/60000)

b <- ggplot(gratefulDeadFinal, aes(instrumentalness, 
                               liveness,
                               colour = track.popularity,
                               text = paste0("Song: ", track.name, 
                                             "\nArtist: ", artist,
                                             "\nLength: ", paste0(
                                               ifelse(floor(track.length) < 10,
                                                      paste0("0",floor(track.length)),
                                                      floor(track.length)), ":",
                                               ceiling(track.length[1] %% 1*60)),
                                             ifelse(is.na(track.preview_url),
                                                    "\nPreview Unavailable",
                                                    "\nClick for Preview")))) + 
  geom_point() + 
  labs(
    title = "Grateful Dead Analysis",
    subtitle = "Click a point to hear the song!")

b1 <- ggplotly(b, tooltip="text") %>%
  layout(title = list(text = paste0('Grateful Dead Analysis',
                                    '<br>',
                                    '<sup>',
                                    'Click the point to hear the song!',
                                    '</sup>')),
         annotation_custom(text_grob("Created by Matthew Paley",
                                     hjust = 1.5, x = 1, face = "italic", size = 10)))

b1$x$data[[1]]$customdata <- gratefulDeadFinal$track.preview_url

b2 <- onRender(b1, "
               function(el, x) {
               el.on('plotly_click', function(d) {
               var url = d.points[0].customdata;
               //url
               window.open(url);
               });
               }
               ")
b2

## Testing
get_my_top_artists_or_tracks(type = 'tracks', time_range = 'medium_term', limit = 10) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  View()
  select(name, artist.name, album.name) %>% 
  kable()


get_my_recently_played() %>% 
  group_by(track.name) %>% 
  summarise(plays = n())

get_my_profile() %>% View()

## doesn't work
next_song <- function() {
  print(paste0("Skipping: ", get_my_currently_playing()))
  skip_my_playback()
  print(paste0("Playing: ", get_my_currently_playing()))
}

next_song()

## Decades
my_playlists <- get_user_playlists(username)

playlists <- my_playlists %>%
  filter(name %in% c('Today\'s Top Hits',
                     'All Out 10s', 'All Out 00s',
                     'All Out 90s', 'All Out 80s',
                     'All Out 70s', 'All Out 60s'))

tracklist <- list()

for(i in 1:nrow(playlists)) {
  tracks1 <- map_dfr(playlists$id[i], get_playlist_tracks)
  tracks1$playlist_name <- playlists$name[i]
  tracks1$playlist_id <- playlists$id[i]
  tracks2 <- map_dfr(tracks1$track.id, get_track_audio_features)
  tracks <- inner_join(tracks1, tracks2, by = c("track.id" = "id"))
  tracks <- inner_join(tracks, playlists, by = c("playlist_name" = "name",
                                                 "playlist_id" = "id"))
  tracklist[[i]] <- tracks
}

tracklist <- do.call(rbind, tracklist)

tracklist$pnum <- NA
tracklist$pnum <- with(tracklist,
     ifelse(playlist_name == 'Today\'s Top Hits', 7,
       ifelse(playlist_name == 'All Out 10s', 6,
       ifelse(playlist_name == 'All Out 00s', 5,
       ifelse(playlist_name == 'All Out 90s', 4,
       ifelse(playlist_name == 'All Out 80s', 3,
       ifelse(playlist_name == 'All Out 70s', 2, 1)))))))
                                    

tracks_avg <- tracklist %>%
  select(playlist_name, danceability, energy, 
         loudness, speechiness, acousticness,
         instrumentalness, liveness, valence, 
         tempo, duration_ms, track.popularity) %>%
  group_by(playlist_name) %>%
  summarise(danceability = mean(danceability),
            energy = mean(energy),
            loudness = mean(loudness),
            speechiness = mean(speechiness),
            acousticness = mean(acousticness),
            instrumentalness = mean(instrumentalness),
            liveness = mean(liveness),
            valence = mean(valence),
            tempo = mean(tempo),
            duration_ms = mean(duration_ms),
            track.popularity = mean(track.popularity))

tracklist <- transform(tracklist, playlist_name = reorder(playlist_name, pnum))

g1 <- ggplot(tracklist, aes(x = danceability, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_joy() +
  theme_joy() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(danceability)), linetype = "dashed")

g2 <- ggplot(tracklist, aes(x = energy, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_joy() +
  theme_joy() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(energy)), linetype = "dashed")

g3 <- ggplot(tracklist, aes(x = loudness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_joy() +
  theme_joy() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(loudness)), linetype = "dashed")

g4 <- ggplot(tracklist, aes(x = speechiness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  xlim(0.01,0.075) +
  geom_joy() +
  theme_joy() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(speechiness)), linetype = "dashed")

g5 <- ggplot(tracklist, aes(x = acousticness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_joy() +
  theme_joy() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(acousticness)), linetype = "dashed")

g6 <- ggplot(tracklist, aes(x = instrumentalness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  xlim(0,.00002) +
  geom_joy() +
  theme_joy() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(instrumentalness)), linetype = "dashed")

g7 <- ggplot(tracklist, aes(x = liveness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  xlim(0,0.3) +
  geom_joy() +
  theme_joy() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(liveness)), linetype = "dashed")

g8 <- ggplot(tracklist, aes(x = valence, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_joy() +
  theme_joy() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(valence)), linetype = "dashed")

g9 <- ggplot(tracklist, aes(x = tempo, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_joy() +
  theme_joy() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(tempo)), linetype = "dashed")

plot <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9) 
title <- expression(atop("Comparing the Decades", 
                         atop(italic("How Music has Changed Over Time"), "")))

plot <- annotate_figure(plot,
                        top = text_grob(title, size = 18),
                        bottom = text_grob("Created by Matthew Paley",
                                           hjust = 1.5, x = 1, face = "italic", size = 10),
                        left = text_grob("Playlist", size = 15, rot = 90))


plot


## Decade model
tracklist2 <- tracklist
tracklist2$decade <- NA
tracklist2$decade <- with(tracklist2,
                       ifelse(playlist_name == 'Today\'s Top Hits', "Current",
                              ifelse(playlist_name == 'All Out 10s', "2010s",
                                     ifelse(playlist_name == 'All Out 00s', "2000s",
                                            ifelse(playlist_name == 'All Out 90s', "90s",
                                                   ifelse(playlist_name == 'All Out 80s', "80s",
                                                          ifelse(playlist_name == 'All Out 70s', "70s", "60s")))))))
tracklist2 <- tracklist2 %>% 
  select(track.popularity, danceability, 
         energy, key, loudness, mode,
         speechiness, acousticness, instrumentalness,
         liveness, valence, tempo, duration_ms,
         time_signature, decade)

tracklist2$mode <- as.factor(tracklist2$mode)
tracklist2$time_signature <- as.factor(tracklist2$time_signature)
tracklist2$decade <- as.factor(tracklist2$decade)

set.seed(33)

sample = sample.split(tracklist2$decade, SplitRatio = .75)
train = subset(tracklist2, sample == TRUE)
test  = subset(tracklist2, sample == FALSE)

rf <- randomForest(decade ~ ., data = train)
pred <- predict(rf, test)

test2 <- test
test2$pred <- pred
View(test2)

test2$correct <- with(test2,
                      ifelse(decade == pred, 1, 0))
table(observed=test2$decade,predicted=test2$pred)

accuracy <- sum(test2$correct)/nrow(test2)

