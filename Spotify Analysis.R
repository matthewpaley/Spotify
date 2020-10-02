library(spotifyr)
library(tidyverse)
library(ggpubr)
library(ggridges)
library(plotly)
library(htmlwidgets)
library(httr)
library(corrplot)

Sys.setenv(SPOTIFY_CLIENT_ID = '5550202656634c518231763c0dc43058')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '0119634663a5465ab14553a00802a4b1')
username <- "1245595776"
redirect_uri <- 'http://localhost:1410/'

access_token <- get_spotify_access_token()


## Decades
my_playlists <- get_user_playlists(username)

playlists <- my_playlists %>%
  filter(name %in% c('Today\'s Top Hits',
                     'All Out 10s', 'All Out 00s',
                     'All Out 90s', 'All Out 80s',
                     'All Out 70s', 'All Out 60s'))

## Create tracklist of songs in playlists
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

tracklist <- tracklist %>% mutate(
  case_when(playlist_name == 'Today\'s Top Hits' ~ 7,
            playlist_name == 'All Out 10s' ~ 6,
            playlist_name == 'All Out 00s' ~ 5,
            playlist_name == 'All Out 90s' ~ 4,
            playlist_name == 'All Out 80s' ~ 3,
            playlist_name == 'All Out 70s' ~ 2, 
            playlist_name == 'All Out 60s' ~ 1)
)

# Add column to sort playlists in chronological order
tracklist$pnum <- NA
tracklist$pnum <- with(tracklist,
                       ifelse(playlist_name == 'Today\'s Top Hits', 7,
                              ifelse(playlist_name == 'All Out 10s', 6,
                                     ifelse(playlist_name == 'All Out 00s', 5,
                                            ifelse(playlist_name == 'All Out 90s', 4,
                                                   ifelse(playlist_name == 'All Out 80s', 3,
                                                          ifelse(playlist_name == 'All Out 70s', 2, 1)))))))

# Get feature means
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

# Sort chronologically
tracklist <- transform(tracklist, playlist_name = reorder(playlist_name, pnum))

## Create Graphs

# Danceability
g1 <- ggplot(tracklist, aes(x = danceability, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(danceability)), linetype = "dashed")

# Energy
g2 <- ggplot(tracklist, aes(x = energy, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(energy)), linetype = "dashed")

# Loudness
g3 <- ggplot(tracklist, aes(x = loudness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(loudness)), linetype = "dashed")

# Speechiness
g4 <- ggplot(tracklist, aes(x = speechiness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  xlim(0.01,0.075) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(speechiness)), linetype = "dashed")

# Acousticness
g5 <- ggplot(tracklist, aes(x = acousticness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(acousticness)), linetype = "dashed")

# Instrumentalness
g6 <- ggplot(tracklist, aes(x = instrumentalness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  xlim(0,.00002) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(instrumentalness)), linetype = "dashed")

# Liveness
g7 <- ggplot(tracklist, aes(x = liveness, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  xlim(0,0.3) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(liveness)), linetype = "dashed")

# Valence
g8 <- ggplot(tracklist, aes(x = valence, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(valence)), linetype = "dashed")

# Tempo
g9 <- ggplot(tracklist, aes(x = tempo, 
                            y = fct_reorder(playlist_name, pnum), 
                            fill = playlist_name)) + 
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_vline(aes(xintercept = median(tempo)), linetype = "dashed")

# Combine all plots into one
plot <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9) 
title <- expression(atop("Comparing the Decades", 
                         atop(italic("How Music has Changed Over Time"), "")))

# Add title, subtitle, and axis title
plot <- annotate_figure(plot,
                        top = text_grob(title, size = 18),
                        bottom = text_grob("Created by Matthew Paley",
                                           hjust = 1.5, x = 1, face = "italic", size = 10))

plot


## Create interactive plot with song preview

# Get needed columns
tlist <- tracklist %>% 
  mutate(artist = sapply(track.artists, "[[", 3),
         track.length = track.duration_ms/60000) %>% 
  select(track.name, playlist_name, artist, 
         track.length, track.popularity,
         track.preview_url, pnum,
         energy, valence, tempo, danceability,
         acousticness, liveness, speechiness,
         loudness, instrumentalness)

# See how features are correlated
tlist_cor <- tlist %>% select(-c(1:3,5,6))
corrplot(cor(tlist_cor, use="complete.obs", method="kendall"))
cov(tlist_cor)

# Select the 250 most popular songs in tracklist
top250 <- tlist %>% 
  arrange(-track.popularity) %>% 
  slice_head(n=250)

# Plot   
p <- ggplot(top250, aes(energy, 
                               valence,
                               colour = pnum,
                               text = paste0("Song: ", track.name, 
                                             "\nArtist: ", artist,
                                             "\nPlaylist: ", playlist_name, 
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
    title = "Data You Can Hear",
    subtitle = "Click a point to hear the song!") + 
  theme(legend.position = "none")

p1 <- ggplotly(p, tooltip="text") %>%
  layout(title = list(text = paste0('Data You Can Hear',
                                    '<br>',
                                    '<sup>',
                                    'Click on a point to listen to a song preview!',
                                    '</sup>')))

p1$x$data[[1]]$customdata <- top250$track.preview_url

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

