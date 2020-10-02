
Music Exploration Using SpotifyR
--------------------------------

In this blog I will demonstrate how I used the R programming language and the spotifyr package to compare music across the decades. Special thanks to Charlie Thompson for creating [spotifyr](https://www.rcharlie.com/spotifyr/).

### Step 1: Get Your Spotify Credentials

To get your Spotify credentials, you need to create a Spotify developer account and set up an app [here](https://developer.spotify.com/my-applications/#!/applications).

From there, you will get your `CLIENT_ID` and `CLIENT_SECRET`. Make note of these, as they will be needed in your code soon. Also, make sure to set your app's **Redirect URI** to `http://localhost:1410/`. Certain functions in the `spotifyr` package require you to be logged in as a Spotify user, and this allows you to do that. Now that we're all setup in Spotify, let's start working in R. Lastly, you will need your numeric username, which can be found [here](https://www.spotify.com/us/account/overview/).

### Step 2: Load Your Libraries

The libraries needed for this project are listed below. If you don't already have them, you can install them with `install.packages()`:

    library(spotifyr)
    library(tidyverse)
    library(ggpubr)
    library(ggridges)
    library(plotly)
    library(htmlwidgets)
    library(httr)
    library(corrplot)

### Step 3: Connect R to the Spotify API

This is where your **CLIENT\_ID** and **CLIENT\_SECRET** are needed, as well as your username. Replace the x's below with your credentials.

    Sys.setenv(SPOTIFY_CLIENT_ID = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
    Sys.setenv(SPOTIFY_CLIENT_SECRET = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
    username <- "xxxxxxxxxx"
    access_token <- get_spotify_access_token()

Step 4: The Fun Stuff
---------------------

Now that we're connected to the Spotify API, we can start digging into the data. The `get_user_playlists()` function returns all the playlists in your Spotify library. I filtered to the decades playlists and then used the `get_playlist_tracks` function to return the songs in each playlist, and the `get_track_audio_features()` function to get details about each song. Then I combined all songs into a dataframe, called **tracklist**.

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

Now that we have our tracklist, we'll use the `mutate()` and `case_when()` functions to add another column. The `pnum` column will be used to chronologically sort our tracklist by decade.

    tracklist <- tracklist %>% 
    mutate(pnum = 
      case_when(playlist_name == 'Today\'s Top Hits' ~ 7,
                playlist_name == 'All Out 10s' ~ 6,
                playlist_name == 'All Out 00s' ~ 5,
                playlist_name == 'All Out 90s' ~ 4,
                playlist_name == 'All Out 80s' ~ 3,
                playlist_name == 'All Out 70s' ~ 2, 
                playlist_name == 'All Out 60s' ~ 1
                )
    ) %>% arrange(pnum)

The last step before we start plotting is to create some features. The [Spotify API](https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/) goes into more detail about what each of these track features mean. For our project, we'll just use the averages of each.

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

Now that our features are ready to go, it's time to get plotting. We'll start by plotting danceability using `ggplot2` and `ggridges` to see how the danceability of music varies across the different decade playlists.

    # Danceability
    g1 <- ggplot(tracklist, aes(x = danceability, 
                                y = fct_reorder(playlist_name, pnum), 
                                fill = playlist_name)) + 
      geom_density_ridges() +
      theme_ridges() +
      theme(legend.position = "none",
            axis.title.y = element_blank()) +
      geom_vline(aes(xintercept = median(danceability)), linetype = "dashed")

![](https://github.com/matthewpaley/Spotify2/tree/master/images/Danceability.jpeg) Right away we can see a trend. Music has been increasaing in danceability pretty steadily each decade. I wonder what trends we can with the other features?

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

Once all plots have been created, we can combine them together to get the full picture.

    # Combine all plots into one
    plot <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9) 
    title <- expression(atop("Comparing the Decades", 
                             atop(italic("How Music has Changed Over Time"), "")))

    # Add title, subtitle, and axis title
    plot <- annotate_figure(plot,
                            top = text_grob(title, size = 18),
                            bottom = text_grob("Created by Matthew Paley",
                                               hjust = 1.5, x = 1, face = "italic", size = 10))

![](https://github.com/matthewpaley/Spotify2/tree/master/images/All%20Feature.jpeg) Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
