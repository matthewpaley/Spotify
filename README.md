Music Through the Decades
================
Matthew Paley
10/2/2020

Music Exploration Using SpotifyR
--------------------------------

In this blog I will demonstrate how I used the R programming language and the spotifyr package to compare music across the decades. Special thanks to Charlie Thompson for creating [spotifyr](https://www.rcharlie.com/spotifyr/).

### Step 1: Get Your Spotify Credentials

To get your Spotify credentials, you need to create a Spotify developer account and set up an app [here](https://developer.spotify.com/my-applications/#!/applications).

From there, you will get your **CLIENT\_ID** and **CLIENT\_SECRET**. Make note of these, as they will be needed in your code soon. Also, make sure to set your app's **Redirect URI** to `http://localhost:1410/`. Certain functions in the **spotifyr** package require you to be logged in as a Spotify user, and this allows you to do that. Now that we're all setup in Spotify, let's start working in R.

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

This is where your **CLIENT\_ID** and **CLIENT\_SECRET** are needed. Replace the x's below with your credentials.

    Sys.setenv(SPOTIFY_CLIENT_ID = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
    Sys.setenv(SPOTIFY_CLIENT_SECRET = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")

    access_token <- get_spotify_access_token()

Including Plots
---------------

You can also embed plots, for example:

![](README_files/figure-markdown_github/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
