## More Spotify Functions

get_my_top_artists <- function(time_range = 'medium_term', limit = 50, offset = 0, auth_code = get_spotify_authorization_code()) {
  
  res <- GET('https://api.spotify.com/v1/me/top/artists', config(token = auth_code), query = list(limit = limit, offset = offset, time_range = time_range)) %>% content %>% .$items
  
  map_df(1:length(res), function(i) {
    this_artist <- res[[i]]
    list(
      artist_name = this_artist$name,
      artist_uri = this_artist$id,
      artist_img = ifelse(length(this_artist$images) > 0, this_artist$images[[1]]$url, NA),
      artist_genres = list(unlist(this_artist$genres)),
      artist_popularity = this_artist$popularity,
      artist_num_followers = this_artist$followers$total,
      artist_spotify_url = this_artist$external_urls$spotify
    )
  })
}

get_my_top_tracks <- function(time_range = 'medium_term', limit = 50, offset = 0, auth_code = get_spotify_auth_code()) {
  
  res <- GET('https://api.spotify.com/v1/me/top/tracks', config(token = auth_code), query = list(limit = limit, offset = offset, time_range = time_range)) %>% content %>% .$items
  
  map_df(1:length(res), function(i) {
    this_track <- res[[i]]
    list(
      track_name = this_track$name,
      artist_name = this_track$artists[[1]]$name,
      album_name = this_track$album$name,
      album_type = this_track$album$album_type,
      track_number = this_track$track_number[[1]],
      track_popularity = this_track$popularity,
      explicit = this_track$explicit,
      album_release_date = this_track$album$release_date,
      album_img = this_track$album$images[[1]]$url,
      track_uri = this_track$id,
      artist_uri = this_track$artists[[1]]$id,
      album_uri = this_track$album$id,
      track_preview_url = ifelse(!is.null(this_track$preview_url), this_track$preview_url, NA),
      track_spotify_url = this_track$external_urls$spotify
    )
  })
}
