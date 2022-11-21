#'*-----------------------------------------------------------------------------*'#
#'*--------------------------PLAYLIST CREATION PROJECT--------------------------*'#
#'*----------------------------------BEN CROSS----------------------------------*'#
#'*-----------------------------------------------------------------------------*'#

#----LOAD LIBRARIES------------------# ####

# Below is the code to install the tinyspotifyr package
# devtools::install_github("troyhernandez/tinyspotifyr")

suppressPackageStartupMessages({
  library(tinyspotifyr)
  library(knitr)
  library(httr)
  library(jsonlite)
  library(scales)
  library(dplyr)
  })

#----BASE SETLIST FM INFORMATION-----# ####

# For the API key you will need to get this from SetlistFM here i will give the 
# option for the user to enter the API_Key but it is better to save your own to
# the document when setting it up. Use the link https://api.setlist.fm/docs/1.0/index.html
# to apply for an API key.

# API_Key <- readline(prompt="Enter SetlistFM API Key: ")

API_Key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" # REPLACE!!!!!!!!!

base <- "https://api.setlist.fm/rest"

#----BASE SPOTIFY INFORMATION--------# ####

# Similar to the SetlistFM info you will need to set up a spotify developer account
# to get this information. A quick google search will be enough to figure out how
# to do this. The information you need is the client_id, client_secret and 
# your spotify username. Again it is better to hard code this information rather
# than pasting every time. Use the link to get to spotify developer mode 
# https://developer.spotify.com/

# spotify_username <- readline(prompt="Enter Spotify Username: ")
# client_id <- readline(prompt="Enter Spotify Client ID: ")
# client_secret <- readline(prompt="Enter Spotify Client Secret: ")

spotify_username <- "xxxxxxxxxxxxxxxxx"             # REPLACE!!!!!!!!!!
client_id <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"     # REPLACE!!!!!!!!!!
client_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"      # REPLACE!!!!!!!

Sys.setenv(SPOTIFY_CLIENT_ID = client_id)         # Sets the client id
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret) # Sets the client secret
access_token <- get_spotify_access_token()        # Gets the access token, will launch browser

#----ARTIST INFORMATION FUNCTION-----# ####
getArtistInfo <- function(artist_name){
  #' This function takes an artists name and returns a table containing all of the artists
  #' which contain that name on setlist fm
  #' 
  #' @param artist_name - An artists name wrapped in quotes.
  #' 
  #' @return a dataframe containing all of the artists on the site with that name.
  #' 
  
  # First i make a url using the artists name
  artist_name_url <- paste0(base, "/1.0/search/artists", "?artistName=", artist_name,
               "&sort=sortName")
  # I then use URLencode to make sure the url is formatted properly to deal with spaces etc.
  artist_name_url <- URLencode(artist_name_url)
  # We then retrieve the artist info using the API key i have added above
  artist_df <- GET(artist_name_url, add_headers("x-api-key" = API_Key))
  # Make sure to get the content
  artist_df <- suppressMessages(content(artist_df, "text"))
  #I then convert this to a dataframe, add a row number and then select the appropriate columns
  artist_df <- as.data.frame(fromJSON(artist_df)$artist) %>%
    mutate(artist_number = row_number()) %>%
    select(artist_number, name, mbid)
  #'*-----------------------------------------------------------------------------*'#
  #'*--------------------------PLAYLIST CREATION PROJECT--------------------------*'#
  #'*----------------------------------BEN CROSS----------------------------------*'#
  #'*-----------------------------------------------------------------------------*'#
  
  #----LOAD LIBRARIES------------------# ####
  
  # Below is the code to install the tinyspotifyr package
  # devtools::install_github("troyhernandez/tinyspotifyr")
  
  suppressPackageStartupMessages({
    library(tinyspotifyr)
    library(stringr)
    library(knitr)
    library(httr)
    library(jsonlite)
    library(scales)
    library(dplyr)
  })
  
  #----BASE SETLIST FM INFORMATION-----# ####
  
  # For the API key you will need to get this from SetlistFM here i will give the 
  # option for the user to enter the API_Key but it is better to save your own to
  # the document when setting it up. Use the link https://api.setlist.fm/docs/1.0/index.html
  # to apply for an API key.
  
  # API_Key <- readline(prompt="Enter SetlistFM API Key: ")
  
  API_Key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
  
  base <- "https://api.setlist.fm/rest"
  
  #----BASE SPOTIFY INFORMATION--------# ####
  
  # Similar to the SetlistFM info you will need to set up a spotify developer account
  # to get this information. A quick google search will be enough to figure out how
  # to do this. The information you need is the client_id, client_secret and 
  # your spotify username. Again it is better to hard code this information rather
  # than pasting every time. Use the link to get to spotify developer mode 
  # https://developer.spotify.com/
  
  # spotify_username <- readline(prompt="Enter Spotify Username: ")
  # client_id <- readline(prompt="Enter Spotify Client ID: ")
  # client_secret <- readline(prompt="Enter Spotify Client Secret: ")
  
  # Inititally set the variables and assign to environment variables shown below
  spotify_username <- "XXXXXXXXXXXXXXX"
  client_id <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
  client_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXX"
  
  access_token <- get_spotify_access_token(client_id,
                                           client_secret)        # Gets the access token, will launch browser
  
  #----ARTIST INFORMATION FUNCTION-----# ####
  getArtistInfo <- function(artist_name){
    #' This function takes an artists name and returns a table containing all of the artists
    #' which contain that name on setlist fm
    #' 
    #' @param artist_name - An artists name wrapped in quotes.
    #' 
    #' @return a dataframe containing all of the artists on the site with that name.
    #' 
    
    # First i make a url using the artists name
    artist_name_url <- paste0(base, "/1.0/search/artists", "?artistName=", artist_name,
                              "&sort=sortName")
    # I then use URLencode to make sure the url is formatted properly to deal with spaces etc.
    artist_name_url <- URLencode(artist_name_url)
    # We then retrieve the artist info using the API key i have added above
    artist_df <- GET(artist_name_url, add_headers("x-api-key" = API_Key))
    # Make sure to get the content
    artist_df <- suppressMessages(content(artist_df, "text"))
    #I then convert this to a dataframe, add a row number and then select the appropriate columns
    artist_df <- as.data.frame(fromJSON(artist_df)$artist) %>%
      arrange(str_length(name)) %>%
      mutate(artist_number = row_number()) %>%
      select(artist_number, name, mbid)
    
    # Check to see if more than 1 artist
    if(nrow(artist_df) == 1){
      # if only one artist then current artist is selected
      correct.artist <- artist_df[1,]
    } else {
      # Print out the table of artists
      print(kable(artist_df, row.names = F))
      cat("\n")
      # Allow the user to select the correct artist
      correct.artist <- readline(prompt = "Select the Correct Artist Number: ")
      # Get the row containing the correct artist
      correct.artist <- artist_df[correct.artist,]
    }
    
    # This dataframe is then returned as an object
    invisible(return(correct.artist))
  }
  
  #----RETURN THE SETLISTS FUNCTION----# ####
  getSetlistInfo <- function(artist_name, number_of_sets = 20){
    #' This is a function to take an artists name and return the setlists available for that artist
    #' up until a max of 20. If a setlist is returned later than the current date then it will be filtered
    #' out of the final setlist dataframe. The final input from the user will be the amount of setlists
    #' that they wish to return.
    #' 
    #' @param artist_name - The name of the artist you wish to search
    #' 
    #' 
    
    # Loops to complete
    loops <- ceiling(number_of_sets / 20)
    
    # Artist base URL
    artist_url <- "/1.0/artist/"
    
    # Get the artist info
    artist <- getArtistInfo(artist_name)[1,3]
    
    # Create an empty data frame
    venue_info <- data.frame()
    
    for(j in 1:loops){
      # Create the setlist url using the artist code from the artist data function
      setlists <- paste0(base, artist_url, artist, "/setlists?p=", j)
      # Get a list of at max, their last 20 shows
      setlist_list <- GET(setlists, add_headers("x-api-key" = API_Key))
      # Return the number of setlists within this list
      number_of_setlists <- as.data.frame(lengths(content(setlist_list)))['setlist',1]
      
      # Loop across the setlists and return various bits of information about the setlist and venue
      # then bind these together in a data frame appending each time. Pasting '' at the end of each
      # value so that it stores a value for it and not NULL if no data available.
      for(i in 1:number_of_setlists){
        new_data <- cbind('ID' = i,
                          'EventDate' = paste0(content(setlist_list)$setlist[[i]]$eventDate, ""),
                          'Country' = paste0(content(setlist_list)$setlist[[i]]$venue$city$country$name, ""),
                          'State' = paste0(content(setlist_list)$setlist[[i]]$venue$city$state, ""),
                          'City' = paste0(content(setlist_list)$setlist[[i]]$venue$city$name, ""),
                          'VenueName' = paste0(content(setlist_list)$setlist[[i]]$venue$name, ""),
                          'Latitude' = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$lat, ""),
                          'Longitude' = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$long, "")
        )
        venue_info <- rbind(venue_info, new_data)
      }
    }
    # Convert the date column to date format
    venue_info$EventDate <- strptime(venue_info$EventDate, format = "%d-%m-%Y")
    # Subset for sets before today and then add an ID number.
    venue_info <- subset(venue_info, as.Date(EventDate) < as.Date(Sys.Date())) %>%
      mutate(EventID = row_number()) %>%
      relocate(EventID)
    
    print(kable(venue_info[,-2], row.names = F))
    
    setlist.choice <- readline(prompt = "Select the Number of Sets to Include: ")
    
    sets <- venue_info[1:setlist.choice,2]
    
    info_needed <- list(content(setlist_list), sets)
    
    return(info_needed)
    
  }
  
  #----RETURN THE SONGS PLAYED---------# ####
  GetSongInfo <- function(artist_name, number_of_sets = 20){
    #' This function takes the artist name as an argument and applies it to the previous function,
    #' Which in turn applies it to the one before that.
    #' Once it has the artist name and the number of sets to choose from it then gathers all of
    #' the songs for each set and turns them into a dataframe ordered by the songs average location in
    #' the set, also including the probability that the song will be played.
    #' 
    #' @param artist_name - The name of the chosen artist to be passed through functions.
    #' 
    #' @return - a dataframe of song info.
    #' 
    
    # First use previous function and save the information returned
    info_needed <- getSetlistInfo(artist_name, number_of_sets)
    
    # Create a blank dataframe which can be used to store the results
    dataset <- data.frame()
    
    # Now, info_needed[[2]] is the SetID of the sets selected, we want to first create a loop across
    # all selected sets.
    for(set in as.numeric(info_needed[[2]])){
      # We need to get the number of sets within each set e.g. main stage, side stage, encore count as 3.
      b <- sum(lengths(info_needed[[1]]$setlist[[set]]$sets))
      # We only want to continue the process if each setlist has more than one set.
      if(b > 0){
        
        # The next loop initiated loops across all of the mini sets within the big sets and returns the number
        # of songs in each of these
        for(i in 1:b){
          # The number of songs in each mini-set
          c <- length(lengths(info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song))
          
          # The final nested loop looks at all the songs within each min-set and larger-set and returns their name
          # i have also included code for if there is a cover song. So we return the name of the cover artist also.
          for(j in 1:c){
            # Begin to create the new dataframe by combining the set number and song title.
            newdata <- cbind('set' = set,
                             'SongName' = info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song[[j]]$name)
            
            # Next i want to see if the current son gi sa cover song or not. First try and allocate to 't'
            t <- try(info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song[[j]]$cover$name)
            
            # If class of 't' is NULL then it is not a cover song, else it is a cover song and grab the artist name
            if("NULL" %in% class(t)){
              newdata <- cbind(newdata,
                               'Cover' = FALSE,
                               'OrigArtist' = 'N/A')
            } else {
              newdata <- cbind(newdata,
                               'Cover' = TRUE,
                               'OrigArtist' = info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song[[j]]$cover$name)
            }
            
            # Bind the new data and the old data together in a dataset.
            dataset <- rbind(dataset, newdata)
          }
        }
      }
    }
    
    # The next part of the function is to convert the dataset into a better format.
    dataset <- dataset %>%
      # First we remove any whitespace
      mutate(across(.cols = (2:4), ~trimws(.x))) %>%
      # Group by the set number
      group_by(set) %>%
      # Add a row number value for the song position
      mutate(song = row_number()) %>%
      ungroup() %>%
      # Then we want to group by the following to summarise
      group_by(SongName, Cover, OrigArtist) %>%
      # Here we want the songs average position, the number of times played and then the probability
      # that the song will be played.
      summarise(AvgPosition = round(mean(song), 2),
                TimesPlayed = n(),
                ProbPlay = length(unique(set)),
                .groups = 'drop') %>%
      # Then overwrite the ProbPlay column with actual probability
      mutate(ProbPlay = percent(TimesPlayed / max(ProbPlay), accuracy = 0.01)) %>%
      arrange(AvgPosition) %>%
      # Finally filter out blanks
      filter(nchar(trimws(SongName)) > 0)
    
    # The final thing is to print out the dataset, removing the cover columns if no covers have been
    # performed.
    if(sum(as.logical(dataset$Cover)) >= 1){
      print(kable(dataset, row.names = F))
    } else {
      print(kable(dataset %>% select(-Cover, -OrigArtist), row.names = F))
    }
    return(dataset)
  }
  
  #----ADD A PLAYLIST TO SPOTIFY-------# ####
  CreateSpotifyPlaylist <- function(artist_name, playlist_name = artist_name, public = FALSE){
    #' This function takes the output from the GetSongInfo function and uses it to create
    #' a playlist on the user's Spotify account so long as they have developer status and
    #' a client id and secret.
    #' 
    #' @param artist_name - The name of the artist you wish you search for. This is passed through
    #'                      the previous functions in a waterfall manner.
    #' @param playlist_name - The name of the new playlist you wish to create. It takes the default
    #'                        value the same as the artist name if nothing is entered.
    #' @param public - Whether you would like the playlist to be public or not, the default is FALSE
    #'
    
    # Firstly, run the code from the previous functions and save the result.
    SongInfo <- GetSongInfo(artist_name)
    
    # Create a playlist wrapped in the invisible function to prevent output
    invisible(create_playlist(spotify_username,
                              playlist_name,
                              public = public))
    
    # Find the playlist id of the first playlist with the name of the playlist. This part could use
    # some work to identify the proper playlist. Maybe saving output of the create playlist function...
    playlist_id <- get_my_playlists() %>%
      filter(name == playlist_name)
    
    playlist_id <- playlist_id$id[1]
    
    # Now we want to loop through all songs in the dataframe returned by the GetSongInfo function and
    # firstly alter the artist based on if the song is a cover, then find the id number of the song
    # followed by adding the song to the playlist if it is found. Again, this bit needs some work.
    # the search_spotify function doesn't search well for the song name so you get the top 50 (max)
    # songs by popularity by the artist in the search, if the song isn't in this top 50 then it
    # wont find a track id so i need to fix to cycle through all lists of 50 to find the song if needed.
    
    for(i in 1:nrow(SongInfo)){
      # Get the original artist
      OrigArtist <- SongInfo$OrigArtist[i]
      # If it is N/A then use the original artist name otherwise it is the cover artist
      if(OrigArtist == "N/A"){
        Artist <- artist_name
      } else {
        Artist <- OrigArtist
      }
      
      # Return the song name of current row
      SongName <- SongInfo$SongName[i]
      
      # Combine artist and song title for searching
      song <- paste(Artist, SongName)
      
      # Search Spotify and keep the rows with the selected song name
      Newdata <- search_spotify(song, type = 'track', limit = 50, market = "US")$tracks$items %>%
        filter(name == SongName)
      
      # Keep only the selected track_id
      track_id <- Newdata[1, 'uri']
      
      # If no track id then continue to next song, else add the song to the playlist
      if(is.na(track_id)){
        next
      } else {
        add_items_to_playlist(playlist_id, track_id)
      }
    }
  }
  
  #----TEST THE CODE-------------------# ####
  #CreateSpotifyPlaylist('Arctic Monkeys', 'Arctic Monkeys Gig')
  
  GetSongInfo('kid cudi', number_of_sets = 100)
  
  
  # Check to see if more than 1 artist
  if(nrow(artist_df) == 1){
    # if only one artist then current artist is selected
    correct.artist <- artist_df[1,]
  } else {
    # Print out the table of artists
    print(kable(artist_df, row.names = F))
    cat("\n")
    # Allow the user to select the correct artist
    correct.artist <- readline(prompt = "Select the Correct Artist Number: ")
    # Get the row containing the correct artist
    correct.artist <- artist_df[correct.artist,]
  }
  
  # This dataframe is then returned as an object
  invisible(return(correct.artist))
}

#----RETURN THE SETLISTS FUNCTION----# ####
getSetlistInfo <- function(artist_name){
  #' This is a function to take an artists name and return the setlists available for that artist
  #' up until a max of 20. If a setlist is returned later than the current date then it will be filtered
  #' out of the final setlist dataframe. The final input from the user will be the amount of setlists
  #' that they wish to return.
  #' 
  #' @param artist_name - The name of the artist you wish to search
  #' 
  #' 
  
  # Artist base URL
  artist_url <- "/1.0/artist/"
  # Create the setlist url using the artist code from the artist data function
  setlists <- paste0(base, artist_url, getArtistInfo(artist_name)[1,3], "/setlists")
  # Get a list of at max, their last 20 shows
  setlist_list <- GET(setlists, add_headers("x-api-key" = API_Key))
  # Return the number of setlists within this list
  number_of_setlists <- as.data.frame(lengths(content(setlist_list)))['setlist',1]
  # Create an empty data frame
  venue_info <- data.frame()
  # Loop across the setlists and return various bits of information about the setlist and venue
  # then bind these together in a data frame appending each time. Pasting '' at the end of each
  # value so that it stores a value for it and not NULL if no data available.
  for(i in 1:number_of_setlists){
    new_data <- cbind('ID' = i,
                      'EventDate' = paste0(content(setlist_list)$setlist[[i]]$eventDate, ""),
                      'Country' = paste0(content(setlist_list)$setlist[[i]]$venue$city$country$name, ""),
                      'State' = paste0(content(setlist_list)$setlist[[i]]$venue$city$state, ""),
                      'City' = paste0(content(setlist_list)$setlist[[i]]$venue$city$name, ""),
                      'VenueName' = paste0(content(setlist_list)$setlist[[i]]$venue$name, ""),
                      'Latitude' = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$lat, ""),
                      'Longitude' = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$long, "")
    )
    venue_info <- rbind(venue_info, new_data)
  }
  # Convert the date column to date format
  venue_info$EventDate <- strptime(venue_info$EventDate, format = "%d-%m-%Y")
  # Subset for sets before today and then add an ID number.
  venue_info <- subset(venue_info, as.Date(EventDate) < as.Date(Sys.Date())) %>%
    mutate(EventID = row_number()) %>%
    relocate(EventID)
  
  print(kable(venue_info[,-2], row.names = F))
  
  setlist.choice <- readline(prompt = "Select the Number of Sets to Include: ")
  
  sets <- venue_info[1:setlist.choice,2]
  
  info_needed <- list(content(setlist_list), sets)
  
  return(info_needed)
  
}

#----RETURN THE SONGS PLAYED---------# ####
GetSongInfo <- function(artist_name){
  #' This function takes the artist name as an argument and applies it to the previous function,
  #' Which in turn applies it to the one before that.
  #' Once it has the artist name and the number of sets to choose from it then gathers all of
  #' the songs for each set and turns them into a dataframe ordered by the songs average location in
  #' the set, also including the probability that the song will be played.
  #' 
  #' @param artist_name - The name of the chosen artist to be passed through functions.
  #' 
  #' @return - a dataframe of song info.
  #' 
  
  # First use previous function and save the information returned
  info_needed <- getSetlistInfo(artist_name)
  
  # Create a blank dataframe which can be used to store the results
  dataset <- data.frame()

  # Now, info_needed[[2]] is the SetID of the sets selected, we want to first create a loop across
  # all selected sets.
  for(set in as.numeric(info_needed[[2]])){
    # We need to get the number of sets within each set e.g. main stage, side stage, encore count as 3.
    b <- sum(lengths(info_needed[[1]]$setlist[[set]]$sets))
    # We only want to continue the process if each setlist has more than one set.
    if(b > 0){
      
      # The next loop initiated loops across all of the mini sets within the big sets and returns the number
      # of songs in each of these
      for(i in 1:b){
        # The number of songs in each mini-set
        c <- length(lengths(info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song))
        
        # The final nested loop looks at all the songs within each min-set and larger-set and returns their name
        # i have also included code for if there is a cover song. So we return the name of the cover artist also.
        for(j in 1:c){
          # Begin to create the new dataframe by combining the set number and song title.
          newdata <- cbind('set' = set,
                           'SongName' = info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song[[j]]$name)
          
          # Next i want to see if the current son gi sa cover song or not. First try and allocate to 't'
          t <- try(info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song[[j]]$cover$name)
          
          # If class of 't' is NULL then it is not a cover song, else it is a cover song and grab the artist name
          if("NULL" %in% class(t)){
            newdata <- cbind(newdata,
                             'Cover' = FALSE,
                             'OrigArtist' = 'N/A')
          } else {
            newdata <- cbind(newdata,
                             'Cover' = TRUE,
                             'OrigArtist' = info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song[[j]]$cover$name)
          }
          
          # Bind the new data and the old data together in a dataset.
          dataset <- rbind(dataset, newdata)
        }
      }
    }
  }

  # The next part of the function is to convert the dataset into a better format.
  dataset <- dataset %>%
    # First we remove any whitespace
    mutate(across(.cols = (2:4), ~trimws(.x))) %>%
    # Group by the set number
    group_by(set) %>%
    # Add a row number value for the song position
    mutate(song = row_number()) %>%
    ungroup() %>%
    # Then we want to group by the following to summarise
    group_by(SongName, Cover, OrigArtist) %>%
    # Here we want the songs average position, the number of times played and then the probability
    # that the song will be played.
    summarise(AvgPosition = round(mean(song), 2),
              TimesPlayed = n(),
              ProbPlay = length(unique(set)),
              .groups = 'drop') %>%
    # Then overwrite the ProbPlay column with actual probability
    mutate(ProbPlay = percent(TimesPlayed / max(ProbPlay), accuracy = 0.01)) %>%
    arrange(AvgPosition) %>%
    # Finally filter out blanks
    filter(nchar(trimws(SongName)) > 0)
  
  # The final thing is to print out the dataset, removing the cover columns if no covers have been
  # performed.
  if(sum(as.logical(dataset$Cover)) >= 1){
    print(kable(dataset, row.names = F))
  } else {
    print(kable(dataset %>% select(-Cover, -OrigArtist), row.names = F))
  }
  return(dataset)
}

#----ADD A PLAYLIST TO SPOTIFY-------# ####
CreateSpotifyPlaylist <- function(artist_name, playlist_name = artist_name, public = FALSE){
  #' This function takes the output from the GetSongInfo function and uses it to create
  #' a playlist on the user's Spotify account so long as they have developer status and
  #' a client id and secret.
  #' 
  #' @param artist_name - The name of the artist you wish you search for. This is passed through
  #'                      the previous functions in a waterfall manner.
  #' @param playlist_name - The name of the new playlist you wish to create. It takes the default
  #'                        value the same as the artist name if nothing is entered.
  #' @param public - Whether you would like the playlist to be public or not, the default is FALSE
  #'

  # Firstly, run the code from the previous functions and save the result.
  SongInfo <- GetSongInfo(artist_name)
  
  # Create a playlist wrapped in the invisible function to prevent output
  invisible(create_playlist(spotify_username,
                            playlist_name,
                            public = public))
  
  # Find the playlist id of the first playlist with the name of the playlist. This part could use
  # some work to identify the proper playlist. Maybe saving output of the create playlist function...
  playlist_id <- get_my_playlists() %>%
    filter(name == playlist_name)
  
  playlist_id <- playlist_id$id[1]
  
  # Now we want to loop through all songs in the dataframe returned by the GetSongInfo function and
  # firstly alter the artist based on if the song is a cover, then find the id number of the song
  # followed by adding the song to the playlist if it is found. Again, this bit needs some work.
  # the search_spotify function doesn't search well for the song name so you get the top 50 (max)
  # songs by popularity by the artist in the search, if the song isn't in this top 50 then it
  # wont find a track id so i need to fix to cycle through all lists of 50 to find the song if needed.
  
  for(i in 1:nrow(SongInfo)){
    # Get the original artist
    OrigArtist <- SongInfo$OrigArtist[i]
    # If it is N/A then use the original artist name otherwise it is the cover artist
    if(OrigArtist == "N/A"){
      Artist <- artist_name
    } else {
      Artist <- OrigArtist
    }
    
    # Return the song name of current row
    SongName <- SongInfo$SongName[i]
    
    # Combine artist and song title for searching
    song <- paste(Artist, SongName)
    
    # Search Spotify and keep the rows with the selected song name
    Newdata <- search_spotify(song, type = 'track', limit = 50, market = "US")$tracks$items %>%
      filter(name == SongName)
    
    # Keep only the selected track_id
    track_id <- Newdata[1, 'uri']
    
    # If no track id then continue to next song, else add the song to the playlist
    if(is.na(track_id)){
      next
    } else {
      add_items_to_playlist(playlist_id, track_id)
    }
  }
}

#----TEST THE CODE-------------------# ####
CreateSpotifyPlaylist('Little Simz', 'Little Simz Gig')
