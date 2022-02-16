library(shiny)
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)

###### used code from the following: https://github.com/pspachtholz/BookRecommender/blob/master/server.R#L29

myurl = "https://liangfgithub.github.io/MovieData/"

# load functions
source('cf_algorithm.R') # collaborative filtering
source('similarity_measures.R') # similarity measures

####### read in data #############################################

# use colClasses = 'NULL' to skip columns
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

#################### create movies #############################################

movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

########## create ratings per movie ##############################

ratings_per = ratings %>% 
  group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID')
summary(ratings_per$ratings_per_movie)

########################## create genres ##########################

genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                              type.convert=TRUE),
                    stringsAsFactors=FALSE)
genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")
m = length(genre_list)
genre_matrix = matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp)){
  genre_matrix[i,genre_list %in% tmp[i,]]=1
}
colnames(genre_matrix) = genre_list
remove("tmp", "genres")

########################### combine ratings movies with genre ################################

tmp = ratings_per %>% 
  left_join(data.frame(MovieID = movies$MovieID, genre_matrix), 
            by = "MovieID")

######### workable code for "most popular" ##############################################

get_popular = function(genre) {
  tmp %>%
    filter(!!as.symbol(genre) == 1) %>%
    filter(ratings_per_movie >= 33) %>% # filter if more than 1st quartile of rated movies
    arrange(desc(ave_ratings)) %>% # filter by descending order
    select(c("Title", "MovieID", "ave_ratings")) %>%
    top_n(10, ave_ratings) # top 10
}

################# code for user ratings ##################################################

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$MovieID, 
                               j = rep(1,nrow(dat)), 
                               x = dat$Rating, 
                               dims = c(nrow(ratingmat), 1))
}

##### create rating matrix ####################################################################

ratingmat <- sparseMatrix(ratings$MovieID, ratings$UserID, x=ratings$Rating) # movie x user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
dimnames(ratingmat) <- list(MovieID = as.character(1:3952), UserID = as.character(sort(unique(ratings$UserID))))

############### shiny app ##################################################################

shinyServer(function(input, output, session) {
  
############################# genre ratings System 1 ################################################
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      res = get_popular(input$Genre)
      user_results = sort(res$ave_ratings, decreasing = TRUE)
      user_ids = as.numeric(res$MovieID)
      
      recom_results = as.numeric(rownames(movies[match(user_ids, movies$MovieID),])) # match movie ids with index
                                 
    }) # still busy
    
  }) # clicked on button
  
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
############################## user ratings System 2 #######################################
  
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  df2 <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      # add user's ratings as first column to rating matrix
      rmat <- cbind(user_ratings, ratingmat)
      
      # get the indices of which cells in the matrix should be predicted
      # predict all movies the current user has not yet rated
      items_to_predict <- which(rmat[, 1] == 0)
      prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
      
      res = predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
    
      user_results = sort(res[,1], decreasing = TRUE)[1:10]
      user_predicted_ids = as.numeric(names(user_results))
      recom_results2 <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  output$results2 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result2 <- df2()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result2$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result2$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
})    
