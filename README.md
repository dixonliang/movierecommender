# Movie Recommender

This repo contains the code for a movie recommendation application written in R, using Shiny. The project was the final project for the course, CS-542 (Practical Statistical Learning as part of the Masters in Computer Science program at the University of Illionis at Urbana Champaign. This was inspired and based on the following book recommendation application which can be found here: https://github.com/pspachtholz/BookRecommender. Much of the code was adapted from this repo. 

The link to the application can be found here: https://dixonliang.shinyapps.io/project_4/

The application provides recommendations through user input. 

1) Genre Recommendation: By using simple filtering in R, the application provides the top 10 movies for a genre selected by the user by using a popularity measure. The populairty measure is based on the highest average rating given a minimum number of ratings. 

2) User Recommendation: By asking the user to rate a subset of movies, the application uses UBCF (User-Based Collaborative Filtering) to recommend up to 10 movies that user might like. 


