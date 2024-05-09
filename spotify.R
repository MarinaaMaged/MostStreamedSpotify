library("tidyverse")
library("dplyr")
library("shiny")
set.seed(123)
spotifySongs <- read.csv("C:/Users/pc/Downloads/spotify-2023.csv")
spotifySongs
summary(spotifySongs)
str(spotifySongs)
#as the data is so big it is better to use sum 
sum(duplicated(spotifySongs))
#double check
duplicated(spotifySongs)
mean(spotifySongs$artist_count)
#this code removes rows from the spotifySongs data frame that have any missing values. 
#The result is a new data frame (or the same one modified in place) with only complete rows.
spotifySongs <- spotifySongs[complete.cases(spotifySongs), ]
spotifySongs
#solving the data structure problem
is.numeric(spotifySongs$streams)
is.numeric(spotifySongs$in_deezer_playlists)
is.numeric(spotifySongs$in_shazam_charts)
#to solve it
spotifySongs$streams <- as.numeric(gsub('""', '', spotifySongs$streams), na.rm = TRUE)
is.numeric(spotifySongs$streams)

spotifySongs$in_deezer_playlists <- as.numeric(gsub('""', '', spotifySongs$in_deezer_playlists), na.rm = TRUE)
is.numeric(spotifySongs$in_deezer_playlists)

spotifySongs$in_shazam_charts <- as.numeric(gsub('""', '', spotifySongs$in_shazam_charts), na.rm = TRUE)
is.numeric(spotifySongs$in_shazam_charts)

str(spotifySongs)
# a problem I have faced that the missing values don't have n/a they are just missing 
spotifySongs <- read.csv("C:/Users/pc/Downloads/spotify-2023.csv", na.strings = "")
spotifySongs
# to check if there is n/a values , sum is better because of this data is so big 
sum(is.na(spotifySongs))
is.na(spotifySongs)

#to remove n/a 
spotifySongs <- na.omit(spotifySongs)
spotifySongs
#double check after adjustements
sum(is.na(spotifySongs))
distinct(spotifySongs)
sum(duplicated(spotifySongs))
summary(spotifySongs)

# Create a boxplot to identify outliers
boxplot(spotifySongs$released_year, main="Boxplot of released year of song", xlab="released year",ylim=c(1940,2024))

# Identify outliers
outliers <- boxplot.stats(spotifySongs$released_year)$out

# Print or inspect the outliers
print(outliers)

# Create a cleaned dataframe without outliers
spotifySongs <- spotifySongs[!spotifySongs$released_year %in% outliers, ]
boxplot(spotifySongs$released_year, main="Boxplot of released year of song", xlab="released year",ylim=c(1940,2024))



# Create a boxplot to identify outliers
boxplot(spotifySongs$bpm, main="Boxplot of bpm", xlab="pbm")

# Identify outliers
outliers <- boxplot.stats(spotifySongs$bpm)$out

# Print or inspect the outliers
print(outliers)

# Create a cleaned dataframe without outliers
spotifySongs <- spotifySongs[!spotifySongs$bpm %in% outliers, ]
boxplot(spotifySongs$bpm, main="Boxplot of bpm", xlab="bpm")



# Create a boxplot to identify outliers
boxplot(spotifySongs$artist_count, main="Boxplot of Artist Count", xlab="Artist count")

# Identify outliers
outliers <- boxplot.stats(spotifySongs$artist_count)$out

# Print or inspect the outliers
print(outliers)

# Create a cleaned dataframe without outliers
spotifySongs <- spotifySongs[!spotifySongs$artist_count %in% outliers, ]
boxplot(spotifySongs$artist_count, main="Boxplot of artist count", xlab="Artist count")

spotifySongs$streams <- as.numeric(gsub('""', '', spotifySongs$streams), na.rm = TRUE)
is.numeric(spotifySongs$streams)

# Create a boxplot to identify outliers 
boxplot(spotifySongs$streams, main="Boxplot of streams", xlab="streams")

# Identify outliers
outliers <- boxplot.stats(spotifySongs$streams)$out

# Print or inspect the outliers
print(outliers)

# Create a cleaned dataframe without outliers
spotifySongs <- spotifySongs[!spotifySongs$streams %in% outliers, ]
boxplot(spotifySongs$streams, main="Boxplot of streams", xlab="streams",ylim=c(58149378 ,501381703))

#sequence for rows after removing outliers, cleaning 
rownames(spotifySongs) <- NULL
spotifySongs

dim(spotifySongs)


top10_data <- head(spotifySongs[order(spotifySongs$streams, decreasing = TRUE), ], 10)
top50_data <- head(spotifySongs[order(spotifySongs$streams, decreasing = TRUE), ], 50)
top100_data <- head(spotifySongs[order(spotifySongs$streams, decreasing = TRUE), ], 100)
variables <- c("streams", "bpm", "danceability_.", "valence_.", "energy_.", "acousticness_.", "instrumentalness_.", "liveness_.", "speechiness_.")

#Distribution
dist_ui <- fluidPage(
  titlePanel("Distribution Plotter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("column", "Choose a column:", choices = setdiff(variables, "streams")),
      actionButton("plotButtonDist", "Plot")
    ),
    mainPanel(
      plotOutput("distributionPlot")
    )
  )
)

# Streams vs
streams_vs_ui <- fluidPage(
  titlePanel("Streams Bar Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a variable:", choices = setdiff(variables, "streams")),
      actionButton("plotButtonStreamsVs", "Plot")
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# BPM Scatter Plots
bpm_scatter_ui <- fluidPage(
  titlePanel("BPM Scatter Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("VariableBPM", "Choose a Variable:", choices = setdiff(variables, "bpm")),
      actionButton("plotButtonBPM", "Plot")
    ),
    mainPanel(
      plotOutput("scatterPlotBPM")
    )
  )
)

# Key vs
key_vs_ui <- fluidPage(
  titlePanel("Key Scatter Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("VariableKeyvs", "Choose a Variable:", choices = variables),
      actionButton("plotButtonKeyvs", "Plot")
    ),
    mainPanel(
      plotOutput("scatterPlotKeyvs")
    )
  )
)

# Key Distribution
key_dist_ui <- fluidPage(
  titlePanel("Key Distribution"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose Dataset:",
                  choices = c("Overall", "Top 100", "Top 50", "Top 10")),
      actionButton("plotButtonKeyDist", "Plot")
    ),
    mainPanel(
      plotOutput("keyPlot")
    )
  )
)

# Mode Distribution
mode_dist_ui <- fluidPage(
  titlePanel("Mode Distribution"),
  sidebarLayout(
    sidebarPanel(
      selectInput("datasetModeDist", "Choose Dataset:",
                  choices = c("Overall", "Top 100", "Top 50", "Top 10")),
      actionButton("plotButtonModeDist", "Plot")
    ),
    mainPanel(
      plotOutput("modePlot")
    )
  )
)

# Playlists vs
playlists_vs_ui <- fluidPage(
  titlePanel("Playlists Scatter Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("VariablePlaylistsVs", "Choose a Variable:", choices = setdiff(variables, "streams")),
      actionButton("plotButtonPlaylistsVs", "Plot")
    ),
    mainPanel(
      plotOutput("scatterPlotPlaylistsVs")
    )
  )
)

ui <- navbarPage(
  "Interactive Visualizations",
  tabPanel("Distribution", dist_ui),
  tabPanel("Streams vs", streams_vs_ui),
  tabPanel("BPM Scatter Plots", bpm_scatter_ui),
  tabPanel("Key vs", key_vs_ui),
  tabPanel("Key Distribution", key_dist_ui),
  tabPanel("Mode Distribution", mode_dist_ui),
  tabPanel("Playlists vs", playlists_vs_ui)
)

server <- function(input, output, session) {
  observeEvent(input$plotButtonDist, {
    selected_plot <- ggplot(spotifySongs, aes_string(x = input$column)) +
      geom_histogram(binwidth = 10, fill = "skyblue", color = "lightblue") +
      labs(x = input$column, y = 'Count', title = paste(input$column, 'Distribution')) +
      theme_bw()
    
    output$distributionPlot <- renderPlot({
      print(selected_plot)
    })
  })
  
  observeEvent(input$plotButtonStreamsVs, {
    create_bar_plot <- function(data, x_variable) {
      ggplot(data = data, aes(x = .data[[x_variable]], y = streams)) +
        stat_summary(fun = "sum", geom = "bar", position = "dodge", fill = "skyblue", color = "lightblue") +
        labs(x = x_variable, y = "Total Streams", title = paste("Streams vs.", x_variable)) +
        theme_bw() +
        scale_y_continuous(labels = scales::comma_format(scale = 1e-9))
    }
    
    selected_plot <- create_bar_plot(spotifySongs, x_variable = input$variable)
    output$barPlot <- renderPlot({
      print(selected_plot)
    })
  })
  
  observeEvent(input$plotButtonBPM, {
    create_scatter_plot <- function(data, x_var, title) {
      ggplot(data = data, aes(x = !!sym(x_var), y = bpm, color = as.factor(key))) +
        geom_jitter(width = 0.2, size = 1) +
        labs(x = x_var, y = "BPM", title = title, color = "Key") +
        theme_bw()
    }
    
    selected_plot <- create_scatter_plot(spotifySongs, x_var = input$VariableBPM, title = paste("BPM vs.", input$VariableBPM))
    output$scatterPlotBPM <- renderPlot({
      print(selected_plot)
    })
  })
  
  observeEvent(input$plotButtonKeyvs, {
    create_scatter_plot <- function(data, y_var, title) {
      ggplot(data = data, aes(x = key, y = !!sym(y_var), color = as.factor(key))) +
        geom_jitter(width = 0.2, size = 1) +
        labs(x = "Key", y = title, title = paste(title, "vs. Key"), color = "Key") +
        theme_bw()
    }
    
    spotifySongs$streams <- spotifySongs$streams / max(spotifySongs$streams)
    selected_plot <- create_scatter_plot(spotifySongs,
                                         y_var = input$VariableKeyvs,
                                         title = input$VariableKeyvs)
    output$scatterPlotKeyvs <- renderPlot({
      print(selected_plot)
    })
  })
  
  
  observeEvent(input$plotButtonKeyDist, {
    create_pie_chart <- function(data, title) {
      data$key_percentage <- data$n / sum(data$n) * 100
      
      ggplot(data, aes(x = "", y = key_percentage, fill = as.factor(key))) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        geom_text(aes(label = paste(key, "\n", sprintf("%.1f%%", key_percentage))), position = position_stack(vjust = 0.5)) +
        coord_polar("y", start = 0) +
        labs(title = title, x = NULL, y = NULL) +
        theme_bw() +
        theme(axis.text = element_blank(), axis.title = element_blank(), legend.position = "none",  panel.grid = element_blank())
    }
    
    selected_data <- switch(input$dataset, "Overall" = spotifySongs, "Top 100" = top100_data, "Top 50" = top50_data, "Top 10" = top10_data)
    key_counts <- count(selected_data, key)
    pie_chart <- create_pie_chart(key_counts, title = input$dataset)
    output$keyPlot <- renderPlot({
      print(pie_chart)
    })
  })
  
  observeEvent(input$plotButtonModeDist, {
    create_pie_chart <- function(data, title) {
      data$mode_percentage <- data$n / sum(data$n) * 100
      ggplot(data, aes(x = "", y = mode_percentage, fill = as.factor(mode))) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        geom_text(aes(label = paste(mode, "\n", sprintf("%.1f%%", mode_percentage))),
                  position = position_stack(vjust = 0.5)) +
        coord_polar("y", start = 0) +
        labs(title = title, x = NULL, y = NULL) +
        theme_bw() +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = "none",
              panel.grid = element_blank())
    }
    
    selected_data <- switch(input$datasetModeDist,
                            "Overall" = spotifySongs,
                            "Top 100" = top100_data,
                            "Top 50" = top50_data,
                            "Top 10" = top10_data)
    mode_counts <- count(selected_data, mode)
    pie_chart <- create_pie_chart(mode_counts, title = input$datasetModeDist)
    output$modePlot <- renderPlot({
      print(pie_chart)
    })
  })
  
  
  observeEvent(input$plotButtonPlaylistsVs, {
    create_scatter_plot <- function(data, x_var, y_var, title) {
      ggplot(data = data, aes(x = !!sym(x_var), y = !!sym(y_var), color = as.factor(key))) +
        geom_jitter(width = 0.2, size = 1) +
        labs(x = x_var, y = "Normalized Average Playlists", title = title, color = "Key") +
        theme_bw()
    }
    spotifySongs$in_spotify_playlists <- spotifySongs$in_spotify_playlists / max(spotifySongs$in_spotify_playlists)
    
    selected_plot <- create_scatter_plot(
      spotifySongs,
      x_var = input$VariablePlaylistsVs,
      y_var = "in_spotify_playlists",
      title = paste("Playlists vs.", input$VariablePlaylistsVs)
    )
    output$scatterPlotPlaylistsVs <- renderPlot({
      print(selected_plot)
    })
  })
}
shinyApp(ui, server)
library(caret)
library(ClusterR) 
library(cluster) 
functionset <- spotifySongs
functionset <- spotifySongs %>%
  mutate(function_col=(danceability_.+valence_.+energy_.+acousticness_.+instrumentalness_.+liveness_.+speechiness_.))
selected_features <- functionset[, c("bpm","function_col")]



seperattedartists <- spotifySongs %>%
  separate_rows(artist.s._name, sep = ",")

unique_artist_count <- unique(seperattedartists$artist.s._name)
number_of_unique_values <- length(unique_artist_count)
number_of_unique_values
kmeans.re <- kmeans(selected_features, centers = 12, nstart = 150) 



plot(functionset[c( "function_col","bpm")],col=kmeans.re$cluster) 
kmeans.re$centers 
kmeans.re$centers[, c("bpm","function_col")] 

points(kmeans.re$centers[, c("function_col","bpm")],  
       col = 1:20, pch = 8, cex = 3)  
y_kmeans <- kmeans.re$cluster 
clusplot(selected_features[, c("function_col", "bpm")], 
         y_kmeans, 
         lines = 0, 
         shade = TRUE, 
         color = TRUE, 
         labels = 1, 
         plotchar = FALSE, 
         span = TRUE, 
         main = paste("Cluster genre"), 
         xlab = 'function_col', 
         ylab = 'bpm')

#classification:
library(rpart)
library(rpart.plot)
your_data <- data.frame(streams = spotify_2023$streams)
your_data<-your_data %>%
  mutate(streams=case_when(
    streams<mean(spotify_2023$streams )~ "low",
    streams >mean(spotify_2023$streams)~ "high"
  ))
print(your_data)
spotify_2023$streams<-your_data
your_data <- your_data %>%
  mutate(streams= your_data)
spotify_2023
spotify_2023$streams <-unlist(spotify_2023$streams) 
tree<-rpart(streams~ danceability_.+valence_.+energy_.+acousticness_.
            +instrumentalness_.+liveness_.+speechiness_.,data=spotify_2023 ,minsplit=2)
tree
rpart.plot(tree,fallen.leaves = F,cex = 0.6)
rpart.rules(tree)
data_to_predict<-data.frame(danceability_.=60,valence_.=30,energy_.=90,acousticness_.
                            =20,instrumentalness_.=10,liveness_.=70,speechiness_.=50)
predict(tree,newdata = data_to_predict)
data_to_predict<-data.frame(danceability_.=60,valence_.=30,energy_.=90,acousticness_.
                            =20,instrumentalness_.=10,liveness_.=70,speechiness_.=25)
predict(tree,newdata = data_to_predict)
#another example
tree<-rpart(mode~ danceability_.+valence_.+energy_.+acousticness_.
            +instrumentalness_.+liveness_.+speechiness_.,data=spotify_2023 ,minsplit=2)
tree
rpart.plot(tree,fallen.leaves = F,cex = 0.6)
rpart.rules(tree)
data_to_predict<-data.frame(danceability_.=60,valence_.=30,energy_.=90,acousticness_.
                            =20,instrumentalness_.=10,liveness_.=70,speechiness_.=50)
predict(tree,newdata = data_to_predict)
data_to_predict<-data.frame(danceability_.=60,valence_.=30,energy_.=90,acousticness_.
                            =20,instrumentalness_.=10,liveness_.=70,speechiness_.=25)
predict(tree,newdata = data_to_predict)

#data frames
str(spotifySongs)
length(spotifySongs)
head(spotifySongs)
tail(spotifySongs)
head(spotifySongs,n=3)
tail(spotifySongs,n=3)
summary(spotifySongs)
names(spotifySongs)
number_of_Major_songs <- sum(spotifySongs $mode == "Major")
 number_of_Major_songs


 number_of_Minor_songs<-sum(spotifySongs $mode == "Minor")
 number_of_Minor_songs



most_streamed_song<-spotifySongs[which(spotifySongs$streams==max(spotifySongs$stream)),"track_name"]
 most_streamed_song
 no_of_streams<-max(spotifySongs$streams)
 no_of_streams
 least_streamed_song<-spotifySongs[which(spotifySongs$streams==min(spotifySongs$stream)),"track_name"]
 least_streamed_song

 no_of_streams<-min(spotifySongs$streams)
 no_of_streams


fastest_song <- spotifySongs[which(spotifySongs$bpm == max(spotifySongs$bpm)),"track_name"]
 fastest_song

 max_bpm<-max(spotifySongs$bpm)
 max_bpm

 slowest_song <- spotifySongs[which(spotifySongs$bpm ==min(spotifySongs$bpm)),"track_name"]
 slowest_song

 min_bpm<-min(spotifySongs$bpm)
 min_bpm

songs_of_Taylor_Swift<- spotifySongs [spotifySongs $artist.s._name=="Taylor Swift","track_name"]
 songs_of_Taylor_Swift
songs_released_in_2023<- spotifySongs [spotifySongs $released_year==2023,"track_name"]
 songs_released_in_2023
 songs_released_in_2023
 table(spotifySongs$released_month)
 table(spotifySongs$released_year)
 table(spotifySongs$released_day)
most_common_key<-most_repeated_name <- names(sort(table(spotifySongs$key), decreasing = TRUE)[1])
 most_common_key
 least_common_key<-least_repeated_name <- names(sort(table(spotifySongs$key), decreasing = FALSE)[1])
 least_common_key 
 table(spotifySongs$key)
