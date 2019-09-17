setwd("F:\\DATA science\\youtube-new")
##Step 1- Cleaning data and preparing it for analysis 
###Lets first load all the packages that we will be using here.
#knitr::opts_chunk$set(echo = TRUE)
#install.packages("rmarkdown")
i#nstall.packages("rjson")
#install.packages("jsonlite")
#install.packages("ggplot2")
#install.packages("pander")
#install.packages("stringr")
#install.packages("cowplot")
library(rmarkdown)
library(rjson)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(pander)
library(stringr)
library(cowplot)
# reading the data 
us_data <- read.csv("USvideos.csv")
gb_data <- read.csv("GBvideos.csv")
ca_data <- read.csv("CAvideos.csv")
de_data <- read.csv("DEvideos.csv")
fr_data <- read.csv("FRvideos.csv")
summary(us_data)
summary(gb_data)
##We see that the category id is character type. Lets convert it to numeric 
##type because it will be used a lot for further analysis. We will do it for all
#5 datasets.
us_data <- us_data[!(us_data$video_error_or_removed == "True" || us_data$comments_disabled == TRUE),]
gb_data <- gb_data[!(gb_data$video_error_or_removed == "True" || gb_data$comments_disabled == TRUE),]
ca_data <- ca_data[!(ca_data$video_error_or_removed == "True" || ca_data$comments_disabled == TRUE),]
de_data <- de_data[!(de_data$video_error_or_removed == "True" || de_data$comments_disabled == TRUE),]
fr_data <- fr_data[!(fr_data$video_error_or_removed == "True" || fr_data$comments_disabled == TRUE),]
#removing the columns which are not useful for the analysises
us_data <- us_data[, !(colnames(us_data) %in% c("thumbnail_link","comments_disabled","ratings_disabled","video_error_or_removed"))]
gb_data <- gb_data[, !(colnames(gb_data) %in% c("thumbnail_link","comments_disabled","ratings_disabled","video_error_or_removed"))]
ca_data <- ca_data[, !(colnames(ca_data) %in% c("thumbnail_link","comments_disabled","ratings_disabled","video_error_or_removed"))]
de_data <- de_data[, !(colnames(de_data) %in% c("thumbnail_link","comments_disabled","ratings_disabled","video_error_or_removed"))]
fr_data <- fr_data[, !(colnames(fr_data) %in% c("thumbnail_link","comments_disabled","ratings_disabled","video_error_or_removed"))]
##Exploratory data analysis.
knitr::opts_chunk$set(echo = TRUE, fig.align = "center")
pairs(us_data[7:10], method = "kendall",panel = panel.smooth, cex = 1.5, pch = 21, bg = "green", main = "Pairs plot for
      trending videos in US")
#britain graph
pairs(gb_data[7:10], method = "kendall",panel = panel.smooth, cex = 1.5, pch = 21, bg = "blue", main = "Pairs plot for
trending videos in Britain")
##canada graph
pairs(ca_data[7:10], method = "kendall",panel = panel.smooth, cex = 1.5, pch = 21, bg = "red", main = "Pairs plot for
trending videos in Canada")
##Denmark graph
pairs(de_data[7:10], method = "kendall",panel = panel.smooth, cex = 1.5, pch = 21, bg = "yellow", main = "Pairs plot for
trending videos in Denmark")
# france
pairs(fr_data[7:10], method = "kendall",panel = panel.smooth, cex = 1.5, pch = 21, bg = "pink", main = "Pairs plot for
trending videos in France")
# now import the json dataset to import and combind it
us_cat_json <- fromJSON("US_category_id.json")
gb_cat_json <- fromJSON("GB_category_id.json")
ca_cat_json <- fromJSON("CA_category_id.json")
de_cat_json <- fromJSON("DE_category_id.json")
fr_cat_json <- fromJSON("FR_category_id.json")
summary(us_cat_json)
##From this JSON data, we need to fetch the name of the category corresponding 
##to the category id. Since the json data is an object of object array.
##We can directly access data by calling out by index.
#Then we will bind the category id and titles into a data frame.
US_category <-  as.data.frame(cbind(us_cat_json[["items"]][["id"]], us_cat_json[["items"]][["snippet"]][["title"]]))
GB_category <-  as.data.frame(cbind(gb_cat_json[["items"]][["id"]], gb_cat_json[["items"]][["snippet"]][["title"]]))
CA_category <-  as.data.frame(cbind(ca_cat_json[["items"]][["id"]], ca_cat_json[["items"]][["snippet"]][["title"]]))
DE_category <-  as.data.frame(cbind(de_cat_json[["items"]][["id"]], de_cat_json[["items"]][["snippet"]][["title"]]))
FR_category <-  as.data.frame(cbind(fr_cat_json[["items"]][["id"]], fr_cat_json[["items"]][["snippet"]][["title"]]))
#renameing the coloumn names
names(US_category) <- c("category_id","category_title")
names(GB_category) <- c("category_id","category_title")
names(CA_category) <- c("category_id","category_title")
names(DE_category) <- c("category_id","category_title")
names(FR_category) <- c("category_id","category_title")
View(FR_category)
#Now lets merge this category data frame with our original data frame by 
#category id for respective country's dataset.
us_data <- merge(x = us_data, y = US_category, by = "category_id", all = "TRUE")
gb_data <- merge(x = gb_data, y = GB_category, by = "category_id", all = "TRUE")
ca_data <- merge(x = ca_data, y = CA_category, by = "category_id", all = "TRUE")
de_data <- merge(x = de_data, y = DE_category, by = "category_id", all = "TRUE")
fr_data <- merge(x = fr_data, y = FR_category, by = "category_id", all = "TRUE")
us_df <- as.data.frame(sort(table(us_data$category_title), decreasing = TRUE))

names(us_df) <- c("category_title","count")

ggplot(us_df, aes(x = category_title, y = count, fill = factor(category_title))) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 70,hjust = 1), legend.position = "none") 
+ scale_x_discrete(name = "Video  category ") + 
  scale_y_continuous(name = "Number of videos") + 
  labs(title = "Plot of trending video categories in US")


gb_df <- as.data.frame(sort(table(gb_data$category_title), decreasing = TRUE))

names(gb_df) <- c("category_title","count")

ggplot(gb_df, aes(x = category_title, y = count, fill = factor(category_title))) + 
  
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 70,hjust = 1), legend.position = "none") 
+ scale_x_discrete(name = "Video
                                                                                                       category ") + scale_y_continuous(name = "Number of videos") +
  labs(title = "Plot of trending video categories in GB")

ca_df <- as.data.frame(sort(table(ca_data$category_title), decreasing = TRUE))

names(ca_df) <- c("category_title","count")
ggplot(ca_df, aes(x = category_title, y = count, fill = factor(category_title))) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 70,hjust = 1), legend.position = "none") + scale_x_discrete(name = "Videocategory ") +
  scale_y_continuous(name = "Number of videos") + 
labs(title = "Plot of trending video categories in    Canada") 


de_df <- as.data.frame(sort(table(de_data$category_title), decreasing = TRUE))
names(de_df) <- c("category_title","count")

ggplot(de_df, aes(x = category_title, y = count, fill = factor(category_title))) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1), legend.position = "none") + scale_x_discrete(name = "Video
                                                                                                       category ") + 
scale_y_continuous(name = "Number of videos") + labs(title = "Plot of trending video categories in
                                                                                                                                                                          Denmark")


fr_df <- as.data.frame(sort(table(fr_data$category_title), decreasing = TRUE))
names(fr_df) <- c("category_title","count")

ggplot(fr_df, aes(x = category_title, y = count, fill = factor(category_title))) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 70,hjust = 1), legend.position = "none") + scale_x_discrete(name = "Video 
                                                                                                       category ") + scale_y_continuous(name = "Number of videos") + 
labs(title = "Plot of trending video categories in 
                                                                                                                                                                          France")
## most top 10 vedios in us

us_ch_df <- as.data.frame(us_data$channel_title[!duplicated(us_data$title)])
names(us_ch_df) <- c("channel")
us_ch_df <- as.data.frame(sort(table(us_ch_df$channel), decreasing = TRUE))
names(us_ch_df) <- c("channel", "count")

ggplot(us_ch_df[1:10,], aes(x = channel, y = count, fill = factor(channel))) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1), legend.position = "none") + scale_x_discrete(name =
                                                                                                         "Channel ",label = function(x) str_wrap(x, width = 15)) + scale_y_continuous(name = "Number of videos") + 
  labs(title = "Plot of top 10 trending channels in US")



gb_ch_df <- as.data.frame(gb_data$channel_title[!duplicated(gb_data$title)])
names(gb_ch_df) <- c("channel")
gb_ch_df <- as.data.frame(sort(table(gb_ch_df$channel), decreasing = TRUE))
names(gb_ch_df) <- c("channel", "count")

ggplot(gb_ch_df[1:10,], aes(x = channel, y = count, fill = factor(channel))) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45,hjust
                                                                                                                                              = 1), legend.position = "none") + scale_x_discrete(name = "Channel ",label = 
                                                                                                                                                                                                   function(x) str_wrap(x, width = 15)) + scale_y_continuous(name = "Number of 
                                                                                                                                                                                                                                                             videos") + 
  labs(title = "Plot of top 10 trending channels in Britain")

# most top 10 vedios in canada
ca_ch_df <- as.data.frame(ca_data$channel_title[!duplicated(ca_data$title)])
names(ca_ch_df) <- c("channel")
ca_ch_df <- as.data.frame(sort(table(ca_ch_df$channel), decreasing = TRUE))
names(ca_ch_df) <- c("channel", "count")

ggplot(ca_ch_df[1:10,], aes(x = channel, y = count, fill = factor(channel))) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45,hjust
                                                                                                                                              = 1), legend.position = "none") + scale_x_discrete(name = "Channel ",label = 
                                                                                                                                                                                                  function(x) str_wrap(x, width = 15)) + 
scale_y_continuous(name = "Number of 
                                                                                                                                                                                                                                                             videos") + labs(title = "Plot of top 10 trending channels in Canada")
de_ch_df <- as.data.frame(de_data$channel_title[!duplicated(de_data$title)])
names(de_ch_df) <- c("channel")
de_ch_df <- as.data.frame(sort(table(de_ch_df$channel), decreasing = TRUE))
names(de_ch_df) <- c("channel", "count")

ggplot(de_ch_df[1:10,], aes(x = channel, y = count, fill = factor(channel))) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45,hjust
                                                                                                                                              = 1), legend.position = "none") + scale_x_discrete(name = "Channel ",label = 
                                                                                                                                                                                                   function(x) str_wrap(x, width = 15)) + scale_y_continuous(name = "Number of 
                                                                                                                                                                                                                                                             videos") + labs(title = "Plot of top 10 trending channels in Denmark")
fr_ch_df <- as.data.frame(fr_data$channel_title[!duplicated(fr_data$title)])
names(fr_ch_df) <- c("channel")
fr_ch_df <- as.data.frame(sort(table(fr_ch_df$channel), decreasing = TRUE))
names(fr_ch_df) <- c("channel", "count")

ggplot(fr_ch_df[1:10,], aes(x = channel, y = count, fill = factor(channel))) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45,hjust 
                                                                                                                                              = 1), legend.position = "none") + scale_x_discrete(name = "Channel ",label = 
                                                                                                                                                                                                   function(x) str_wrap(x, width = 15)) + scale_y_continuous(name = "Number of 
                                                                                                                                                                                                                                                             videos") + labs(title = "Plot of top 10 trending channels in France")
