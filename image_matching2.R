# ###############################################################################################
#
#   Image matching
#   Ver. R 3.4.2
#   2018/08/04
#   Kibaek Kim
#
# ###############################################################################################
# ###############################################################################################

### Must install package "jpeg"
### Must install package "EBImage"
# try http:// if https:// URLs are not supported
# source("https://bioconductor.org/biocLite.R")
# biocLite("EBImage")


### options
options(scipen=999)
Sys.setlocale(category = "LC_ALL", locale="Korean") 




# ###########################################################
#
#     GREY Scaling
#     color : Grey
#     Scale : 640*640
#
# ###########################################################

### folder sets
folder_main = "C:/Users/kibaek/Desktop/R_project"
folder_data = paste0(folder_main, "/data/cut_all/")
folder_result = paste0(folder_main, "/result/")
folder_prediction = paste0(folder_main, "/prediction/")
folder_model = paste0(folder_main, "/model/")
folder_plot = paste0(folder_main, "/plot/")
folder_grey = paste0(folder_main, "/data_grey/")

                     
setwd(folder_main)                     


### scaling : gray, pixel(640 x 640) #######################
library(jpeg)
library(EBImage)
images <- list.files(folder_data)
images



### setting width, height
w <- 640
h <- 640

setwd(folder_data)

for(i in 1:length(images))
  
{
  
  # Try-catch is necessary since some images
  
  # may not work.
  
  result <- tryCatch({
    
    # Image name
    print(images[i])
    imgname <- images[i]
    
    # Read image
    
    img <- readImage(imgname)
    
    # Resize image 28x28
    
    img_resized <- resize(img, w = w, h = h)
    
    # Set to grayscale
    
    grayimg <- channel(img_resized,"gray")
    
    # Path to file
    
    path <- paste(folder_grey, imgname, sep = "")
    
    # Save image
    
    writeImage(grayimg, path, quality = 70)
    
    # Print status
    
    print(paste("Done",i,sep = " "))},
    
    # Error function
    
    error = function(e){print(e)})
  
}

# #######################################################











# ##############################################################################################
#
#       Correlation Comparison
#
#
# 
# ##############################################################################################

### folder sets
folder_main = "C:/Users/kibaek/Desktop/R_project"
folder_data = paste0(folder_main, "/data/cut_all/")
folder_result = paste0(folder_main, "/result/")
folder_prediction = paste0(folder_main, "/prediction/")
folder_model = paste0(folder_main, "/model/")
folder_plot = paste0(folder_main, "/plot/")
folder_grey = paste0(folder_main, "/data_grey/")


setwd(folder_main) 
getwd()



library(jpeg)
library(EBImage)
images_list <- list.files(folder_grey)
images_list


### data table of images
images_df <- data.frame(numbers = 1:length(images_list), list = images_list)
images_df$list <- as.character(images_df$list)
str(images_df)
images_df[,2]






# #################################################################
#
#   image combination
#
# #################################################################

### Container
table_comb <- NULL

### a table for images by row
for(i in 1:nrow(images_df))
{
    
    ## loading image files
    file_name_each <- NULL
    
    file_name_each <- paste0(folder_grey, images_df[i,2])
    file_name_each
    
    image_read_each <- NULL
    image_read_each <- readJPEG(file_name_each)
    image_read_each
    
    
    ## vertor by row
    image_each_row <- NULL
    image_each_row <- as.numeric(t(image_read_each))
    # length(image_each_row)
    # -> 409600
    
    table_comb <<- rbind(table_comb, image_each_row)
}


dim(table_comb)
# apply(table_comb, 2, FUN = sum )



### save table_comb
file_name_table_comb_rds <- paste0(folder_data, "table_comb_640.rds")
saveRDS(table_comb, file_name_table_comb_rds)


file_name_table_comb_csv <- paste0(folder_data, "table_comb_640.csv")
write.csv(table.comb, file_name_table_comb_csv, row.names = FALSE)







##################################################################
#
#   Correlation
#
#  the cross-correlation or cross-covariance of two univariate series.
#
# ################################################################


# TEST ##################################
i=1
j=10
ccf(table_comb[i,], table_comb[j,],plot=FALSE)
ccf(table_comb[i,], table_comb[j,],plot=FALSE)[0]
ccf(table_comb[i,], table_comb[j,])
# #######################################



### correlation table
result_ccf_comb <- NULL

for(i in 1:nrow(table_comb))
{
  
    result_ccf_each_set <- NULL

    
      for(j in 1:nrow(table_comb))
      {
          ccf_value_origin <- NULL
          ccf_value_origin <- ccf(table_comb[i,], table_comb[j,],plot=FALSE)[0]
          
          ccf_value <- NULL
          ccf_value <- as.numeric(ccf_value_origin$acf)
          # str(ccf_value)
          
          
          result_ccf_each_set <<- append(result_ccf_each_set, ccf_value)
          
          # str(ccf_value)
      }
    
    
    result_ccf_comb <<- rbind(result_ccf_comb, result_ccf_each_set)
}
head(result_ccf_comb)
dim(result_ccf_comb)
str(result_ccf_comb)
result_ccf_comb <- as.data.frame(result_ccf_comb)
row.names(result_ccf_comb) <- 1:nrow(table_comb)
head(result_ccf_comb)
View(result_ccf_comb)



# RDS
file_name_ccf_table_rds <- paste0(folder_result, "ccf_table.rds")
saveRDS(result_ccf_comb, file_name_ccf_table_rds)
# CSV
file_name_ccf_table_csv <- paste0(folder_result, "ccf_table.csv")
write.csv(result_ccf_comb, file_name_ccf_table_csv, row.names=FALSE)


result_ccf_comb <- readRDS(file_name_ccf_table_rds)





# ###########################################################
#
#     Ranking table (look-alike)
#
# ###########################################################

rank_table_comb <- NULL

for( i in 1:nrow(table_comb))
{
    rank_each_row <- NULL
    rank_each_row <- append(i, which(rank(result_ccf_comb[i,]) == nrow(table_comb)-1))
    rank_each_row <- append(rank_each_row, result_ccf_comb[i, rank_each_row[2]])
    # str(rank_each_row)
    rank_each_row <- as.numeric(rank_each_row)
    rank_table_comb <<- rbind(rank_table_comb, rank_each_row)
}

rank_table_comb
rank_table_comb2 <- data.frame(numbers = rank_table_comb[,1],
                               alike = rank_table_comb[,2],
                               rate = rank_table_comb[,3])
rank_table_comb2$rate <- as.character(rank_table_comb2$rate*100)
rank_table_comb2$rate <- paste0(substr(rank_table_comb2$rate, 1, 5), " %")
rank_table_comb2


### name list table
split001 <- strsplit(images_df$list, split = "\\.")
split001 <- matrix(unlist(split001), ncol=2, byrow = TRUE)[,1]

split002 <- strsplit(split001, split = "\\_")
split002 <- matrix(unlist(split002), ncol=3, byrow = TRUE)[,3]

images_df_simple <- data.frame(name_num = 1:nrow(images_df), name_list = split002)
images_df_simple






### combination table : rank + name
rank_table_name <- merge(rank_table_comb2, images_df_simple, by.x = "numbers", by.y = "name_num")
rank_table_name2 <- merge(rank_table_name, images_df_simple, by.x = "alike", by.y = "name_num")
names(rank_table_name2) <- c("alike", "numbers", "rate", "numbers_name", "alike_name")
rank_table_name3 <- subset(rank_table_name2, select = c("numbers", "numbers_name", "alike", "alike_name", "rate"))


library(tidyverse)
rank_table_name4 <- rank_table_name3 %>%
                      arrange(numbers)
  
rank_table_name4
table(rank_table_comb2$alike)





# ###########################################################
#
#     Final result
#
# ###########################################################

file_name_final_result_rds <- paste0(folder_result, "final_result.rds")
saveRDS(rank_table_name4, file_name_final_result_rds)

file_name_final_result_csv <- paste0(folder_result, "final_result.csv")
write.csv(rank_table_name4, file_name_final_result_csv)



# ################################################################################################################
# ################################################################################################################













# ################################################################################################################
#
#     K-means (K-means classifier)
#     Unsupervised Learning
#     Grouping by 4 
#
# ################################################################################################################

# K-means concepts: http://rfriend.tistory.com/228


### folder sets
folder_main = "C:/Users/kibaek/Desktop/R_project"
folder_data = paste0(folder_main, "/data/cut_all/")
folder_result = paste0(folder_main, "/result/")
folder_prediction = paste0(folder_main, "/prediction/")
folder_model = paste0(folder_main, "/model/")
folder_plot = paste0(folder_main, "/plot/")
folder_grey = paste0(folder_main, "/data_grey/")




### loading data
file_name_table_comb_rds <- paste0(folder_data, "table_comb_640.rds")
table_comb <- readRDS(file_name_table_comb_rds)
dim(table_comb)

### KNN
library(class)
kmeans_4 <- kmeans(table_comb, 4)
names(kmeans_4)
kmeans_4$cluster
table(kmeans_4$cluster)
kmeans_4_result <- as.numeric(kmeans_4$cluster)


### adding base info
file_name_final_result_rds <- paste0(folder_result, "final_result.rds")
rank_table_name4 <- readRDS(file_name_final_result_rds)

rank_table_name4[,1:2]

result_kmeans <- rank_table_name4[, 1:2]

result_kmeans$cluster <- kmeans_4_result

result_kmeans


### save
file_name_cluster_rds <- paste0(folder_result, "result_clustering.rds")
saveRDS(result_kmeans, file_name_cluster_rds)

file_name_cluster_csv <- paste0(folder_result, "result_clustering.csv")
write.csv(result_kmeans, file_name_cluster_csv)















# ###########################################################################################################
# 
#     WITH CELEBRITIES
#
#
#
#
#
# 
# ###########################################################################################################


# ###########################################################
#
#       GREY Scaling
#     color : Grey
#     Scale : 640*640
#
# ###########################################################

### folder sets
folder_main = "C:/Users/kibaek/Desktop/R_project"
folder_data = paste0(folder_main, "/data_n_celeb/")
folder_result = paste0(folder_main, "/result_celeb/")
folder_grey = paste0(folder_main, "/data_n_celeb_grey/")


setwd(folder_main)                     


### scaling : gray, pixel(640 x 640) #######################
library(jpeg)
library(EBImage)
images <- list.files(folder_data)
images



### setting width, height
w <- 640
h <- 640

setwd(folder_data)

for(i in 1:length(images))
  
{
  
  # Try-catch is necessary since some images
  
  # may not work.
  
  result <- tryCatch({
    
    # Image name
    
    imgname <- images[i]
    
    # Read image
    
    img <- readImage(imgname)
    
    # Resize image 28x28
    
    img_resized <- resize(img, w = w, h = h)
    
    # Set to grayscale
    
    grayimg <- channel(img_resized,"gray")
    
    # Path to file
    
    path <- paste(folder_grey, imgname, sep = "")
    
    # Save image
    
    writeImage(grayimg, path, quality = 70)
    
    # Print status
    
    print(paste("Done",i,sep = " "))},
    
    # Error function
    
    error = function(e){print(e)})
  
}

# #######################################################













# ##############################################################################################
#
#       Correlation Comparison
#
#
# 
# ##############################################################################################

folder_main = "C:/Users/kibaek/Desktop/R_project"
folder_data = paste0(folder_main, "/data_n_celeb/")
folder_result = paste0(folder_main, "/result_celeb/")
folder_grey = paste0(folder_main, "/data_n_celeb_grey/")



setwd(folder_main) 
getwd()



library(jpeg)
library(EBImage)
images_list <- list.files(folder_grey)
images_list


### data table of images
images_df <- data.frame(numbers = 1:length(images_list), list = images_list)
images_df$list <- as.character(images_df$list)
str(images_df)
images_df[,2]






# #################################################################
#
#   image combination
#
# #################################################################

### Container
table_comb <- NULL

### a table for images by row
for(i in 1:nrow(images_df))
{
  
  ## loading image files
  file_name_each <- NULL
  
  file_name_each <- paste0(folder_grey, images_df[i,2])
  file_name_each
  
  image_read_each <- NULL
  image_read_each <- readJPEG(file_name_each)
  image_read_each
  
  
  ## vertor by row
  image_each_row <- NULL
  image_each_row <- as.numeric(t(image_read_each))
  # length(image_each_row)
  # -> 409600
  
  table_comb <<- rbind(table_comb, image_each_row)
}


dim(table_comb)
# apply(table_comb, 2, FUN = sum )



### save table_comb
file_name_table_comb_rds <- paste0(folder_data, "table_comb_640.rds")
saveRDS(table_comb, file_name_table_comb_rds)


file_name_table_comb_csv <- paste0(folder_data, "table_comb_640.csv")
write.csv(table_comb, file_name_table_comb_csv, row.names = FALSE)







##################################################################
#
#   Correlation
#
#  the cross-correlation or cross-covariance of two univariate series.
#
# ################################################################


# TEST ##################################
i=1
j=10
ccf(table_comb[i,], table_comb[j,],plot=FALSE)
ccf(table_comb[i,], table_comb[j,],plot=FALSE)[0]
# ccf(table_comb[i,], table_comb[j,])
# #######################################



### correlation table
result_ccf_comb <- NULL

for(i in 1:nrow(table_comb))
{
  
  result_ccf_each_set <- NULL
  
  
  for(j in 1:nrow(table_comb))
  {
    ccf_value_origin <- NULL
    ccf_value_origin <- ccf(table_comb[i,], table_comb[j,],plot=FALSE)[0]
    
    ccf_value <- NULL
    ccf_value <- as.numeric(ccf_value_origin$acf)
    # str(ccf_value)
    
    
    result_ccf_each_set <<- append(result_ccf_each_set, ccf_value)
    
    # str(ccf_value)
  }
  
  
  result_ccf_comb <<- rbind(result_ccf_comb, result_ccf_each_set)
}
head(result_ccf_comb)
dim(result_ccf_comb)
str(result_ccf_comb)
result_ccf_comb <- as.data.frame(result_ccf_comb)
row.names(result_ccf_comb) <- 1:nrow(table_comb)
head(result_ccf_comb)
# View(result_ccf_comb)



# RDS
file_name_ccf_table_rds <- paste0(folder_result, "ccf_table.rds")
saveRDS(result_ccf_comb, file_name_ccf_table_rds)
# CSV
file_name_ccf_table_csv <- paste0(folder_result, "ccf_table.csv")
write.csv(result_ccf_comb, file_name_ccf_table_csv, row.names=FALSE)


result_ccf_comb <- readRDS(file_name_ccf_table_rds)





# ###########################################################
#
#     Ranking table (look-alike)
#
# ###########################################################

rank_table_comb <- NULL

for( i in 1:nrow(table_comb))
{
  rank_each_row <- NULL
  rank_each_row <- append(i, which(rank(result_ccf_comb[i,]) == nrow(table_comb)-1))
  rank_each_row <- append(rank_each_row, result_ccf_comb[i, rank_each_row[2]])
  # str(rank_each_row)
  rank_each_row <- as.numeric(rank_each_row)
  rank_table_comb <<- rbind(rank_table_comb, rank_each_row)
}

rank_table_comb
rank_table_comb2 <- data.frame(numbers = rank_table_comb[,1],
                               alike = rank_table_comb[,2],
                               rate = rank_table_comb[,3])
rank_table_comb2$rate <- as.character(rank_table_comb2$rate*100)
rank_table_comb2$rate <- paste0(substr(rank_table_comb2$rate, 1, 5), " %")
rank_table_comb2


### name list table
split001 <- strsplit(images_df$list, split = "\\.")
split001 <- matrix(unlist(split001), ncol=2, byrow = TRUE)[,1]

split002 <- strsplit(split001, split = "\\_")
split002 <- matrix(unlist(split002), ncol=3, byrow = TRUE)[,3]

images_df_simple <- data.frame(name_num = 1:nrow(images_df), name_list = split002)
images_df_simple






### combination table : rank + name
rank_table_name <- merge(rank_table_comb2, images_df_simple, by.x = "numbers", by.y = "name_num")
rank_table_name2 <- merge(rank_table_name, images_df_simple, by.x = "alike", by.y = "name_num")
names(rank_table_name2) <- c("alike", "numbers", "rate", "numbers_name", "alike_name")
rank_table_name3 <- subset(rank_table_name2, select = c("numbers", "numbers_name", "alike", "alike_name", "rate"))


library(tidyverse)
rank_table_name4 <- rank_table_name3 %>%
  arrange(numbers)

rank_table_name4
table(rank_table_comb2$alike)





# ###########################################################
#
#     Final result
#
# ###########################################################

file_name_final_result_rds <- paste0(folder_result, "final_result.rds")
saveRDS(rank_table_name4, file_name_final_result_rds)

file_name_final_result_csv <- paste0(folder_result, "final_result.csv")
write.csv(rank_table_name4, file_name_final_result_csv)

