setwd("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/extract_embeddings/OutputAlphaEarthEmbeddings")
library(stringr)

# 2019 # 

d1 <- read.csv("AlphaEarth_inventory_2019_3yr.csv", header=T, sep=",")
names(d1)


replacements <- c("_2017$" = "_2", 
                  "_2018$" = "_1", 
                  "_2019$" = "_0")

names(d1) <- str_replace_all(names(d1), replacements)

# 2020 # 

d2 <- read.csv("AlphaEarth_inventory_2020_3yr.csv", header=T, sep=",")
names(d2)


replacements <- c("_2018$" = "_2", 
                  "_2019$" = "_1", 
                  "_2020$" = "_0")

names(d2) <- str_replace_all(names(d2), replacements)

# 2021 #
d3 <- read.csv("AlphaEarth_inventory_2021_3yr.csv", header=T, sep=",")
names(d3)


replacements <- c("_2019$" = "_2", 
                  "_2020$" = "_1", 
                  "_2021$" = "_0")

names(d3) <- str_replace_all(names(d3), replacements)

# 2022 #
d4 <- read.csv("AlphaEarth_inventory_2022_3yr.csv", header=T, sep=",")
names(d4)


replacements <- c("_2020$" = "_2", 
                  "_2021$" = "_1", 
                  "_2022$" = "_0")

names(d4) <- str_replace_all(names(d4), replacements)

# 2023 #
d5 <- read.csv("AlphaEarth_inventory_2023_3yr.csv", header=T, sep=",")
names(d5)


replacements <- c("_2021$" = "_2", 
                  "_2022$" = "_1", 
                  "_2023$" = "_0")

names(d5) <- str_replace_all(names(d5), replacements)


d <- rbind(d1,d2,d3,d4,d5)

write.csv(d,"AE_3y_embeddings.csv")

test <- read.csv("AE_3y_embeddings.csv", header=T, sep=",")
