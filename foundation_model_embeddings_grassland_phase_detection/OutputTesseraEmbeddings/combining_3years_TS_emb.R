setwd("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/extract_embeddings/OutputTesseraEmbeddings")
library(stringr)

# 2019 # 

d1 <- read.csv("Tessera_inventory_2019_3yr.csv", header=T, sep=",")
names(d1)


replacements <- c("_2017_" = "_2_", 
                  "_2018_" = "_1_", 
                  "_2019_" = "_0_")

names(d1) <- str_replace_all(names(d1), replacements)

# 2020 # 

d2 <- read.csv("Tessera_inventory_2020_3yr.csv", header=T, sep=",")
names(d2)


replacements <- c("_2018_" = "_2_", 
                  "_2019_" = "_1_", 
                  "_2020_" = "_0_")

names(d2) <- str_replace_all(names(d2), replacements)

# 2021 #
d3 <- read.csv("Tessera_inventory_2021_3yr.csv", header=T, sep=",")
names(d3)


replacements <- c("_2019_" = "_2_", 
                  "_2020_" = "_1_", 
                  "_2021_" = "_0_")

names(d3) <- str_replace_all(names(d3), replacements)

# 2022 #
d4 <- read.csv("Tessera_inventory_2022_3yr.csv", header=T, sep=",")
names(d4)


replacements <- c("_2020_" = "_2_", 
                  "_2021_" = "_1_", 
                  "_2022_" = "_0_")

names(d4) <- str_replace_all(names(d4), replacements)

# 2023 #
d5 <- read.csv("Tessera_inventory_2023_3yr.csv", header=T, sep=",")
names(d5)


replacements <- c("_2021_" = "_2_", 
                  "_2022_" = "_1_", 
                  "_2023_" = "_0_")

names(d5) <- str_replace_all(names(d5), replacements)


d <- rbind(d1,d2,d3,d4,d5)

write.csv(d,"TS_3y_embeddings.csv")

test <- read.csv("TS_3y_embeddings.csv", header=T, sep=",")
