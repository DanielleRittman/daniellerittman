install.packages("readr")
install.packages("dplyr")
install.packages("maps")
install.packages("choroplethr")
install.packages("choroplethrMaps")

library(readr)
library(stringr)
library(maps)
library(dplyr)
library(readxl)
library("choroplethrMaps")
library("choroplethr")
library(RColorBrewer)
library(ggplot2)
citation("maps")
citation("choroplethr")
citation("choroplethrMaps")

# Load data #### 
AMA <- read_excel("AMA.xlsx")
View(AMA)

County_Population <- read_excel("County Population.xlsx")
View(County_Population)

# Create a single variable to store the county name #### 
dat.county <- select(County_Population, COUNTY, X__1:X__4)
dat.county[is.na(dat.county)] <- ""
  dat.county$County = paste(dat.county$COUNTY, dat.county$X__1, dat.county$X__2, dat.county$X__3, dat.county$X__4)
County_Population$County <- dat.county$County
County_Population$County <- str_replace(string = County_Population$County, pattern="[ ]+$", replacement="")

# Merge populaition data with number of psychiatrists #### 
dat.merged <- left_join(County_Population, AMA, by=c("STNAME"="STATE", "County"="COUNTY"))

# Confirm that Baldwin county is matched between the two data frames. 
# "Baldwin" %in% sort(setdiff(County_Population$County, AMA$COUNTY))

# Calculate ratios #### 
dat.merged$ratio.addiction <- dat.merged$`Addiction Psychiatry` / dat.merged$`2017 Population`
dat.merged$ratio.child <- dat.merged$`Child Psychiatry` / dat.merged$`2017 Population`
dat.merged$ratio.geriatric <- dat.merged$`Geriatric Psychiatry` / dat.merged$`2017 Population`
dat.merged$ratio.psychiatrist <- dat.merged$Psychiatry / dat.merged$`2017 Population`

##export##
install.packages("xlsx")
library(xlsx)
write.xlsx(dat.merged, "M:/dat.merged.xlsx")

quantile(dat.merged$ratio.addiction, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(dat.merged$ratio.child, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(dat.merged$ratio.geriatric, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(dat.merged$ratio.psychiatrist, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Make maps ###### 

data(county.fips)
data(county.map)
head(county.map, 200)

data(state.map)
head(state.map)
head(filter(state.map, region=="idaho"))

head(county.map, 100)
head(filter(county.map, LSAD=="Borough"), 100)




# Obtain the region number of each county #### 

# Obtain the two-digit ID for each state from state.map 
state.info <- distinct(select(state.map, STATE, region))
state.info <- rename(state.info, State.Name=region)

# Merge the alphabetic state names into county.map 
county.info <- full_join(county.map, state.info, by="STATE")

# Remove redundant rows from county.info 
head(select(county.info, State.Name, NAME, region), 100)
county.info.forMerging <- distinct(select(county.info, State.Name, NAME, region), 
                                   State.Name, NAME, .keep_all=TRUE)

# Merge the county region id into our full dataframe (dat.merged) ##### 
dat.merged$State.LowerCase <- tolower(dat.merged$STNAME)


"Fairbanks North Star" %in% county.map$NAME

# Drop: 
#   Census Area 
#   Burough 
#   Municipality 

setdiff(county.info.forMerging$NAME, dat.merged$County)
setdiff(dat.merged$County, county.info.forMerging$NAME)

dat.merged$County[which(dat.merged$County == "Do�a Ana")] <- "Dona Ana" 
county.info.forMerging$NAME[which(county.info.forMerging$NAME == "Wade Hampton")] <- "Kusilvak"
county.info.forMerging$NAME[which(county.info.forMerging$NAME == "Shannon" & county.info.forMerging$State.Name == "south dakota")] <- "Oglala Lakota"
county.info.forMerging$NAME[which(county.info.forMerging$NAME == "La Salle" & county.info.forMerging$State.Name == "louisiana")] <- "LaSalle"

county.formatted <- str_replace(string=dat.merged$County, pattern=" Census Area", replacement="")
county.formatted <- str_replace(string=county.formatted, pattern=" City and Borough", replacement="")
county.formatted <- str_replace(string=county.formatted, pattern=" Borough", replacement="")
county.formatted <- str_replace(string=county.formatted, pattern=" Municipality", replacement="")

setdiff(county.info.forMerging$NAME, county.formatted)
setdiff(county.formatted, county.info.forMerging$NAME)

setdiff(paste(county.info.forMerging$NAME, county.info.forMerging$State.Name), 
        paste(county.formatted, dat.merged$State.LowerCase))

setdiff(paste(county.formatted, dat.merged$State.LowerCase), 
        paste(county.info.forMerging$NAME, county.info.forMerging$State.Name))


View(dat.merged[dat.merged$County == "Baltimore" & dat.merged$State.LowerCase == "maryland", ])
View(dat.merged[dat.merged$County == "Fairfax" & dat.merged$State.LowerCase == "virginia", ])
View(dat.merged[dat.merged$County == "Franklin" & dat.merged$State.LowerCase == "virginia", ])
View(dat.merged[dat.merged$County == "Richmond" & dat.merged$State.LowerCase == "virginia", ])
View(dat.merged[dat.merged$County == "Roanoke" & dat.merged$State.LowerCase == "virginia", ])
View(dat.merged[dat.merged$County == "St. Louis" & dat.merged$State.LowerCase == "missouri", ])

dat.merged <- dat.merged %>% group_by(County, State.LowerCase) %>% 
  mutate(population.rank=order(`2017 Population`, decreasing=TRUE)) %>% 
  filter(population.rank==1)

dat.merged$county.formatted <- county.formatted

dat.merged.region <- full_join(
  dat.merged, county.info.forMerging, by=c("State.LowerCase"="State.Name", "County"="NAME"))

# Make the choropleth maps #### 

# heatmap, child #### 
df.child <- data.frame(value=fulldata$ratio_child * 1e6, region=fulldata$CTY_FIPS)
df.child <- df.child[!is.na(df.child$region),]
df.child$region <- as.numeric(as.character(df.child$region))
duplicated(df.child$region)

# county_choropleth(df.child, "Child Psychiatrists (per million people)") 

# https://www.r-bloggers.com/advanced-choroplethr-changing-color-scheme/

# Previous version of the code 
# col.pal <- colorRampPalette(c("lightblue", "blue"))( 7 )
# choro1<- CountyChoropleth$new(df.child) 
# choro1$ggplot_scale <- scale_fill_manual(name="Professionals per 1,000,000 residents",
#                                          values=col.pal, drop=FALSE)
# choro1$render()
# ggsave("childPsych_heatmap.png")

# display quantiles of the data (this explains why only three colors were initially displayed)
quantile(df.child$value, probs=seq(from=0.01, to=0.99, length.out=7), na.rm = TRUE)

col.pal <- colorRampPalette(c("lightblue", "blue"))( 7 )
child.breaks <- c(quantile(df.child$value, probs=c(0, seq(from=0.82, to=0.99, length.out=6)), na.rm = TRUE), 
                  max(df.child$value, na.rm=TRUE))
child.factor <- cut(x=df.child$value, breaks=child.breaks)
df.child$value <- child.factor
choro1<- CountyChoropleth$new(df.child) 
choro1$ggplot_scale <- scale_fill_manual(name="Child Psychiatrists per 1,000,000 residents",
                                         values=col.pal, drop=FALSE)
choro1$render()
ggsave("childPsych_heatmap.png")



# heatmap, geriatric #### 
df.geriatric <- data.frame(value=fulldata$ratio_geriatric * 1e6, region=fulldata$CTY_FIPS)
df.geriatric <- df.geriatric[!is.na(df.geriatric$region),]
df.geriatric$region <- as.numeric(as.character(df.geriatric$region))

# Previous version of the code 
# col.pal <- colorRampPalette(c("lightblue", "blue"))( 7 )
# choro1<- CountyChoropleth$new(df.geriatric) 
# choro1$ggplot_scale <- scale_fill_manual(name="Professionals per 1,000,000 residents",
#                                          values=col.pal, drop=FALSE)
# choro1$render()
# ggsave("geriatricPsych_heatmap.png")

# check quantiles 
quantile(df.geriatric$value, probs=seq(0, 1, length.out = 100), na.rm = TRUE)
quantile(df.geriatric$value, probs=c(0, seq(from=0.95, to=0.99, length.out=6)), na.rm = TRUE)

col.pal <- colorRampPalette(c("lightblue", "blue"))( 7 )
geriatric.breaks <- c(quantile(df.geriatric$value, probs=c(0, seq(from=0.95, to=0.99, length.out=6)), na.rm = TRUE), 
                  max(df.geriatric$value, na.rm=TRUE))
geriatric.factor <- cut(x=df.geriatric$value, breaks=geriatric.breaks)
df.geriatric$value <- geriatric.factor

choro1<- CountyChoropleth$new(df.geriatric) 
choro1$ggplot_scale <- scale_fill_manual(name="Professionals per 1,000,000 residents",
                                         values=col.pal, drop=FALSE)
choro1$render()
ggsave("geriatricPsych_heatmap.png")


# heatmap, addiction #### 
df.addiction <- data.frame(value=dat.merged.region$ratio.addiction * 1e6, region=dat.merged.region$region)
df.addiction <- df.addiction[!is.na(df.addiction$region),]
df.addiction$region <- as.numeric(as.character(df.addiction$region))

col.pal <- colorRampPalette(c("lightblue", "blue"))( 7 )
choro1<- CountyChoropleth$new(df.addiction) 
choro1$ggplot_scale <- scale_fill_manual(name="Professionals per 1,000,000 residents",
                                         values=col.pal, drop=FALSE)
choro1$render()
ggsave("addictionPsych_heatmap.png")

# heatmap, psychiatrist #### 
df.psychiatrist <- data.frame(value=dat.merged.region$ratio.psychiatrist * 1e6, region=dat.merged.region$region)
df.psychiatrist <- df.psychiatrist[!is.na(df.psychiatrist$region),]
df.psychiatrist$region <- as.numeric(as.character(df.psychiatrist$region))

col.pal <- colorRampPalette(c("lightblue", "blue"))( 7 )
choro1<- CountyChoropleth$new(df.psychiatrist) 
choro1$ggplot_scale <- scale_fill_manual(name="Professionals per 1,000,000 residents",
                                         values=col.pal, drop=FALSE)
choro1$render()
ggsave("Psych_heatmap.png")


