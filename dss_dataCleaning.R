# Read DSS file
dss <- read_excel("Downloads/dss.xlsx") 
View(dss)

# Remove : from inspector name
dss$inspector_name<-gsub(":","",as.character(dss$inspector_name)) 
View(dss)

# Remove empty standard number
dss1<- dss[!dss$`standard_#`=="",] 

# Remove values that do not contain 22VAC in standard number
dss1 <- dss[grep("22VAC", dss$`standard_#`),] 

# Changed column name of Standard_ to StandardNumber
names(dss1)[24] <- 'StandardNumber' 

# This row has 106 empty rows. This seperates the column into three columns
dss1 <- dss1 %>% separate(date, c('Month', 'Day', 'Year')) 

# This splits the city_zip column into two.
dss1 <- dss1 %>% extract(city_zip, c("City", "State"), "([^,]+), ([^)]+)") 

# This splits the state column. Creating a new ZIP column. 
dss1 <- dss1 %>% separate(State, c('State', 'ZIP')) 

# Seperating Standard number into 4 columns
dss3<- dss1 %>% separate(StandardNumber, into = c("A", "B", "C", "D")) 

# Making NA into Blanks
dss3[is.na(dss3)] <- "" 

# Combine the two columns
dss3$CD <- paste0(dss3$C, dss3$D)

# Matches rows from columns and drops the rest. 
newX2 <- dss3[ dss3$CD %in% LCF_Buckets$`Section Concat`,] 

# Renaming column to match for the merge
names(newX2)[32] <- 'Section Concat' 

# Joins the columns
df = merge(x=newX2,y=LCF_Buckets,by="Section Concat") 

# Combines columns that were separated
df$StandardNumber <- paste0(df$A, "-", df$B, "-", df$C, "-", df$D) 

# Removing columns to clean up
df <- subset( df, select = -c(A,B,C,D ) ) 

# Removing column
df <- subset( df, select = -`Section Concat`) 
Collapse