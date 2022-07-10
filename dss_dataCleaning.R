# Read files - Can be found on LTC Github Repository
dss <- read_excel("dss.xlsx")            # Web scraped file from VA DSS site
lcf <- read_excel('lcf_buckets.xlsx')    # Manual mapping file

# View data summary
summary(dss)

# Remove ":" from inspector name column
dss$inspector_name<-gsub(":","",as.character(dss$inspector_name)) 

# Remove null standard numbers
dss1 <- dss[!is.na(dss$`standard_#`),]

# Remove standard numbers that do not contain "22VAC"
dss1 <- dss[grep("22VAC", dss$`standard_#`),] 

# Changed column name of Standard_ to StandardNumber
names(dss1)[24] <- 'StandardNumber' 

# Date column has 382 empty rows. 
# This separates the column into three separate ones.
# We will look into removing the empty rows later.
sum(is.na(dss1$date)) 
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
newX2 <- dss3[ dss3$CD %in% lcf$`Section Concat`,] 

# Renaming column to match for the merge
names(newX2)[32] <- 'Section Concat' 

# Joins the columns
df = merge(x=newX2,y=lcf,by="Section Concat") 

# Combines columns that were separated
df$StandardNumber <- paste0(df$A, "-", df$B, "-", df$C, "-", df$D) 

# Removing columns to clean up
df <- subset( df, select = -c(A,B,C,D ) ) 

# Removing column
df <- subset( df, select = -`Section Concat`) 

# The date column we split still contains 56 blank rows.
# That is less than 2% of the data frame, so it's safe to remove these rows.
sum(df$Month == '')
df <- df[df$Month != '',]
