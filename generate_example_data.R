# create sample fundraising data 

set.seed(2)

library(randomNames)
library(zipcode)
library(tidyverse)

data("zipcode")

# remove zips with missing cords from zip data set 
zipcode <- filter(zipcode, !is.na(longitude))


# create round up function
roundUp <- function(x,to=10){to*(x%/%to + as.logical(x%%to))}

# insert 0's into giving data 
insert_NAs <- function(x) {
  len <- length(x)
  n <- sample(1:floor(0.4*len), 1)
  i <- sample(1:len, n)
  x[i] <- NA 
  x
}

#function which returns a matrix, and takes column vectors as arguments for mean and sd
normv <- function( n , mean , sd ){
  out <- rnorm( n*length(mean) , mean = mean , sd = sd )
  return( matrix( out , , ncol = n , byrow = FALSE ) )
}


# variables needed 
  # id 
  # name
  # annual gift by year 
  # capital commitments by year
  # zip code 
  # class year 
  # constituency 
  # status 
  # rating 
  # proposal amount 
  # proposal priority 
  # gift officer


# define values for below 
# years of data 
years <- 5

#observations 
obs <- 10000

# gift officers 
officers <- 20


df <- data.frame(
  mean=c(rep(100, years),    # annual fund
         rep(10000, years),  # capital 
         100000,             # rating
         500000),            # proposal 
  sd=c(rep(1000, years),
       rep(100000, years),
       1000000,
       500000))


df <- data.frame(t(normv(obs , df$mean , df$sd )))

colnames(df) <- c("af12", "af13", "af14", "af15", "af16",
                  "cap12", "cap13", "cap14", "cap15", "cap16",
                  "rating", "proposal")

# clean continuous data 
# negative values to zero or missing(rating)


df <- data.frame(lapply(df, function(x) x <- ifelse(x < 0, 0, round(x))))

df <- data.frame(
  cbind(df[!(colnames(df) %in% c("cap12", "cap13", "cap14", "cap15", "cap16", "rating"))], 
        lapply(df[, c("cap12", "cap13", "cap14", "cap15", "cap16", "rating")], function(x) roundUp(x, 10000))))

for(i in 1:years){
  df$proposal <- insert_NAs(df$proposal)
}


df$rating <- insert_NAs(df$rating)

# create class year 
df$class <-  sample(1950:2016, obs, replace = TRUE)
df$class <- insert_NAs(df$class)  

# create categorical vars 

# constituency 
df$constituency <- ifelse(is.na(df$class), "Parent", "Alumn")

# status
df$status <- ifelse(
  df$rating > 100000,
  sample(c("Assessment", "Cultivation", "Stewardship", "Solicitation", "Not a Prospect", "Annual Fund Leadership"), obs, replace = TRUE, prob = c(.3, .3, .1, .1, .1, .1)),
  sample(c("Assessment", "Cultivation", "Stewardship", "Solicitation", "Not a Prospect", "Annual Fund Leadership"), obs, replace = TRUE, prob = c(.05, .05, .05, .01, .3, .54)))

# proposal priority 
df$priority <- ifelse(!is.na(df$proposal), 
                      sample(c("Athletics", "Math/Science Center", "Financial Aid", "Residence Hall", "Professorship", "Unresistricted"), obs, replace = TRUE, prob = c(.3, .2, .2, .14, .14, .02)),
                      "")

# create prospect name 
df$name <- randomNames(n = obs)

# create gift officer name 
officers <- randomNames(n = officers)
p.assigned <- (length(officers) * 150)/(sum(df$rating > 100000, na.rm = TRUE)) # calculate percentage of prospects to be assigned 

officers <- c(officers, rep(NA, round(length(officers)/(p.assigned))))

df$officer <- ifelse(df$rating > 100000,
                     sample(officers, replace = TRUE),
                     sample(c(officers, rep(NA, round(length(officers)/(p.assigned)))), replace = TRUE))


# create address info 
zips <- sample(zipcode$zip, round(obs*.5), replace = TRUE)   # pick random sample of zips 
zips <- data.frame(zip = sample(zips, obs,  replace = TRUE))  # ensure clustering by repeating sampling within smaller group
df$zip <- zips$zip

# create last contact date 
df$last.contact <- sample(seq(as.Date('2009/01/01'), as.Date(Sys.Date()), by="day"), obs, replace  = TRUE)
df$last.contact <- insert_NAs(df$last.contact)

# create connection score 
df$connection <- sample(c("Very Connected", "Connected", "Somewhat Connected", "Modestly Connected", "Not Connected"), 
                        prob = c(.15, .2, .3, .2, .15), obs, replace  = TRUE)

# write out sample data 
tmp <- 
  df %>%
    select(name, class, rating, status, constituency, connection, officer, proposal, priority, zip,  1:11)  %>% 
    left_join(zipcode)

write.csv(tmp, "example.csv", row.names = FALSE)

