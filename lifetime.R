#This function will create summary tables for juveniles, litters and lifetimes data from the cloud version of the krsp database.  

# Install packages as needed
# install.packages("tidyverse")
# install.packages("pedantics")

# Install krsp package from GitHub
# install.packages("devtools")
# library (devtools)
# devtools::install_github("KluaneRedSquirrelProject/krsp")

#library (plyr)  #Causes conflicts with dplyr - needs to load first
library (krsp)
library (dplyr)
library (lubridate)
select = dplyr::select #necessary as MASS also has a select function

# Connection to the AWS database
## Note that an additional connection might be needed if you also want to access the most recent year's data that havent been
## incorporated into the longterm data yet.
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)



# Summarize cones data
library(RCurl)
script <- getURL("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/cone_count_summaries.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

grids_cone_data<-cones_grids_years %>%
  ungroup() %>% 
  mutate(Year=as.integer(Year)) %>% 
  mutate(cones_t = cone_index_t) %>% 
  mutate(cones_tm1 = cone_index_tm1) %>% 
  select (Year, Grid, cones_t, cones_tm1, mast, Exp, EXP_label) %>% 
  mutate (cones_tm1 = ifelse(Year == 2005 & Grid == "AG", 1.045008523, cones_tm1)) 
#No cone data available for AG in 2004 so this needs to be added - assumed to be equal to cone index on LL

#Generation in Experiment (or control)
fla<-read.csv("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/generations3.csv", header=T, stringsAsFactors = FALSE) %>% 
  select (squirrel_id, generation, maternal_generation) %>% 
  mutate(squirrel_id=as.integer(squirrel_id)) 


flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  # exclusions
  filter(gr %in% c("SU", "KL", "CH", "AG", "LL", "JO")) %>% 
  select(squirrel_id, gr, byear=byear, dam_id, sire_id, bcert=bcert)

flastall<-collect(flastall)


# Grid density
script <- getURL("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/density.R", ssl.verifypeer = FALSE)
eval(parse(text = script))


density <- grids_density %>% 
  select(year, grid, spr_density)


#Add in variable <bucket> that indicates whether a female owned a bucket
food_add <- read.csv("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/fed_squirrels.csv") %>% 
  filter(!is.na(squirrel_id_spring)) %>% 
  mutate(bucket = "Y") %>% 
  select(squirrel_id = squirrel_id_spring, year, bucket)


juvenile <- tbl(con, "juvenile") %>% 
  select(squirrel_id, litter_id, sex, weight, tagwt = tagWT) %>% 
  collect() %>% 
  #Correct known errors
  mutate (sex = ifelse (squirrel_id %in% c(6134, 5893, 5130, 4549, 4918, 7839, 5893), "M", sex)) %>% 
  mutate (sex = ifelse (squirrel_id%in% c(7905, 8398, 7269, 6333), "F", sex))

# litter level information
litter <- tbl(con, "litter") %>% 
  collect() %>% 
  #Correct known errors
  mutate (ln = ifelse(id==6049, 2, ln)) %>% 
  mutate (ln = ifelse(id==4662, 2, ln)) %>% 
  mutate (ln = ifelse(id==4663, 3, ln)) %>% 
  mutate (br = ifelse(id==4663, 2, br)) %>% 
  mutate (yr = ifelse(id==2781, 2011, yr)) %>% 
  mutate (grid = ifelse(id==3148, "AG", grid)) %>% 
  
  # exclusions
  filter(yr >= 1989,
         !grid %in% c("EN", "FL", "SX"),
         !(grid == "AG" & yr < 2004),
         !(grid == "LL" & yr >= 1997 & yr <= 2003),
         !(grid == "LL" & yr >= 2012),
         !(grid == "JO" & yr >= 2012),
         !(grid == "CH" & yr >= 2012)) %>% 
  select(litter_id = id,
         year = yr, field_bdate = fieldBDate, n1_date = date1, tag_date = tagDt,
         mother_id = squirrel_id, grid, locx, locy, ln, food)
results <- inner_join(juvenile, litter, by = "litter_id")
results <- collect(results)
rm(litter, juvenile)

# flastall - juvenile
flastall_juv <- tbl(con, "flastall2") %>% 
  select(squirrel_id, date_end = datee, fate_end = f2) %>% 
  collect()
results <- left_join(results, flastall_juv, by = "squirrel_id")
rm(flastall_juv)

# flastall - mother
flastall_mom <- tbl(con, "flastall2") %>% 
  select(mother_id = squirrel_id, mother_byear = byear, mother_bcert = bcert) %>% 
  collect()
results <- left_join(results, flastall_mom, by = "mother_id")
rm(flastall_mom)


## Known errors.  These squirrels were listed as litter_id=5032 but this is incorrect. Need to figure out a new litter_id for them.  I have used 9999 in the mean time
results<-results %>% 
  mutate (litter_id = ifelse(squirrel_id==13172, "9999", litter_id),
          litter_id = ifelse(squirrel_id==13173, "9999", litter_id),
          litter_id = ifelse(squirrel_id==13174, "9999", litter_id),
          litter_id = ifelse(squirrel_id==13175, "9999", litter_id))

# clean up variables
results <- results %>% 
  # ids imported as numeric, convert back to integer
  mutate(mother_id = as.integer(mother_id),
         squirrel_id = as.integer(squirrel_id),
         litter_id = as.integer(litter_id)) %>% 
  # convert locs to numeric values
  mutate(locx = loc_to_numeric(locx),
         locy = loc_to_numeric(locy)) %>% 
  # convert dates from character to date class
  mutate(field_bdate = ymd(field_bdate),
         n1_date = ymd(n1_date),
         tag_date = ymd(tag_date),
         date_end = ymd(date_end)) %>% 
  #calculates mom age
  mutate(dam_age = ifelse(mother_bcert=="Y", year - mother_byear, NA_real_))


# calculate litter size
results <- results %>% 
  group_by(litter_id) %>% 
  dplyr::summarize(litter_size = n()) %>% 
  inner_join(results, ., by = "litter_id")
# add aditional fields
results <- results %>% 
  mutate(
    # growth rate
    nest_days = as.numeric(difftime(tag_date, n1_date, units = "days")),
    growth = (tagwt - weight) / nest_days,
    # calculate birth date as julian day
    bdate = yday(field_bdate),
    # calculate age as of last sighting
    age_last = as.integer(difftime(date_end, field_bdate, units = "days")))
# there are various exlcusions for which growth rate shouldn't be calculated
results <- results %>% 
  mutate(
    # n1 weight between 0 and 50 g
    growth = if_else(is.na(weight) | !between(weight, 1, 50),
                     NA_real_, growth),
    # tag weight between 0 and 100 g
    growth = if_else(is.na(tagwt) | !between(tagwt, 1, 100),
                     NA_real_, growth),
    # tag date at least 5 days after n1 date
    growth = if_else(nest_days < 5, NA_real_, growth),
    # exclude food manipulation experiments - want to keep these for now
    #growth = if_else(food %in% c(1, 4, 5, 6, 7, 9, 10, 13),
    #                 NA_real_, growth),
    growth = if_else(tagwt-weight==0,
                     NA_real_, growth)) %>% 
  select(-nest_days)


results <- results %>% 
  mutate(
    # did the squirrel survive to 200 days?
    survived_200d = !(age_last < 200))

# remove fitness for LL 2012 and CH 2011
results <- results %>% 
  mutate(survived_200d = ifelse(grid == "LL" & year == 2012, NA, survived_200d),
         survived_200d = ifelse(grid == "CH" & year == 2011, NA, survived_200d),
         survived_200d = ifelse(year == max(year), NA, survived_200d))

# output
# convert T/F to 1/0
results<-results %>% 
  mutate(survived_200d = as.integer(survived_200d)) 

#  Add in cones and density
results<-results %>% 
  left_join(density, by=c("year", "grid")) %>% 
  left_join(grids_cone_data, by=c("year"="Year", "grid"="Grid")) 

temp_cones<-grids_cone_data %>% 
  select(year = Year, grid = Grid, mom_mast=mast, mom_cones_t = cones_t)

results<- results%>% 
  left_join(temp_cones, by = c("mother_byear"="year", "grid")) 


results<-results %>% 
  # left join will give NAs when squirrel/year isn't in food_add
  left_join(food_add, by = c("mother_id"="squirrel_id", "year")) %>% 
  distinct(squirrel_id, .keep_all = TRUE) %>% 
  #This is needed because otherwise juvenile rows will be duplicated for each match in the join.  This is a potentially big problem!
  # fill NAs with "0"
  mutate(dam_bucket = coalesce(bucket, "0")) %>% 
  mutate(dam_bucket2 = if_else(year %in% c(2011, 2015)&grid %in% c("AG", "JO", "LL"), "1", dam_bucket)) %>% 
  # 2011 and 2015 have dam_bucket==0 and dam_bucket2==1
  mutate(dam_bucket = if_else(dam_bucket=="Y", "1", dam_bucket)) %>% 
  mutate (dam_bucket = as.numeric(dam_bucket)) %>% 
  mutate(dam_bucket2 = if_else(dam_bucket2=="Y", "1", dam_bucket2)) %>% 
  mutate (dam_bucket2 = as.numeric(dam_bucket2))


##  Litter Data
litter <- tbl(con, "litter") %>% 
  # exclusions
  filter(yr >= 1989,
         grid %in% c("KL", "SU", "CH", "AG", "LL", "JO"), 
         ln==1,
         !(grid == "AG" & yr < 2004),
         !(grid == "LL" & yr >= 1997 & yr <= 2003),
         !(grid == "LL" & yr >= 2012),
         !(grid == "JO" & yr >= 2012),
         !(grid == "CH" & yr >= 2012),
         !(grid == "AG" & yr >= 2018)) %>% 
  select(litter_id = id,
         year = yr, field_bdate = fieldBDate, n1_date = date1, tag_date = tagDt,squirrel_id, grid, locx, locy, ln, food)

litter<-collect(litter)


dam_fit <- results %>% 
  group_by(litter_id) %>% 
  dplyr::summarize(litter_fit=sum(survived_200d), mean_growth=mean(growth, na.rm=T), ls = mean(litter_size, na.rm=T)) %>% 
  mutate(litter_id = as.integer(litter_id))

dam_fit2<-results %>% #Sum of all offspring from that year regardless of litter number
  group_by(mother_id, year) %>% 
  dplyr::summarize(all_litters_fit=sum(survived_200d))

litter<- litter %>% 
  mutate(grid=factor(grid, levels = c("AG", "CH", "JO", "KL", "LL", "SU"))) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         litter_id = as.integer(litter_id)) %>% 
  mutate(field_bdate = ymd(field_bdate),
         n1_date = ymd(n1_date),
         tag_date = ymd(tag_date)) %>% 
  mutate(yrf=factor(year)) 

temp_cones2<-grids_cone_data %>% 
  select(year = Year, grid = Grid, by_mast=mast, by_cones_t = cones_t)



litter<-litter %>% 
  left_join(grids_cone_data, by =c("year" = "Year", "grid" = "Grid")) %>% 
  select(litter_id, food, grid, ln, squirrel_id, year, yrf, field_bdate, cones_tm1, Exp, EXP_label, cones_t, mast) %>% 
  left_join(flastall, by="squirrel_id") %>% 
  mutate(age=year-byear) %>% 
  left_join(temp_cones2, by =c("byear" = "year", "grid")) %>%    
  left_join(dam_fit, by="litter_id") %>%
  left_join(dam_fit2, by= c("squirrel_id" = "mother_id", "year"))  %>% 
  left_join(fla, by="squirrel_id") %>%
  left_join(density, by=c("year", "grid")) %>%  
  select(litter_id, food, grid, ln, squirrel_id, year, yrf, field_bdate, mean_growth, ls, cones_t, cones_tm1, mast, spr_density, by_mast, by_cones_t, Exp, EXP_label, byear, bcert, age, litter_fit, all_litters_fit, sire_id, dam_id, maternal_generation, generation)

rm(dam_fit, temp_cones2)  

litter<-litter %>% 
  mutate(Julian=yday(field_bdate),
         grid=factor(grid),
         yrf=factor(yrf)) %>% 
  collect()

litter <- litter %>% 
  # left join will give NAs when squirrel/year isn't in food_add
  left_join(food_add, by = c("squirrel_id", "year")) %>% 
  distinct(litter_id, .keep_all = TRUE) %>% 
  # fill NAs with "0"
  mutate(bucket = coalesce(bucket, "0")) %>% 
  mutate(bucket2 = if_else(year %in% c(2011, 2015)&grid %in% c("AG", "JO", "LL"), "1", bucket)) %>% 
  # 2011 and 2015 have bucket==0, and bucket2==1
  mutate(bucket = if_else(bucket=="Y", "1", bucket)) %>% 
  mutate (bucket = as.numeric(bucket)) %>% 
  mutate(bucket2 = if_else(bucket2=="Y", "1", bucket2)) %>% 
  mutate (bucket2 = as.numeric(bucket2))

# Standardize litter table within grid-years
library (plyr)
litter<-ddply(litter, c("yrf", "grid"), transform, std_Growt = scale(mean_growth))
litter<-ddply(litter, c("yrf", "grid"), transform, std_Julian = scale(Julian))
litter<-ddply(litter, c("yrf", "grid"), transform, std_LS = scale(ls))
litter<-ddply(litter, c("yrf", "grid"), transform, w_litter_fit = litter_fit/mean(litter_fit, na.rm=T))
#Check that this worked
with(litter, tapply(std_Julian, list(yrf, grid), mean, na.rm=T))
with(litter, tapply(std_Julian, list(yrf, grid), sd, na.rm=T))
with(litter, tapply(w_litter_fit, list(yrf, grid), mean, na.rm=T))
with(litter, tapply(w_litter_fit, list(yrf, grid), var, na.rm=T))

detach(package:plyr)

#Generate lifetime data
lifetime<-tbl(con, "flastall2") %>% 
  filter(byear >= 1989,
         gr %in% c("AG", "CH", "JO", "KL", "LL", "SU"),
         bcert=="Y",
         !(f2 %in% c("4", "5", "11", "12", "22"))) %>% 
  select(squirrel_id, grid=gr, sex, dates, f1, byear=byear, litter_id, dam_id, sire_id, bcert=bcert, new_grid=newgr, new_sex=newsex, datee, f2, locX, locY) %>% 
  collect()

##  Note there are many behavioural records for squirrel_id=6230 that dont seem like they should be for her.  dateE should be 1999-08-06
## There seems to be a dead squirrel (squirrel_id = 7581) that showed up in the census when dead
## There are two litters for squirrel_id = 11486 (F5874/F7334; C1.) that are squished together as the same litter_id = 5032.  This is the correct id for some of the juves but the rest are from a second litter.  Need to get proper litter info for this litter and assign a new litter id.

#fix known errors
lifetime<-lifetime %>% 
  mutate (datee = ifelse(squirrel_id==6230, "1999-08-06", datee)) %>% 
  mutate (datee = ifelse(squirrel_id==7581, "1998-06-26", datee)) %>% 
  mutate (sex = ifelse (squirrel_id %in% c(6134, 5893, 5130, 4549, 4918, 7839, 5893), "M", sex)) %>% 
  mutate (sex = ifelse (squirrel_id==7905, "F", sex)) %>%
  mutate (sex = ifelse (squirrel_id==7269, "F", sex)) %>%
  mutate (sex = ifelse (squirrel_id==8398, "F", sex)) %>% 
  mutate (sex = ifelse (squirrel_id==6333, "F", sex))

#Link in juvenile data
results_temp<-results %>% 
  select(squirrel_id, field_bdate, bdate, age_last, birth_litter_size=litter_size, growth, birth_locx=locx, birth_locy=locy, dam_age, mother_byear)

lifetime<-lifetime %>% 
  left_join(results_temp, by="squirrel_id") %>% 
  #left_join(food_coding, by = c("grid", "byear"="yr")) %>% 
  left_join(grids_cone_data, by = c("grid"="Grid", "byear"="Year")) %>% 
  left_join(density, by = c("grid", "byear"="year")) %>% 
  left_join(fla, by="squirrel_id")

rm(results_temp)


lifetime<- lifetime%>% 
  left_join(temp_cones, by = c("mother_byear"="year", "grid")) 

rm(temp_cones)

lifetime<-lifetime %>% 
  mutate(dateE=ymd(datee),
         dateS=ymd(dates),
         field_bdate=ymd(field_bdate),
         longevity = as.integer(difftime(datee, field_bdate, units = "days")),
         grid=as.factor(grid),
         sex=as.factor(sex),
         bcert=as.factor(bcert))

## Calculate LBS
lifetime <- lifetime %>% 
  group_by(dam_id) %>% 
  summarize(dam_lbs = n()) %>% 
  left_join(lifetime, ., by = c("squirrel_id"= "dam_id"))

lifetime <- lifetime %>% 
  group_by(sire_id) %>% 
  summarize(sire_lbs = n()) %>% 
  left_join(lifetime, ., by = c("squirrel_id"= "sire_id"))

lifetime<-lifetime %>% 
  mutate(dam_lbs = ifelse(is.na(dam_lbs)&sex=="F", 0, dam_lbs),
         sire_lbs = ifelse(is.na(sire_lbs)&sex=="M", 0, sire_lbs),
         sire_lbs = ifelse(byear>2001, sire_lbs, NA_real_)) 


## Calculate LRS
lifetime <- lifetime %>% 
  filter(longevity > 199) %>% 
  group_by(dam_id) %>% 
  summarize(dam_lrs = n()) %>% 
  left_join(lifetime, ., by = c("squirrel_id"= "dam_id"))

lifetime <- lifetime %>% 
  filter(longevity > 199) %>% 
  group_by(sire_id) %>% 
  summarize(sire_lrs = n()) %>% 
  left_join(lifetime, ., by = c("squirrel_id"= "sire_id"))

lifetime<-lifetime %>% 
  mutate(dam_lrs = ifelse(is.na(dam_lrs)&sex=="F", 0, dam_lrs),
         sire_lrs = ifelse(is.na(sire_lrs)&sex=="M", 0, sire_lrs),
         sire_lrs = ifelse(byear>2001, sire_lrs, NA_real_))


lifetime<-lifetime %>% 
  mutate(LBS = coalesce(lifetime$dam_lbs, lifetime$sire_lbs),
         LRS = coalesce(lifetime$dam_lrs, lifetime$sire_lrs))

# LRS not defined for squirrels that did not survive to 200 days of age.
lifetime<-lifetime %>% 
  mutate(LRS = ifelse(longevity > 199, LRS, NA_real_))

library (plyr)
lifetime<-ddply(lifetime, c("byear", "grid"), transform, std_bdate = scale(bdate))
lifetime<-ddply(lifetime, c("byear", "grid"), transform, std_growth = scale(growth))
lifetime<-ddply(lifetime, c("byear", "grid"), transform, std_birth_litter_size = scale(birth_litter_size))
lifetime<-ddply(lifetime, c("byear", "grid"), transform, w_dam_lrs = dam_lrs/mean(dam_lrs, na.rm=T))
lifetime<-ddply(lifetime, c("byear", "grid"), transform, w_sire_lrs = sire_lrs/mean(sire_lrs, na.rm=T))

detach(package:plyr)

#Link some adult traits
first_litter_means<-litter %>% 
  filter (ln==1) %>% 
  group_by(squirrel_id) %>%
  dplyr::summarize(mean_std_julian = mean(std_Julian, na.rm=T),
                   mean_std_LS = mean(std_LS, na.rm=T), 
                   mean_std_growth = mean(std_Growt, na.rm=T))

lifetime<-lifetime %>% 
  left_join(first_litter_means, by = "squirrel_id")

rm(first_litter_means)

#Substitute zeros for missing adult traits (i.e. did not survive or was a male!)
lifetime<-lifetime %>% 
  mutate(mean_std_julian = ifelse(is.na(mean_std_julian), 0, mean_std_julian),
         mean_std_LS = ifelse(is.na(mean_std_LS), 0, mean_std_LS),
         mean_std_growth = ifelse(is.na(mean_std_growth), 0, mean_std_growth))


#Substitute zeros for missing juvenile traits (i.e. did not survive)
lifetime<-lifetime %>% 
  mutate(std_bdate = ifelse(is.na(std_bdate), 0, std_bdate),
         std_birth_litter_size = ifelse(is.na(std_birth_litter_size), 0, std_birth_litter_size),
         std_growth = ifelse(is.na(std_growth), 0, std_growth))


lifetime<-results %>%
  select(squirrel_id, ln) %>% 
  right_join (lifetime, by = "squirrel_id") %>% 
  mutate(ln=as.factor(ln))


#  Update litter table with some information from lifetime
litter<-lifetime %>%
  select(squirrel_id, std_bdate) %>% 
  right_join(litter, by="squirrel_id") %>% 
  mutate(std_mom_bdate = std_bdate) %>% 
  select (-std_bdate)

##  Any Missing Data in Lifetime?
good_litters<-c("0", "1", "2", "4", "6", "7")
missed_litters<-c("3", "5")

good_litter_records <- tbl(con, "litter") %>% 
  filter(yr >= 1989,
         grid %in% c("KL", "SU", "CH", "AG", "LL", "JO"), 
         br %in% good_litters) %>% 
  select(litter_id = id, squirrel_id) %>% 
  group_by(squirrel_id) %>% 
  dplyr::summarize(num_good_litters = n()) %>%
  collect()


bad_litter_records <- tbl(con, "litter") %>% 
  # exclusions
  filter(yr >= 1989,
         grid %in% c("KL", "SU", "CH", "AG", "LL", "JO"), 
         br %in% missed_litters) %>% 
  select(litter_id = id, squirrel_id) %>% 
  group_by(squirrel_id) %>% 
  dplyr::summarize(num_missed_litters = n()) %>%
  collect()

manipulation_codes<-c("1", "4", "5", "9", "10", "13", "14", "15", "16", "17")

manipulated_litter_records <- tbl(con, "litter") %>% 
  # exclusions
  filter(yr >= 1989,
         grid %in% c("KL", "SU", "CH", "AG", "LL", "JO"), 
         food %in% manipulation_codes) %>% 
  select(litter_id = id, squirrel_id) %>% 
  group_by(squirrel_id) %>% 
  dplyr::summarize(num_manipulated_litters = n()) %>%
  collect()

afr <- tbl(con, "litter") %>% 
  collect()

afr<-afr %>% 
  left_join(flastall, by="squirrel_id") %>% 
  mutate(age=yr-byear) %>%
  filter (br != 0) %>% 
  select(squirrel_id, age) %>%
  group_by(squirrel_id) %>% 
  dplyr::summarize(afr = min(age),
                   alr = max(age))


lifetime<-lifetime %>% 
  left_join(good_litter_records, by="squirrel_id") %>% 
  left_join(bad_litter_records, by="squirrel_id") %>% 
  left_join(manipulated_litter_records, by="squirrel_id") %>% 
  left_join(afr, by="squirrel_id") %>% 
  left_join(food_add, by=c("dam_id" = "squirrel_id", "byear" = "year")) %>% 
  distinct(squirrel_id, .keep_all = TRUE) %>% 
  mutate(bucket = coalesce(bucket, "0")) %>% 
  mutate(bucket2 = if_else(byear %in% c(2011, 2015)&grid %in% c("AG", "JO", "LL"), "1", bucket)) %>% 
  # 2011 and 2015 have bucket==0, bucket2==1
  mutate(bucket = if_else(bucket=="Y", "1", bucket)) %>% 
  mutate (bucket = as.numeric(bucket)) %>% 
  mutate(bucket2 = if_else(bucket2=="Y", "1", bucket2)) %>% 
  mutate (bucket2 = as.numeric(bucket2)) %>% 
  mutate(num_missed_litters = ifelse(is.na(num_missed_litters), 0, num_missed_litters))

  # Food add as adult.  Codes bucket.as.adult based on whether the squirrel owned a bucket as an adult.
  food_add_temp<-food_add %>% 
  select(squirrel_id, bucket.as.adult=bucket)
  
  lifetime<-lifetime %>% 
  left_join(food_add_temp, by = c("squirrel_id")) %>% 
  distinct(squirrel_id, .keep_all = TRUE) %>% 
  # fill NAs with "0"
  mutate(bucket.as.adult = coalesce(bucket.as.adult, "0")) %>% 
  mutate(bucket.as.adult = if_else(bucket.as.adult=="Y", "1", bucket.as.adult)) %>% 
  mutate (bucket.as.adult = as.numeric(bucket.as.adult))
  
rm(food_add_temp)  

rm(afr, bad_litter_records, good_litter_records, good_litters, manipulated_litter_records, manipulation_codes, missed_litters) 

# Lifetime Clean
lifetime_clean<-lifetime %>%
  filter (byear<2012, num_missed_litters<1, 
          is.na(alr)|is.na(afr)|num_good_litters - (alr-afr) >0)

# Clean Up
rm(con, density, fla, flastall, food_add, grids_cone_data)




