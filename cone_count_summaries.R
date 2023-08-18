# This code will use our raw cone count data to generate annual and grid summaries of cone production.
#Andrew McAdam


library(tidyverse)
library (krsp)

#con <- krsp_connect(group="krsp-aws")
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)


selected_grids <- c("AG", "CH", "JO", "KL", "LL", "SU")

cone_counts<-tbl(con, "cones") %>%
  filter(Grid %in% selected_grids, Year>=1988) %>%
  collect()  %>%
  mutate(Year = as.numeric(Year), 
         NumNew = as.numeric(NumNew),
         cone_index = log(NumNew + 1),
         total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01)) # according to Krebs et al. 2012
         )



##################################
# Means calculated per Grid Year #
##################################
#detach(package:plyr)
cones_grids_years <- cone_counts %>% 
  group_by(Year, Grid) %>% 
  summarise(num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE),
            cone_index = mean(cone_index, na.rm = TRUE)) %>% 
  mutate(Year_tp1 = Year+1,
         cone_index_t = ifelse(is.finite(cone_index), cone_index, NA))

#link in cones from the previous year
cone_temp<-cones_grids_years %>% 
  select(Grid, Year, Year_tp1, cone_index_tm1=cone_index_t)

cones_grids_years<-left_join(cones_grids_years, cone_temp, by=c("Grid", "Year" = "Year_tp1")) %>% 
  select(-Year_tp1, -Year.y)

# Manually code mast years

cones_grids_years<-cones_grids_years %>%
  mutate (mast = "n") %>% 
  mutate (mast = ifelse(Grid=="KL"&Year==1993, "y", mast),
          mast = ifelse(Grid=="LL"&Year==1993, "y", mast),
          mast = ifelse(Grid=="SU"&Year==1993, "y", mast),
          mast = ifelse(Grid=="KL"&Year==1998, "y", mast),
          mast = ifelse(Grid=="LL"&Year==1998, "y", mast),
          mast = ifelse(Grid=="SU"&Year==1998, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2005, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2005, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2005, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2010, "y", mast),
          mast = ifelse(Grid=="LL"&Year==2010, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2010, "y", mast),
          mast = ifelse(Grid=="CH"&Year==2010, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2010, "y", mast),
          mast = ifelse(Grid=="AG"&Year==2010, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2014, "y", mast),
          mast = ifelse(Grid=="LL"&Year==2014, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2014, "y", mast),
          mast = ifelse(Grid=="CH"&Year==2014, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2014, "y", mast),
          mast = ifelse(Grid=="AG"&Year==2014, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2019, "y", mast),
          mast = ifelse(Grid=="LL"&Year==2019, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2019, "y", mast),
          mast = ifelse(Grid=="CH"&Year==2019, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2019, "y", mast),
          mast = ifelse(Grid=="AG"&Year==2022, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2022, "y", mast),
          mast = ifelse(Grid=="LL"&Year==2022, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2022, "y", mast),
          mast = ifelse(Grid=="CH"&Year==2022, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2022, "y", mast),
          mast = ifelse(Grid=="AG"&Year==2022, "y", mast)) %>% 
  mutate (Exp = "c") %>% 
  mutate (Exp = ifelse(Grid=="AG"&Year>2004&Year<2018, "f", Exp),
          Exp = ifelse(Grid=="JO"&Year>2006&Year<2013, "f", Exp),
          Exp = ifelse(Grid=="LL"&Year>2005&Year<2012, "f", Exp)) %>% 
  mutate (EXP_label = 1) %>% 
  mutate (EXP_label = ifelse(Exp=="f", 19, EXP_label))


#############################
# Means calculated per Year #
#############################


yearly_cones <- group_by(cone_counts, Year) %>% 
  summarize(num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE),
            cone_index = mean(cone_index, na.rm = TRUE),
            total_cones = mean(total_cones, na.rm = TRUE))

# Must add in data for 1989 because there were no cones but the zeros were not entered
yearly_cones <- rbind(yearly_cones,
                            list(1989L, 0, 0, 0, 0.005525156))

yearly_cones <- yearly_cones %>% 
  mutate(cone_index_t = ifelse(is.finite(cone_index), cone_index, NA)) %>% 
  mutate(Year_tp1 = Year+1)

yearly_cone_temp<-yearly_cones %>% 
  select(Year, Year_tp1, cone_index_tm1=cone_index_t)

yearly_cones<-left_join(yearly_cones, yearly_cone_temp, by=c("Year" = "Year_tp1")) %>% 
  select(Year, num_trees, cone_counts, cone_index_t, cone_index_tm1, total_cones) %>% 
  mutate(mast=ifelse(Year %in% c(1993, 1998, 2005, 2010, 2014, 2019, 2022), "y", "n"))


############################################
# Means calculated per Year KL and SU only #
############################################

cone_counts_klsu<-tbl(con, "cones") %>%
  filter(Grid %in% c("SU", "KL"), Year>=1988, !is.na(NumNew)) %>%
  collect() %>%
  mutate(Year = as.numeric(Year),
         NumNew = as.numeric(NumNew)) %>% 
  mutate(total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01)))

yearly_cones_klsu <- group_by(cone_counts_klsu, Year) %>% 
  summarize(num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE),
            cone_index = mean(log(NumNew + 1), na.rm = TRUE),
            total_cones = mean(total_cones, na.rm = TRUE))

yearly_cones_klsu <- yearly_cones_klsu %>% 
  mutate(cone_index_t = ifelse(is.finite(cone_index), cone_index, NA)) %>% 
  mutate(Year_tp1 = Year+1)

yearly_klsu_cone_temp<-yearly_cones_klsu %>% 
  select(Year, Year_tp1, cone_index_tm1=cone_index_t)

yearly_cones_klsu<-left_join(yearly_cones_klsu, yearly_klsu_cone_temp, by=c("Year" = "Year_tp1")) %>% 
  select(Year, num_trees, cone_counts, cone_index_t, cone_index_tm1, total_cones)%>% 
  mutate(mast=ifelse(Year %in% c(1993, 1998, 2005, 2010, 2014, 2019, 2022), "y", "n"))



rm(yearly_klsu_cone_temp, yearly_cone_temp, cone_temp)
