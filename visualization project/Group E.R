#clean the environment
rm(list=ls(all=TRUE))
cat("\014") 

#load the library
library(here)
library(tidyr)
library(ggplot2)
library(corrplot)
library(gplots)
library(tidyverse)
library(maps)



#load the data
salary <- read.csv('data_cleaned_2021.csv')
#summary(salary)
us_states <- map_data("state")

#drop the "useless" columns
salary <- salary[,-c(1,4,7,15:17,25:26,29:39,41:42)]
#str(salary)
attach(salary)

#data cleaning
#drop na value
salary=salary[salary$job_title_sim!='na',] 

salary=salary[salary$Industry!='-1',] 


#Categorize states by region 

salary$region <- NA
salary <- salary %>%
  mutate(region = case_when(
    Job.Location %in% 'ME' ~  'Northeast',
    Job.Location %in% 'NH' ~  'Northeast',
    Job.Location %in% 'VT' ~ 'Northeast',
    Job.Location %in% 'MA' ~ 'Northeast',
    Job.Location %in% 'RI' ~  'Northeast',
    Job.Location %in% 'CT' ~ 'Northeast',
    Job.Location %in% 'NY' ~ 'Northeast',
    Job.Location %in% 'PA' ~  'Northeast',
    Job.Location %in% 'NJ' ~ 'Northeast',
    Job.Location %in% 'DC' ~ 'Northeast',
    Job.Location %in% 'WI' ~ 'Midwest',
    Job.Location %in% 'MI' ~ 'Midwest',
    Job.Location %in% 'IL' ~ 'Midwest',
    Job.Location %in% 'IN' ~ 'Midwest',
    Job.Location %in% 'OH' ~ 'Midwest',
    Job.Location %in% 'MO' ~ 'Midwest',
    Job.Location %in% 'ND' ~ 'Midwest',
    Job.Location %in% 'SD' ~ 'Midwest',
    Job.Location %in% 'NE' ~ 'Midwest',
    Job.Location %in% 'KS' ~ 'Midwest',
    Job.Location %in% 'MN' ~ 'Midwest',
    Job.Location %in% 'IA' ~ 'Midwest',
    Job.Location %in% 'DE' ~ 'South',
    Job.Location %in% 'MD' ~ 'South',
    Job.Location %in% 'VA' ~ 'South',
    Job.Location %in% 'WV' ~ 'South',
    Job.Location %in% 'NC' ~ 'South',
    Job.Location %in% 'SC' ~ 'South',
    Job.Location %in% 'GA' ~ 'South',
    Job.Location %in% 'FL' ~ 'South',
    Job.Location %in% 'KY' ~ 'South',
    Job.Location %in% 'TN' ~ 'South',
    Job.Location %in% 'MS' ~ 'South',
    Job.Location %in% 'AL' ~ 'South',
    Job.Location %in% 'NK' ~ 'South',
    Job.Location %in% 'TX' ~ 'South',
    Job.Location %in% 'AR' ~ 'South',
    Job.Location %in% 'LA' ~ 'South',
    Job.Location %in% 'ID' ~ 'West',
    Job.Location %in% 'MT' ~ 'West',
    Job.Location %in% 'WY' ~ 'West',
    Job.Location %in% 'NV' ~ 'West',
    Job.Location %in% 'UT' ~ 'West',
    Job.Location %in% 'CO' ~ 'West',
    Job.Location %in% 'AZ' ~ 'West',
    Job.Location %in% 'NM' ~ 'West',
    Job.Location %in% 'AK' ~ 'West',
    Job.Location %in% 'WA' ~ 'West',
    Job.Location %in% 'OR' ~ 'West',
    Job.Location %in% 'CA' ~ 'West',
    Job.Location %in% 'HI' ~ 'West',
    
    TRUE ~ 'na'))

#check the region
table(salary$region)
salary[salary$region=='na',]

#-----------------------------------------------------------------------------------#
####################################DATA VISULIZATION################################
#-----------------------------------------------------------------------------------#

#* How are data scientists spread out in the United States geographically (by region)?

jobregioncount <- salary %>% count(region, sort = TRUE)
p <- ggplot(jobregioncount)+
  geom_col(aes(x=reorder(region,-n),y=n,fill=region))+
  theme(legend.position = 'none')+
  labs(x='Region',y='Job Count',title='Number of Data Scientist Jobs Posted on Glassdoor in 2021')+
  scale_fill_manual(values = c('tomato3','cornflowerblue','darkgoldenrod1','chartreuse4'))
p


#-----------------------------------------------------------------------------------#
#* What is the variation of salary according to industry
library(tidyverse)

industry = salary %>%
  group_by(Industry) %>%
  summarize(value = mean(Avg.Salary.K.))
#drop Other retails
industry=industry[industry$Industry!='Other Retail Stores',]
#select top10
industrytop8=industry[industry$value>118,]
industrybot10=industry[industry$value<78,]

industrytop8 %>%
  ggplot(aes(x = fct_reorder(Industry, -value), y = value,fill=Industry)) +
  geom_col()+
  theme(legend.position = 'none')+
  geom_text(label=paste((round(industrytop8$value))))+
  labs(x='Industry',y='Average Salary',title='Top 8 Average Salary in Different Industry')+
  scale_fill_brewer(palette="Spectral")

#-----------------------------------------------------------------------------------#
#* job title and job salary
#I use job_title_sim as job title,cuz job_title_sim has 10,job title has 248,too many
unique(salary$job_title_sim)
unique(salary$Job.Title)


ggplot(salary, aes(x=reorder(job_title_sim,-Avg.Salary.K.,FUN = mean),y=Avg.Salary.K.))+
  geom_boxplot(aes(fill=job_title_sim))+
  labs(x='Job Title(Simple)',y='Average Salary',title='Job Title and Avgerage Salary')+
  theme(legend.position = 'none')+
  scale_fill_manual(values = c('tomato3','tan3','yellow3','springgreen3','steelblue3','slategray3','violetred3','thistle3','wheat3','snow3'))


#-----------------------------------------------------------------------------------#
#* The relationship between state and avg salary using map
joblocation = salary %>%
  group_by(Job.Location) %>%
  summarize(value = mean(Avg.Salary.K.))

joblocation <- joblocation %>%
  mutate(region = case_when(
    Job.Location %in% 'AL' ~  'alabama',
    Job.Location %in% 'AK' ~  'alaska',
    Job.Location %in% 'AZ' ~ 'arizona',
    Job.Location %in% 'AR' ~ 'arkansas',
    Job.Location %in% 'CA' ~  'california',
    Job.Location %in% 'CO' ~ 'colorado',
    Job.Location %in% 'CT' ~ 'connecticut',
    Job.Location %in% 'DE' ~  'delaware',
    Job.Location %in% 'FL' ~ 'florida',
    Job.Location %in% 'GA' ~ 'georgia',
    Job.Location %in% 'HI' ~ 'hawaii',
    Job.Location %in% 'ID' ~ 'idaho',
    Job.Location %in% 'IL' ~ 'illinois',
    Job.Location %in% 'IN' ~ 'indiana',
    Job.Location %in% 'IA' ~ 'iowa',
    Job.Location %in% 'KS' ~ 'kansas',
    Job.Location %in% 'KY' ~ 'kentucky',
    Job.Location %in% 'LA' ~ 'louisiana',
    Job.Location %in% 'ME' ~ 'maine',
    Job.Location %in% 'MD' ~ 'maryland',
    Job.Location %in% 'MA' ~ 'massachusetts',
    Job.Location %in% 'MI' ~ 'michigan',
    Job.Location %in% 'MN' ~ 'minnesota',
    Job.Location %in% 'MS' ~ 'mississippi	',
    Job.Location %in% 'MO' ~ 'missouri',
    Job.Location %in% 'MT' ~ 'montana',
    Job.Location %in% 'NE' ~ 'nebraska',
    Job.Location %in% 'NV' ~ 'nevada',
    Job.Location %in% 'NH' ~ 'new hampshire',
    Job.Location %in% 'NJ' ~ 'new jersey',
    Job.Location %in% 'NM' ~ 'new mexico',
    Job.Location %in% 'NY' ~ 'new york',
    Job.Location %in% 'NC' ~ 'north carolina',
    Job.Location %in% 'ND' ~ 'north dakota',
    Job.Location %in% 'OH' ~ 'ohio',
    Job.Location %in% 'OK' ~ 'oklahoma',
    Job.Location %in% 'OR' ~ 'oregon',
    Job.Location %in% 'PA' ~ 'pennsylvania',
    Job.Location %in% 'RI' ~ 'rhode island',
    Job.Location %in% 'SC' ~ 'south carolina',
    Job.Location %in% 'SD' ~ 'south dakota',
    Job.Location %in% 'TN' ~ 'tennessee',
    Job.Location %in% 'TX' ~ 'texas',
    Job.Location %in% 'UT' ~ 'utah',
    Job.Location %in% 'VT' ~ 'vermont',
    Job.Location %in% 'VA' ~ 'virginia',
    Job.Location %in% 'WA' ~ 'washington',
    Job.Location %in% 'WV' ~ 'west virginia',
    Job.Location %in% 'WI' ~ 'wisconsin',
    Job.Location %in% 'WY' ~ 'wyoming',
    Job.Location %in% 'DC' ~ 'DC',
    
    TRUE ~ 'na'))

#check the region
table(salary$region)
salary[salary$region=='na',]


joblocation <- joblocation %>%
  mutate(category = case_when(
    value<100 ~ 'Below Average',
    value>=100 ~ 'Above Average',
    TRUE ~ 'na'))
table = data.frame(
  Job.Location=c('AK','AR','HI','ME','MS','MT','NV','ND','OK','SD','VT','WY','WV','NH'),
  value=c('na'),
  region = c('alaska','arkansas','hawaii','maine','mississippi','montana','nevada',
             'north dakota','oklahoma','south dakota','vermont','wyoming','west virginia','new hampshire'),
  category = c('To Be Announced')
)

library(plyr)
u=rbind.fill(joblocation,table)

county_full <- left_join(us_states,u, by = "region")
county_full <- county_full[,-6]
county_full <- na.omit(county_full)
p <- ggplot(data = county_full,
            aes(x = long, y = lat,
                group = group, fill = category))

p + geom_polygon(color = "gray90", linewidth = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  labs(title='Salary Level in Different State')+
  scale_fill_manual(values = c('tomato3','cornflowerblue','cornsilk'))




