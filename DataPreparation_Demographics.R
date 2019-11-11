dat = read.csv("P300_peak_new2.csv", header = TRUE)

head(dat)
dat = dat[,2:38] # exclude the identity of patients
head(dat)
dim(dat)

# standardization
std_dat = dat
std_dat[,2:37] = scale(dat[,2:37])
head(std_dat)

# ds1: remove 23rd obs
dat_new = std_dat[-23,]

# ds2: raw obs
# ds3: w/o standardization

library(dplyr)
filter(dat_new, SuiHx == 0) # 35
filter(dat_new, SuiHx == 1) # 21

head(dat)

demo = read.csv("Demographics.final.csv", header = TRUE)
demo = demo[1:57,c(1:4,6)]
View(demo)
head(demo)

# male
male = filter(demo, demo$gender == "1")  # 26
male.yes = filter(male, male$SuiHx == "1")  # 8
male.no = filter(male, male$SuiHx == "0")  # 18
# female
female = filter(demo, demo$gender == "2") # 31
female.yes = filter(female, female$SuiHx == "1") # 14
female.no = filter(female, female$SuiHx == "0") # 17
twenty = filter(demo, demo$age < 30) # 12
thirty = filter(demo, demo$age < 40 & demo$age >= 30) # 11
forty = filter(demo, demo$age < 50 & demo$age >= 40) # 16
fifty = filter(demo, demo$age < 60 & demo$age >= 50) # 15
sixty = filter(demo, demo$age >= 60)  # 3

demo$age_group = ifelse(demo$age < 30, '1',{ifelse(demo$age < 40 & demo$age >= 30, '2', 
                                                   {ifelse(demo$age < 50 & demo$age >= 40, '3', {
                                                     ifelse(demo$age < 60 & demo$age >= 50, '4', '5')
                                                   })})})
                        
                        #ifelse(demo$age < 40 & demo$age >= 30, '2', 
                        #ifelse(demo$age < 50 & demo$age >= 40, '3', 
                        #(ifelse(demo$age < 60 & demo$age >= 50, '4', '5'))

# combine gender column to ERP data
demo[,3] = as.factor(demo[,3])
demo[,4] = as.factor(demo[,4])
chisq.test(x = demo[,3], y = demo[,4])
class(demo$SuiHx)

# for t-test
yes = filter(demo, demo$SuiHx == "1")
no = filter(demo, demo$SuiHx == "0")
t.test(x = as.numeric(yes$age), y = as.numeric(no$age))
t.test(x = as.numeric(yes$EDU_YRS), y = as.numeric(no$EDU_YRS), alternative = c("two.sided"))

# age: mean and stdev
(sa.age = mean(yes$age))
(nsa.age = mean(no$age))
(sa.age2 = sd(yes$age))
(nsa.age2 = sd(no$age))
(sa.edu = mean(yes$EDU_YRS))
(nsa.edu = mean(no$EDU_YRS))
(sa.edu2 = sd(yes$EDU_YRS))
(nsa.edu2 = sd(no$EDU_YRS))
no.edu2 = na.rm(no.edu2)
na.rm = no$EDU_YRS[-c(4,33)]
sd(na.rm)
