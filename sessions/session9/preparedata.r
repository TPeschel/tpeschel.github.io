## read in data

require(Hmisc)
demogr <- sasxport.get("DEMO_G.XPT")
bp <- sasxport.get("BPX_G.XPT")
physact <- sasxport.get("PAQ_G.XPT")
bm <- sasxport.get("BMX_G.XPT")

require(dplyr)

## BMDSTATS - Body Measures Component Status Code
## 1 	Complete data for age group 
## 2 	Partial: Only height and weight obtained
## 3 	Other partial exam 
## 4 	No body measures exam data

## BMXWT - Weight (kg)
## BMXRECUM - Recumbent Length (cm)
## BMXHT - Standing Height (cm)
## BMXBMI - Body Mass Index (kg/m**2)
## BMDBMIC - BMI Category - Children/Adolescents
## BMXARMC - Arm Circumference (cm)
## BMXWAIST - Waist Circumference (cm)
bm <- bm %>%
    select(seqn,bmdstats,bmxwt,bmxrecum,bmxht,bmxbmi,bmdbmic,bmxarmc,bmxwaist)

## PAQ635 - Walk or bicycle
## 1 	Yes 	2344 	2344 	
## 2 	No 	4434 	6778 	PAQ650
## 7 	Refused 	1 	6779 	PAQ650
## 9 	Don't know 	0 	6779 	PAQ650
## . 	Missing 	2328 	9107

## PAQ640 - Number of days walk or bicycle
## 1 to 7 	Range of Values 	2343 	2343 	
## 77 	Refused 	0 	2343 	PAQ650
## 99 	Don't know 	1 	2344 	PAQ650
## . 	Missing 	6763 	9107 	

## PAD645 -  Minutes walk/bicycle for transportation (per day)
physact <- physact %>% select(seqn,paq635,paq640,pad645)

## BPXCHR - 60 sec HR (30 sec HR * 2)
## BPXSY3 - Systolic: Blood pres (3rd rdg) mm Hg
## BPXDI3 - Diastolic: Blood pres (3rd rdg) mm Hg
bp <- bp %>% select(seqn,bpxchr,bpxsy3,bpxdi3)

## demogr
## RIAGENDR - Gender
## RIDAGEYR - Age in years at screening 
## RIDEXAGM - Age in months at exam - 0 to 19 years
demogr <- demogr %>% select(seqn,riagendr,ridageyr,ridexagm)

nhanes <- Reduce(merge,list(demogr,bm,bp,physact))
nhanes <- filter(nhanes,ridageyr > 18)

nhanes <- sample_n(nhanes,500)

nhanes$riagendr <- factor(nhanes$riagendr,labels=c("male","female"))

save(nhanes,file="nhanesdata.rdata")

require(XLConnect)
wb <- loadWorkbook("nhanes1112.xlsx",create = T)

createSheet(wb,"demographics")
writeWorksheet(wb,demogr,"demographics")

createSheet(wb,"bp")
writeWorksheet(wb,bp,"bp")

createSheet(wb,"physactivity")
writeWorksheet(wb,physact,"physactivity")

createSheet(wb,"bodymeas")
writeWorksheet(wb,bm,"bodymeas")

saveWorkbook(wb)
