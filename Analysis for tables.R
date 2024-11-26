library(readxl)
data1 <- read_excel("All journals database-140.xlsx")
data2 <- read_excel("non-top journals-140.xlsx")
options(scipen=200)
options(digits=3) 
###table2/Descrptive analysis###
library('compareGroups'); library('haven')
data1$Region <- factor(data1$Region, levels = c("Northern America", "Western Europe", "Other regions"))
data1$ClinicalFocus <- factor(data1$ClinicalFocus, levels = c("General", "Speciality"))
data1$Database <- factor(data1$Database, levels = c("Top journals", "Whole journals"))
data1$A_Category <- factor(data1$A_Category, levels = c("0", "1", "2", "3"),labels = c("No policy","External policy","Own policy","Own & External policy"), ordered = T)
data1$R_Category <- factor(data1$R_Category, levels = c("0", "1", "2", "3"),labels = c("No policy","External policy","Own policy","Own & External policy"), ordered = T)
data1$External <- factor(data1$External, levels = c("0", "1"))
data1$IF <- as.numeric(data1$IF)
data1$CiteScore <- as.numeric(data1$CiteScore)
data1$Eigenefactor <- as.numeric(data1$Eigenefactor)
table2 <- descrTable(Database ~ SJR+IF+CiteScore+Eigenefactor+Region+ClinicalFocus+A_Category+R_Category+External, data = data1, simplify=F, method = c(SJR=2, IF =2, CiteScore=2, Eigenefactor=2))
print(table2)

###Table3###
AuthoruseY <- data1[data1$A_I1 == "Y", ]
AuthoruseY$A_specificity <- as.numeric(AuthoruseY$A_specificity)
AuthoruseY$A_I2 <- factor(AuthoruseY$A_I2, levels = c("0", "1"))
AuthoruseY$A_I3 <- factor(AuthoruseY$A_I3, levels = c("0", "1"))
AuthoruseY$A_I4 <- factor(AuthoruseY$A_I4, levels = c("0", "1"))
AuthoruseY$A_I5 <- factor(AuthoruseY$A_I5, levels = c("0", "1"))
AuthoruseY$A_I6 <- factor(AuthoruseY$A_I6, levels = c("0", "1"))
AuthoruseY$A_I7 <- factor(AuthoruseY$A_I7, levels = c("0", "1"))
AuthoruseY$A_I8 <- factor(AuthoruseY$A_I8, levels = c("0", "1"))
table3a <- descrTable(Database ~ A_I2+A_I3+A_I4+A_I5+A_I6+A_I7+A_I8+A_specificity, data = AuthoruseY, simplify=F)
print(table3a)
RevieweruseY <- data1[data1$R_I1 == "Y", ]
RevieweruseY$R_specificity <- as.numeric(RevieweruseY$R_specificity)
RevieweruseY$R_I2 <- factor(RevieweruseY$R_I2, levels = c("0", "1"))
RevieweruseY$R_I3 <- factor(RevieweruseY$R_I3, levels = c("0", "1"))
table3b <- descrTable(Database ~ R_I2+R_I3+R_specificity, data = RevieweruseY, simplify=F)
print(table3b)
RevieweruseYN <- data1[data1$R_I1 == "Y" | data1$R_I1 =="N",]
table3b1 <- descrTable(Database ~ R_I1, data = RevieweruseYN, simplify=F)
print(table3b1)
External <- data1[data1$External == "1", ]
table3c <- descrTable(Database ~ COPE+ICMJE+WAME, data = External, simplify=F)
print(table3c)

#####Top journals####
###Table5/multinomial logistic regression###
Topjournals <- data1[data1$Database == "Top journals", ]
library(nnet);library(car)
fit_Top_A_1 <- multinom(A_Category ~ SJR, data = Topjournals)
cof_Top_A_1<- broom::tidy(fit_Top_A_1)
confint(fit_Top_A_1)
fit_Top_A_2 <- multinom(A_Category ~ Region, data = Topjournals)
cof_Top_A_2<- broom::tidy(fit_Top_A_2)
confint(fit_Top_A_2)
fit_Top_A_3 <- multinom(A_Category ~ ClinicalFocus, data = Topjournals)
cof_Top_A_3<- broom::tidy(fit_Top_A_3)
confint(fit_Top_A_3)
CofTopA <- rbind(cof_Top_A_1,cof_Top_A_2,cof_Top_A_3)

fit_Top_A_4 <- multinom(A_Category ~ IF, data = Topjournals)
cof_Top_A_4<- broom::tidy(fit_Top_A_4)
confint(fit_Top_A_4)
fit_Top_A_5 <- multinom(A_Category ~ CiteScore, data = Topjournals)
cof_Top_A_5<- broom::tidy(fit_Top_A_5)
confint(fit_Top_A_5)
fit_Top_A_6 <- multinom(A_Category ~ Eigenefactor, data = Topjournals)
cof_Top_A_6<- broom::tidy(fit_Top_A_6)
confint(fit_Top_A_6)
CofTopA2 <- rbind(cof_Top_A_4,cof_Top_A_5,cof_Top_A_6)
write.csv(CofTopA2, "CofTopA2.csv")

fit_Top_R_1 <- multinom(R_Category ~ SJR, data = Topjournals)
cof_Top_R_1<- broom::tidy(fit_Top_R_1)
confint(fit_Top_R_1)
fit_Top_R_2 <- multinom(R_Category ~ Region, data = Topjournals)
cof_Top_R_2<- broom::tidy(fit_Top_R_2)
confint(fit_Top_R_2)
fit_Top_R_3 <- multinom(R_Category ~ ClinicalFocus, data = Topjournals)
cof_Top_R_3<- broom::tidy(fit_Top_R_3)
confint(fit_Top_R_3)
CofTopR <- rbind(cof_Top_R_1,cof_Top_R_2,cof_Top_R_3)
fit_Top_R_4 <- multinom(R_Category ~ IF, data = Topjournals)
cof_Top_R_4<- broom::tidy(fit_Top_R_4)
confint(fit_Top_R_4)
fit_Top_R_5 <- multinom(R_Category ~ CiteScore, data = Topjournals)
cof_Top_R_5<- broom::tidy(fit_Top_R_5)
confint(fit_Top_R_5)
fit_Top_R_6 <- multinom(R_Category ~ Eigenefactor, data = Topjournals)
cof_Top_R_6<- broom::tidy(fit_Top_R_6)
confint(fit_Top_R_6)
CofTopR2 <- rbind(cof_Top_R_4,cof_Top_R_5,cof_Top_R_6)
write.csv(CofTopR2, "CofTopR2.csv")


###SuppTable7/Liner regression###
TopAuthoruseY <- Topjournals[Topjournals$A_I1 == "Y", ]
TopRevieweruseY <- Topjournals[Topjournals$R_I1 == "Y", ]
TopAuthorSpecificity_fit1 <- lm(A_specificity ~ SJR, data = TopAuthoruseY)
summary(TopAuthorSpecificity_fit1)
(ci <- confint(TopAuthorSpecificity_fit1))
TopAuthorSpecificity_fit2 <- lm(A_specificity ~ Region, data = TopAuthoruseY)
summary(TopAuthorSpecificity_fit2)
(ci <- confint(TopAuthorSpecificity_fit2))
TopAuthorSpecificity_fit3 <- lm(A_specificity ~ ClinicalFocus, data = TopAuthoruseY)
summary(TopAuthorSpecificity_fit3)
(ci <- confint(TopAuthorSpecificity_fit3))
TopAuthorSpecificity_fit4 <- lm(A_specificity ~ IF, data = TopAuthoruseY)
summary(TopAuthorSpecificity_fit4)
(ci <- confint(TopAuthorSpecificity_fit4))
TopAuthorSpecificity_fit5 <- lm(A_specificity ~ Eigenefactor, data = TopAuthoruseY)
summary(TopAuthorSpecificity_fit5)
(ci <- confint(TopAuthorSpecificity_fit5))
TopAuthorSpecificity_fit6 <- lm(A_specificity ~ CiteScore, data = TopAuthoruseY)
summary(TopAuthorSpecificity_fit6)
(ci <- confint(TopAuthorSpecificity_fit6))

TopReviewerSpecificity_fit1 <- lm(R_specificity ~ SJR, data = TopRevieweruseY)
summary(TopReviewerSpecificity_fit1)
(ci <- confint(TopReviewerSpecificity_fit1))
TopReviewerSpecificity_fit2 <- lm(R_specificity ~ Region, data = TopRevieweruseY)
summary(TopReviewerSpecificity_fit2)
(ci <- confint(TopReviewerSpecificity_fit2))
TopReviewerSpecificity_fit3 <- lm(R_specificity ~ ClinicalFocus, data = TopRevieweruseY)
summary(TopReviewerSpecificity_fit3)
(ci <- confint(TopReviewerSpecificity_fit3))
TopReviewerSpecificity_fit4 <- lm(R_specificity ~ IF, data = TopRevieweruseY)
summary(TopReviewerSpecificity_fit4)
(ci <- confint(TopReviewerSpecificity_fit4))
TopReviewerSpecificity_fit5 <- lm(R_specificity ~ Eigenefactor, data = TopRevieweruseY)
summary(TopReviewerSpecificity_fit5)
(ci <- confint(TopReviewerSpecificity_fit5))
TopReviewerSpecificity_fit6 <- lm(R_specificity ~ CiteScore, data = TopRevieweruseY)
summary(TopReviewerSpecificity_fit6)
(ci <- confint(TopReviewerSpecificity_fit6))

#####non-top journals####
library(tidyverse)
calculate_weighted_freq <- function(df, var_name) {
  df %>%
    group_by({{var_name}}) %>%
    summarise(weighted_count = sum(Weight140),
              weighted_percent = (sum(Weight140) / sum(df$Weight140)) * 100) %>%
    ungroup()
}
###
nontopAuthoruseY <- data2[data2$A_I1 == "Y", ]
nontopRevieweruseY <- data2[data2$R_I1 == "Y", ]
nontopexternalY <- data2[data2$External == "1", ]
# compute “nontop journals” weighted frequency and percentage
weighted_nontop_Region <- calculate_weighted_freq(data2, Region)
weighted_nontop_ClinicalFocus <- calculate_weighted_freq(data2, ClinicalFocus)
weighted_nontop_Acategory <- calculate_weighted_freq(data2, A_Category)
weighted_nontop_Rcategory <- calculate_weighted_freq(data2, R_Category)

calculate_weighted_freq(nontopAuthoruseY, A_I4)
calculate_weighted_freq(nontopRevieweruseY, R_I3)
nontopRevieweruse <- data2[data2$R_I1 == "Y"|data2$R_I1 =="N", ]
calculate_weighted_freq(nontopRevieweruse, R_I1)

nontopexternalY$COPE <- as.factor(nontopexternalY$COPE, levels=c("Y","N"))
calculate_weighted_freq(nontopexternalY, WAME)

###weighted Chi-square test###
library(weights)
wtd.chi.sq(data1$ClinicalFocus, data1$Database, weight=data1$Weight140)
Allauthoruse <- data1[data1$A_I1 == "Y"|data1$A_I1 =="N", ]
wtd.chi.sq(Allauthoruse$A_I8, Allauthoruse$Database, weight=Allauthoruse$Weight140)
AllexternalY <- data1[data1$External == "1", ]
wtd.chi.sq(AllexternalY$COPE, AllexternalY$Database, weight=AllexternalY$Weight140)
###Table5/multinomial logistic regression###
library(nnet);library(car)
fit_nontop_A_1 <- multinom(A_Category ~ SJR, data = data2, weights = data2$Weight140)
cof_nontop_A_1<- broom::tidy(fit_nontop_A_1)
confint(fit_nontop_A_1)
fit_nontop_A_2 <- multinom(A_Category ~ Region, data = data2, weights = data2$Weight140)
cof_nontop_A_2<- broom::tidy(fit_nontop_A_2)
confint(fit_nontop_A_2)
fit_nontop_A_3 <- multinom(A_Category ~ ClinicalFocus, data = data2, weights = data2$Weight140)
cof_nontop_A_3<- broom::tidy(fit_nontop_A_3)
confint(fit_nontop_A_3)

fit_nontop_R_1 <- multinom(R_Category ~ SJR, data = data2, weights = data2$Weight140)
cof_nontop_R_1<- broom::tidy(fit_nontop_R_1)
confint(fit_nontop_R_1)
fit_nontop_R_2 <- multinom(R_Category ~ Region, data = data2, weights = data2$Weight140)
cof_nontop_R_2<- broom::tidy(fit_nontop_R_2)
confint(fit_nontop_R_2)
fit_nontop_R_3 <- multinom(R_Category ~ ClinicalFocus, data = data2, weights = data2$Weight140)
cof_nontop_R_3<- broom::tidy(fit_nontop_R_3)
confint(fit_nontop_R_3)

###SuppTable7/Liner regression###
nontopAuthorSpecificity_fit1 <- lm(A_specificity ~ SJR, data = nontopAuthoruseY, weights = Weight140)
summary(nontopAuthorSpecificity_fit1)
(ci <- confint(nontopAuthorSpecificity_fit1))
nontopAuthorSpecificity_fit2 <- lm(A_specificity ~ Region, data = nontopAuthoruseY, weights = Weight140)
summary(nontopAuthorSpecificity_fit2)
(ci <- confint(nontopAuthorSpecificity_fit2))
nontopAuthorSpecificity_fit3 <- lm(A_specificity ~ ClinicalFocus, data = nontopAuthoruseY, weights = Weight140)
summary(nontopAuthorSpecificity_fit3)
(ci <- confint(nontopAuthorSpecificity_fit3))


nontopReviewerSpecificity_fit1 <- lm(R_specificity ~ SJR, data = nontopRevieweruseY, weights = Weight140)
summary(nontopReviewerSpecificity_fit1)
(ci <- confint(nontopReviewerSpecificity_fit1))
nontopReviewerSpecificity_fit2 <- lm(R_specificity ~ Region, data = nontopRevieweruseY, weights = Weight140)
summary(nontopReviewerSpecificity_fit2)
(ci <- confint(nontopReviewerSpecificity_fit2))
nontopReviewerSpecificity_fit3 <- lm(R_specificity ~ ClinicalFocus, data = nontopRevieweruseY, weights = Weight140)
summary(nontopReviewerSpecificity_fit3)
(ci <- confint(nontopReviewerSpecificity_fit3))


##table6 multinomial logistic regression (single regression)##
library(nnet);library(car)
fit_single_A_1 <- multinom(A_Category ~ SJR, data = data2, weights = data2$Weight140)
cof_single_A_1<- broom::tidy(fit_single_A_1)
confint(fit_single_A_1)
fit_single_A_2 <- multinom(A_Category ~ Region, data = data2, weights = data2$Weight140)
cof_single_A_2<- broom::tidy(fit_single_A_2)
confint(fit_single_A_2)
fit_single_A_3 <- multinom(A_Category ~ ClinicalFocus, data = data2, weights = data2$Weight140)
cof_single_A_3<- broom::tidy(fit_single_A_3)
confint(fit_single_A_3)

fit_single_R_1 <- multinom(R_Category ~ SJR, data = data2, weights = data2$Weight140)
cof_single_R_1<- broom::tidy(fit_single_R_1)
confint(fit_single_R_1)
fit_single_R_2 <- multinom(R_Category ~ Region, data = data2, weights = data2$Weight140)
cof_single_R_2<- broom::tidy(fit_single_R_2)
confint(fit_single_R_2)
fit_single_R_3 <- multinom(R_Category ~ ClinicalFocus, data = data2, weights = data2$Weight140)
cof_single_R_3<- broom::tidy(fit_single_R_3)
confint(fit_single_R_3)

##supp table8 linear regression##
singleAuthoruseY <- data2[data2$A_I1 == "Y", ]
singleRevieweruseY <- data2[data2$R_I1 == "Y", ]
singleAuthorSpecificity_fit1 <- lm(A_specificity ~ SJR, data = singleAuthoruseY, weights = Weight140)
summary(singleAuthorSpecificity_fit1)
(ci <- confint(singleAuthorSpecificity_fit1))
singleAuthorSpecificity_fit2 <- lm(A_specificity ~ Region, data = singleAuthoruseY, weights = Weight140)
summary(singleAuthorSpecificity_fit2)
(ci <- confint(singleAuthorSpecificity_fit2))
singleAuthorSpecificity_fit3 <- lm(A_specificity ~ ClinicalFocus, data = singleAuthoruseY, weights = Weight140)
summary(singleAuthorSpecificity_fit3)
(ci <- confint(singleAuthorSpecificity_fit3))


singleReviewerSpecificity_fit1 <- lm(R_specificity ~ SJR, data = singleRevieweruseY, weights = Weight140)
summary(singleReviewerSpecificity_fit1)
(ci <- confint(singleReviewerSpecificity_fit1))
singleReviewerSpecificity_fit2 <- lm(R_specificity ~ Region, data = singleRevieweruseY, weights = Weight140)
summary(singleReviewerSpecificity_fit2)
(ci <- confint(singleReviewerSpecificity_fit2))
singleReviewerSpecificity_fit3 <- lm(R_specificity ~ ClinicalFocus, data = singleRevieweruseY, weights = Weight140)
summary(singleReviewerSpecificity_fit3)
(ci <- confint(singleReviewerSpecificity_fit3))