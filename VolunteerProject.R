library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(tm)
library(wordcloud)
library(fpc)
library(NbClust)
library(stringr)
library(cluster)
library(zipcode)
library(Matrix)
library(xgboost)
library(ROSE)
library(mfx)
library(Amelia)


# **************************************
# Load Data
# **************************************

case = read.csv("/Users/royhu/Desktop/Cal Poly/Decision/Salesforce Data/Salesforce Data/Case.csv",header=T, na.strings=c("","NA"))
contact = read.csv("/Users/royhu/Desktop/Cal Poly/Decision/Salesforce Data/Salesforce Data/Contact.csv",header=T,stringsAsFactors=FALSE)


# **************************************
# Case Data
# **************************************

#Drop Variables that Have Near Zero Variance

temp=case
feature.names <- names(temp)
for (f in feature.names) {
  if (class(temp[[f]])=="character" || class(temp[[f]])=="factor") {
    temp[[f]] <- as.integer(factor(temp[[f]]))
  }
}

#nzv <- nearZeroVar(temp, saveMetrics= TRUE)
#nzv[nzv$nzv,][1:10,]
#table(nzv$nzv)

dropnzv <- nearZeroVar(temp)
filteredCase <- case[,-dropnzv]


#Get rid of exact time
filteredCase$CreatedDate=gsub( " .*$", "", filteredCase$CreatedDate)
filteredCase$LastModifiedDate=gsub( " .*$", "", filteredCase$LastModifiedDate)

#Get frequency count
caseFreq = data.frame(table(filteredCase$ContactId))
filteredCase <- filteredCase %>%
  dplyr::left_join(.,caseFreq,by=c("ContactId" = "Var1"))


#Clean up and separate. Creates a new lag feature 
filteredCase <- filteredCase %>%
  separate(CreatedDate, into = c("dac_month", "dac_day","dac_year"), sep = "/", remove=FALSE) %>%
  separate(LastModifiedDate, into = c("lmod_month", "lmod_day","lmod_year"), sep = "/", remove=FALSE) %>%
  dplyr::mutate(
    lag = as.Date(filteredCase$LastModifiedDate, format="%m/%d/%Y") - as.Date(filteredCase$CreatedDate, format="%m/%d/%Y")
  )


# **************************************
# Contact Data
# **************************************

#Drop Near-Zero Variance Features

temp=contact
feature.names <- names(temp)
for (f in feature.names) {
  if (class(temp[[f]])=="character" || class(temp[[f]])=="factor") {
    temp[[f]] <- as.integer(factor(temp[[f]]))
  }
}


nzv <- nearZeroVar(temp, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
table(nzv$nzv)

dropnzv <- nearZeroVar(temp)
filteredContacts <- contact[,-dropnzv]

filteredContacts[filteredContacts==""] = NA


# **************************************
# Feature Engineering
# **************************************

#Merge Zip Code, MSA Data
data(zipcode)

zipcodes1 = read.csv("/Users/royhu/Desktop/Cal Poly/Decision/fs10_gpci_by_ZIP.csv",header=T,stringsAsFactors=FALSE)
zipcodes1 = zipcodes1[,(1:2)]
names(zipcodes1)=c("zip","MSA")
zipcodes1$zip=as.character(zipcodes1$zip)

merged <- filteredContacts %>%
  dplyr::left_join(.,zipcodes1,by=c("MailingPostalCode" = "zip")) %>%
  left_join(.,zipcode,by=c("MailingPostalCode" = "zip"))
##


#Merge Volunteers in Case with and Contact Data 
filteredCase$Volunteer=ifelse(filteredCase$Freq>0,1,0)
uniqueCase = subset(filteredCase, !duplicated(ContactId))
uniqueCase = uniqueCase[,c(3,27)]

uniqueCase$ContactId=as.character(uniqueCase$ContactId)
merged2 <- merged %>%
  dplyr::left_join(.,uniqueCase,by=c("Id" = "ContactId"))

merged2[merged2==""] = NA


#Adjust Desired Salary
for(i in 1:64549){
  if(merged2$Compensation_Desired__c[i]>10000&&!is.na(merged2$Compensation_Desired__c[i])){
    merged2$Compensation_Desired__c[i]=merged2$Compensation_Desired__c[i]/2000
  }
  if(merged2$Compensation_Desired__c[i]>1000&&merged2$Compensation_Desired__c[i]<10000&&!is.na(merged2$Compensation_Desired__c[i])){
    merged2$Compensation_Desired__c[i]=merged2$Compensation_Desired__c[i]/200
  }
  if(merged2$Compensation_Desired__c[i]>100&&merged2$Compensation_Desired__c[i]<1000&&!is.na(merged2$Compensation_Desired__c[i])){
    merged2$Compensation_Desired__c[i]=merged2$Compensation_Desired__c[i]/20
  }
}
unique(merged2$Compensation_Desired__c)

#Adjust Current Salary
for(i in 1:64549){
  if(merged2$Current_Salary__c[i]>10000&&!is.na(merged2$Current_Salary__c[i])){
    merged2$Current_Salary__c[i]=merged2$Current_Salary__c[i]/2000
  }
  if(merged2$Current_Salary__c[i]>1000&&merged2$Current_Salary__c[i]<10000&&!is.na(merged2$Current_Salary__c[i])){
    merged2$Current_Salary__c[i]=merged2$Current_Salary__c[i]/200
  }
  if(merged2$Current_Salary__c[i]>100&&merged2$Current_Salary__c[i]<1000&&!is.na(merged2$Current_Salary__c[i])){
    merged2$Current_Salary__c[i]=merged2$Current_Salary__c[i]/20
  }
}
unique(merged2$Current_Salary__c)

##Compare Volunteer Service (contact) to Volunteers -- some new ones

A=merged2
for(i in 1:64549){
  if(merged2$Volunteer_Services__c[i]=="None"|is.na(merged2$Volunteer_Services__c[i]))
    A$Volunteer2[i]=0
  else A$Volunteer2[i]=1
}

merged2$Volunteer[which(A$Volunteer2>A$Volunteer)]=1



#Earliest Available 
merged2$Earliest_Employment_Start_Date__c[grepl("asap",merged2$Earliest_Employment_Start_Date__c,ignore.case=TRUE)]<-1
merged2$Earliest_Employment_Start_Date__c[grepl("noti",merged2$Earliest_Employment_Start_Date__c,ignore.case=TRUE)]<-1
merged2$Earliest_Employment_Start_Date__c[grepl("medi",merged2$Earliest_Employment_Start_Date__c,ignore.case=TRUE)]<-1
merged2$Earliest_Employment_Start_Date__c[grepl("soon",merged2$Earliest_Employment_Start_Date__c,ignore.case=TRUE)]<-1
merged2$Earliest_Employment_Start_Date__c[grepl("poss",merged2$Earliest_Employment_Start_Date__c,ignore.case=TRUE)]<-1
merged2$Earliest_Employment_Start_Date__c[grepl("week",merged2$Earliest_Employment_Start_Date__c,ignore.case=TRUE)]<-1
merged2$Earliest_Employment_Start_Date__c[grepl("now",merged2$Earliest_Employment_Start_Date__c,ignore.case=TRUE)]<-1
merged2$Earliest_Employment_Start_Date__c[grepl("Avail",merged2$Earliest_Employment_Start_Date__c,ignore.case=TRUE)]<-1

#Clean up and separate important dates. Creates a new lag feature for month, year. 
merged2$SystemModstamp=gsub( " .*$", "", merged2$SystemModstamp)
merged2$CreatedDate=gsub( " .*$", "", merged2$CreatedDate)
merged2$LastModifiedDate=gsub( " .*$", "", merged2$LastModifiedDate)
merged2$Date_of_Service_EntryNew__c=gsub( " .*$", "", merged2$Date_of_Service_EntryNew__c)

merged2 <- merged2 %>%
  separate(SystemModstamp, into = c("sysMonth", "sysDay","sysYear"), sep = "/", remove=TRUE) %>%
  separate(CreatedDate, into = c("createMonth", "createDay","createYear"), sep = "/", remove=TRUE) %>%
  separate(LastModifiedDate, into = c("lastmodMonth", "lastmodDay","lastmodYear"), sep = "/", remove=TRUE) %>%
  separate(Date_of_Service_EntryNew__c, into = c("serveMonth", "serveDay","serveYear"), sep = "/", remove=TRUE)


# **************************************
# Cluster Areas of Experience Data
# **************************************


merged2$Areas_of_Experience__c<-iconv(enc2utf8(merged2$Areas_of_Experience__c))


cols.corpus = Corpus(VectorSource(merged2$Areas_of_Experience__c))
inspect(cols.corpus)
cols.corpus = tm_map(cols.corpus,tolower)
cols.corpus = tm_map(cols.corpus,removeNumbers)
cols.corpus <- tm_map(cols.corpus, PlainTextDocument)
cols.corpus<- tm_map(cols.corpus,removePunctuation)
cols.corpus <- tm_map(cols.corpus, removeWords,stopwords("english"))
cols.corpus <- tm_map(cols.corpus, stripWhitespace)

#Finds Word Frequencies
dtm <- TermDocumentMatrix(cols.corpus,control = list(minWordLength = 1))

#Most common words from Qualifications (minimum 5000 occurrences per word)

freq.terms = findFreqTerms(dtm,lowfreq=5000)
term.freq = rowSums(as.matrix(dtm))
term.freq = subset(term.freq,term.freq>5000)
df=data.frame(term=names(term.freq),freq=term.freq)

ggplot(df,aes(x=term,y=freq))+geom_bar(stat = "identity")+xlab("Terms")+ylab("Count")+coord_flip()

#hist of all words? gotta remove the NAs though 

m = as.matrix(dtm)
word.freq = sort(rowSums(m),decreasing=T)

#wordcloud(words=names(word.freq),freq=word.freq,random.order=F)

#Make 6 clusters
dtm2=removeSparseTerms(dtm,sparse=0.95)
m2=as.matrix(dtm2)
distMatrix = dist(scale(m2))
fit = hclust(distMatrix,method="ward.D")
plot(fit)

rect.hclust(fit,k=6)

m3=t(m2)
set.seed(1)
k=6
kmeansResult = kmeans(m3,k)
round(kmeansResult$centers,digits=3)

#Replace Areas of Expertise 
test=as.data.frame(kmeansResult$cluster)
test[test==3] = NA
merged2=cbind(merged2,test)
merged2$Areas_of_Experience__c = NULL


for(i in 1:k){
  cat(paste("cluster ",i, ": ",sep=""))
  s = sort(kmeansResult$center[i,],decreasing=T)
  cat(names(s)[1:5],"\n")
}

#Used Elbow Rule to determine optimal clusters (6)
mydata <- m3
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

names(merged2)[names(merged2) == 'kmeansResult$cluster'] = "AreaOfExpertiseCluster"



# ***************************************
# Remove Redundant & Irrelevant Features
# ***************************************

merged2$createDay = NULL
merged2$lastmodDay = NULL
merged2$sysDay = NULL
merged2$AccountId = NULL
merged2$RecordTypeId = NULL
merged2$MailingCity= NULL
merged2$MailingState= NULL
merged2$MailingPostalCode= NULL
merged2$OwnerId = NULL
merged2$Level_of_Disability__c = NULL
merged2$Is_disability_a_result_of_Combat__c = NULL
merged2$Fri_Talkin_Baseball_Number_of_Guests__c = NULL
merged2$Sat_Awards_Banquet_Number_of_Guests__c = NULL
merged2$Sun_Picnic_Number_of_Guests__c = NULL
merged2$Security_Clearance_Type__c = NULL
merged2$If_Security_Clearance_Yes_What_kind__c = NULL
merged2$Desired_City_of_Employment__c = NULL
merged2$Id.y = NULL
merged2$CaseNumber = NULL
merged2$CreatedDate = NULL
merged2$CreatedById = NULL
merged2$LastModifiedDate = NULL
merged2$KeywordsSelected__c = NULL

merged2$SystemModstamp = NULL
merged2$Primary_Military_Occupational_Specialty__c = NULL

merged2$Highest_Level_of_Education_Completed__c = NULL
merged2$Specific_Industries_Jobs__c = NULL
merged2$Education_Summary__c = NULL
merged2$Summary_of_Qualifications__c = NULL
merged2$Areas_of_Experience__c = NULL
merged2$Primary_Military_Occupational_Specialty__c = NULL
merged2$geopointe__Geocode__c = NULL
merged2$Subject = NULL
merged2$OwnerId = NULL
merged2$CreatedDate = NULL
merged2$CreatedById = NULL
merged2$ClosedDate = NULL
merged2$LastModifiedDate = NULL
merged2$LastModifiedById = NULL
merged2$Date_of_Original_Request__c = NULL

#Hiring Data
merged2$Hire_Heroes_USA_Confirmed_Hire__c = NULL
merged2$Hired_but_still_active_and_looking__c = NULL
merged2$Active_Color__c = NULL
merged2$Submitted_for_Hire__c = NULL

merged2$dac_day= NULL
merged2$lmod_day=NULL
merged2$serveDay= NULL
merged2$Date_of_Separation__c =NULL
merged2$Date_of_Service_Entry__c=NULL
merged2$Special_Event__c=NULL
merged2$ts2__Business_Unit__c=NULL
merged2$ts2__Notice__c=NULL
merged2$ts2__People_Status__c=NULL
merged2$ts2__Source__c=NULL
merged2$HHUSA_Veteran_Classification__c = NULL
merged2$ts2__Language_Read_2__c = NULL
merged2$ts2__Language_Write_1__c = NULL
merged2$ts2__Language_Write_2__c = NULL
merged2$Military_Occupation__c = NULL
merged2$Volunteer_Services__c=NULL
merged2$city=NULL

#Remove after XGBoost: Not important features
merged2$LeadSource=NULL
merged2$Active__c=NULL
merged2$Send_Green_Survey__c=NULL
merged2$ts2__Contact_Method__c=NULL
merged2$Office_Manager_Approved__c=NULL
merged2$Office_Manager_Approved__c=NULL
merged2$DD214__c=NULL
merged2$Last_Rank__c = NULL
merged2$Service_Rank__c = NULL


merged2[sapply(merged2, is.character)] <- lapply(merged2[sapply(merged2, is.character)], 
                                                 as.factor)


# **************************************
# Cole's Part
# **************************************

merged2$education = educa$X2
merged2$education[merged2$education == 0] <- NA

merged2$officer = RANK_feat$OFFICER
merged2$enlisted = RANK_feat$ENLISTED
merged2$rank = RANK_feat$r3 

# **************************************
# Propensity Score  Weight
# **************************************

#Generate temp to feed into XGBoost. Needs to be all numeric. 
temp=merged2
feature.names <- names(temp)

for (f in feature.names) {
  if (class(temp[[f]])=="character" || class(temp[[f]])=="factor") {
    temp[[f]] <- as.integer(factor(temp[[f]]))
  }
}

temp$Id=NULL

for(i in 1:64549){
  if(is.na(temp$Volunteer[i]))
    temp$Volunteer[i]=0
}


missmap(temp)


#Split Train and Test set for cross-validation

## 70% of the sample size
smp_size <- floor(0.70* nrow(temp))

## set the seed to make your partition reproductible
#set.seed(123)
train_ind <- sample(seq_len(nrow(temp)), size = smp_size)

train <- temp[train_ind, ]
test <- temp[-train_ind, ]


#XGBoost 

y <- train$Volunteer
train$Volunteer=NULL

xgtrain = xgb.DMatrix(data= data.matrix(train), label = y, missing=NA)
xgtest = xgb.DMatrix(data=data.matrix(test), missing=NA)

#CV purpose is to return optimal number of iterations given a choice of parameters
docv <- function(param0, iter) {
  model_cv = xgb.cv(
    params = param0
    , nrounds = iter
    , nfold = 2
    , data = xgtrain
    , early.stop.round = 10
    , maximize = FALSE
    , nthread = 8
  )
  gc()
  best <- min(model_cv$test.logloss.mean)
  bestIter <- which(model_cv$test.logloss.mean==best)
  
  cat("\n",best, bestIter,"\n")
  print(model_cv[bestIter])
  
  bestIter-1
}

doTest <- function(param0, iter) {
  watchlist <- list('train' = xgtrain)
  model = xgb.train(
    nrounds = iter
    , params = param0
    , data = xgtrain
    , watchlist = watchlist
    , print.every.n = 20
    , nthread = 8
  )
  p <- predict(model, xgtest)
  rm(model)
  gc()
  p
}

param0 <- list(
  # some generic, non specific params
  "objective"  = "binary:logistic"
  , "eval_metric" = "logloss"
  , "eta" = 0.07
  , "subsample" = 0.9
  , "colsample_bytree" = 0.9
  , "min_child_weight" = 1
  , "max_depth" = 10
)

#Get optimal CV Value
cv <- docv(param0, 500)

#Ensemble of 10XGB Models to Evaluate Performance
ensemble <- rep(0, nrow(test))

for (i in 1:10) {
  print(i)
  set.seed(i + 2017)
  p <- doTest(param0, cv) 
  # use 40% to 50% more than the best iter rounds from your cross-fold number.
  # as you have another 50% training data now, which gives longer optimal training time
  ensemble <- ensemble + p
}



PredictedProb <- ensemble/10

predicted = ifelse(PredictedProb>.15,1,0)


tp=length((which(A$Volunteer==1&A$predicted==1)))
fp = length((which(A$Volunteer==0&A$predicted==1)))

tn = length((which(A$Volunteer==0&A$predicted==0)))
fn = length((which(A$Volunteer==1&A$predicted==0)))


accuracy.meas(test$Volunteer,PredictedProb,threshold=.13)
roc.curve(test$Volunteer,PredictedProb,plotit=T)



#Use CV Value to build predict probabilities on full data set

y <- temp$Volunteer
temp$Volunteer=NULL
xgtotal = xgb.DMatrix(data= data.matrix(temp), label = y, missing=NA)


watchlist <- list('total' = xgtotal)
model2 = xgb.train(
  nrounds = 81
  , params = param0
  , data = xgtotal
  , watchlist = watchlist
  , print.every.n = 20
  , nthread = 8
)

A=predict(model2,xgtotal)
B = as.data.frame(cbind(y,A))
names(B)=c("Volunteer","Prob")
B$weight.ATE = ifelse(B$Volunteer==1,1/B$Prob,1/(1-B$Prob))



#Plot Feature Importance Chart
feature.names = names(temp)
importance <- xgb.importance(feature_names=feature.names,model = model2)

xgb.plot.importance(importance_matrix = importance[1:30])



# **************************************
# Causal Inference
# **************************************

#Create Hired through 
hired = filteredContacts %>% dplyr::select(Hired_but_still_active_and_looking__c,Hire_Heroes_USA_Confirmed_Hire__c)
hired$Hired_but_still_active_and_looking__c=ifelse(hired$Hired_but_still_active_and_looking__c=="No",0,1)
hired = hired %>% mutate(hiredTotal = Hired_but_still_active_and_looking__c+Hire_Heroes_USA_Confirmed_Hire__c)
hired$hiredTotal=ifelse(hired$hiredTotal>0,1,0)

regdata = as.data.frame(cbind(hired$hiredTotal,B$Volunteer,B$weight.ATE))
names(regdata) = c("hired","Volunteer","propWeight")

volunteer.ATE = glm(hired~Volunteer,data=regdata,family="binomial",weights=(propWeight))
summary(volunteer.ATE)

volunteer.mfx=logitmfx(volunteer.ATE,regdata)

volunteer.mfx


# **************************************
# Misc. 
# **************************************


#Pie Chart of the Hiring Color Code

contact$Active_Color__c[which(contact$Active_Color__c=="Gray")]="Grey"

colors = data.frame(table(contact$Active_Color__c))
colors=colors[-4,]
colors$percentage=colors$Freq/64549
colnames(colors)[1]="HHCode"

bp<- ggplot(colors, aes(x="", y=percentage, fill=HHCode))+ 
  geom_bar(width = 1, stat = "identity") + scale_fill_manual(values=c("#FF00FF05", "#000000", "#0072B2", "#009E73", 
                                                                      "#999999", "#551a8b", "#FF0000"))
bp

pie <- bp + coord_polar("y", start=0)
pie
pie + scale_fill_manual(values=c("#FF00FF05", "#000000", "#0072B2", "#009E73", 
                                 "#999999", "#551a8b", "#FF0000"))

