x<-c("tm", "ggplot2", "wordcloud", "RColorBrewer", "RTextTools", "plyr", "class", "stringi")
lapply(x, require, character.only = TRUE)

#setwd("/Users/jaesohn/Google Drive/Research/Project Sohn NLP Protocol/") #Mac Pro
#setwd("/Users/bashta/Google Drive/Research/Project Sohn NLP Protocol/") #Mac Air
setwd("C:/Users/Jae Ho Sohn/Google Drive/Research/Project Sohn NLP Protocol") #Desktop

#ab<-read.csv("NLPprot_MSK_train.csv")
ab<-read.csv("NLPprot_abd_full.csv")

#colnames(d) <- c("indication","contrast")
#d$indication<-as.String(d$indication) #this causes error
colnames(ab) <- c("indication","contrast")
ab$contrast<-as.factor(ab$contrast)
d<-ab

###################################
d_corpus <- Corpus(VectorSource(d$indication))
d_corpus <- tm_map(d_corpus, PlainTextDocument)
d_corpus <- tm_map(d_corpus, removePunctuation)
d_corpus <- tm_map(d_corpus, tolower)
d_corpus <- tm_map(d_corpus, stripWhitespace)

d_corpus <- tm_map(d_corpus, removeWords, 
                   c('reason', 'with','this','and','eval','for','mri','has','please','spine'))
d_corpus <- tm_map(d_corpus, PlainTextDocument)

pal2 <- brewer.pal(8,"Dark2")

#png("Figure 1 msk.png", width=12, height=8, units="in", res=300)
wordcloud(d_corpus, max.words = 100, random.order = FALSE, colors = pal2)
#dev.off()



###################################

List <- strsplit(ab$indication, " ")
dt<-data.frame(contrast=rep(ab$contrast, sapply(List, length)), indication=unlist(List))

acorpus <- Corpus(VectorSource(dt$indication))
acorpus <- tm_map(acorpus, PlainTextDocument)
acorpus <- tm_map(acorpus, removePunctuation)
acorpus <- tm_map(acorpus, tolower)
acorpus <- tm_map(acorpus, stripWhitespace)

acorpus <- tm_map(acorpus, removeWords, 
                   c('reason', 'with','this','and','eval','for','mri','has','please','spine'))
acorpus <- tm_map(acorpus, PlainTextDocument)

pal2 <- brewer.pal(8,"Dark2")

#png("Figure 1.png", width=12, height=8, units="in", res=300)
#wordcloud(acorpus, max.words = 100, random.order = FALSE, colors = pal2)
#dev.off()

dtm <- DocumentTermMatrix(acorpus)

findAssocs(dtm, "appendicitis", corlimit=0.5) # specifying a correlation limit of 0.98   



m <- as.matrix(dtm)
d <- dist(m)

rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)), 
                     substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))

hc <- hclust(d,method="ward.D")
hcd <- as.dendrogram(hc)

plot(hcd, main="Main")
plot(cut(hcd, h=5)$upper, 
     main="Upper tree of cut at h=75")
plot(cut(hcd, h=75)$lower[[2]], 
     main="Second branch of lower tree with cut at h=75")

plot(groups, hang=-1)



######
tdm <- DocumentTermMatrix(acorpus)
# Transform dtm to matrix to data frame - df is easier to work with
mat.df <- as.data.frame(data.matrix(tdm), stringsAsfactors = FALSE)
mat.df <- cbind(mat.df, ab$contrast)
colnames(mat.df)[ncol(mat.df)] <- "contrast"

train <- sample(nrow(tdm),ceiling(nrow(tdm)*0.75))
test <- (1:nrow(tdm))[-train]
cl <- mat.df[ncol(mat.df)]

knn.pred <- knn(tdm[train,],tdm[test,],cl[train,])

conf.mat <- table("Predictions" = knn.pred, Actual = cl[test,])
heatmap(t(conf.mat)[ncol(conf.mat):1,], Rowv=NA, Colv=NA, col = heat.colors(10))
accuracy <- sum(diag(conf.mat))/length(test) * 100




######Wiki Classification
docsTDM <- TermDocumentMatrix(acorpus)

docsdissim <- dist(as.matrix(docsTDM), method = "cosine")

docsdissim2 <- as.matrix(docsdissim)

rownames(docsdissim2) <- titles
colnames(docsdissim2) <- titles
docsdissim
h <- hclust(docsdissim, method = "ward.D2")
plot(h, labels = titles, sub = "")



##############
dtMatrix <- create_matrix(ab["indication"])
container <- create_container(dtMatrix, as.numeric(factor(ab$contrast)), 
                              trainSize=1:200, testSize = 201:251, virgin=FALSE)

svm <- train_model(container,"SVM")
svm_c <- classify_model(container, svm)
svm_v <- cross_validate(container, 4, "SVM")
summary(svm_c)
svm_a  <- create_analytics(container, svm_c)
svm_a@ensemble_summary
y<-svm_a@document_summary

maxent <- train_model(container,"MAXENT")
maxent_c <- classify_model(container, maxent)
maxent_v <- cross_validate(container, 4, "MAXENT")
summary(maxent_c)
maxent_a  <- create_analytics(container, cbind(svm_c, maxent_c))
maxent_a@ensemble_summary
y<-maxent_a@document_summary

#every <- train_models(container,algorithms=c("SVM","MAXENT","TREE","BOOSTING","BAGGING", "GLMNET","SLDA","RF"))
every <- train_models(container,algorithms=c("SVM","MAXENT","TREE","BOOSTING","BAGGING", "SLDA","RF")) #for multiple classification
results<-classify_models(container,every)
y<-create_analytics(container,results)

#every_v <- cross_validate(container, 4, algorithms=c("SVM","MAXENT","TREE","BOOSTING","BAGGING", "GLMNET","SLDA","RF"))
every_v <- cross_validate(container, 4, algorithms=c("SVM","MAXENT","TREE","BOOSTING","BAGGING", "SLDA","RF")) #for multiple classification

TREE <- train_model(container,"TREE")
NNET <- train_model(container,"NNET")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")
RF <- train_model(container,"RF")


TREE_CLASSIFY <- classify_model(container,TREE)
NNET_CLASSIFY <- classify_model(container, NNET)
BOOSTING_CLASSIFY <- classify_model(container,BOOSTING)
BAGGING_CLASSIFY <- classify_model(container,BAGGING)
GLMNET_CLASSIFY <- classify_model(container,GLMNET)
MAXENT_CLASSIFY <- classify_model(container,MAXENT)
SLDA_CLASSIFY <- classify_model(container,SLDA)
RF_CLASSIFY <- classify_model(container,RF)


summary(TREE_CLASSIFY$TREE_LABEL)
summary(NNET_CLASSIFY$NNET_LABEL)
summary(BOOSTING_CLASSIFY$LOGITBOOST_LABEL)
summary(BAGGING_CLASSIFY$BAGGING_LABEL)
summary(GLMNET_CLASSIFY$GLMNET_LABEL)
summary(MAXENT_CLASSIFY$MAXENTROPY_LABEL) ##Winner
summary(RF_CLASSIFY$FORESTS_LABEL) 

summary(ab$contrast[1001:1239])

x<-ab$contrast[1001:1239]
y<-RF_CLASSIFY$FORESTS_LABEL
summary(x==y)


# create a prediction document term matrix
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), virgin=FALSE)

results <- classify_model(predictionContainer, model)


#################
tdm<-DocumentTermMatrix(acorpus)
txt_mat<- as.textmatrix(as.matrix(tdm))

lsa_model <- lsa(txt_mat)

dim(lsa_model$tk) #Terms x New LSA Space
dim(lsa_model$dk) #Documents x New LSA Space
length(lsa_model$sk) #Singular Values

hc = hclust(dist(d$contrast))
plot(hc)

################
