# Load libraries
library(mallet)
library(coreNLP)

OUTDIR = "../img/ch10/"
dir.create(OUTDIR, FALSE, TRUE)
options(width=70)

#########################
# TF-IDF
wikiFiles = dir("../data/ch10/wiki_annotations/", full.names=TRUE)
wikiNames = gsub("\\.Rds", "", basename(wikiFiles))

lemmas = c()
for (f in wikiFiles) {
  anno = readRDS(f)
  token = getToken(anno)
  theseLemma = token$lemma[token$POS %in% c("NNS","NN")]
  lemmas = append(lemmas, theseLemma)
}
lemmas = names(sort(table(lemmas),decreasing=TRUE)[1:50])
lemmas

tf = matrix(0,nrow=length(wikiFiles),ncol=length(lemmas))
colnames(tf) = lemmas
rownames(tf) = substr(wikiNames,nchar(wikiNames)-10,nchar(wikiNames))

for (j in 1:length(wikiFiles)) {
  anno = readRDS(wikiFiles[j])
  token = getToken(anno)
  theseLemma = token$lemma[token$POS %in% c("NNS","NN")]
  theseLemma = theseLemma[theseLemma %in% lemmas]
  tab = table(theseLemma)
  index = match(lemmas,names(tab))
  tf[j,!is.na(index)] = tab[index[!is.na(index)]]
}

tf[1:6,1:6]

lemmas[apply(tf,1,which.max)][1:10]

df = apply(tf!=0,2,sum)
df[1:20]

df = matrix(rep(df,length(wikiFiles)),ncol=length(lemmas),
            byrow=TRUE) / length(wikiFiles)

impScore = round(tf * log(1/df),3)
impScore[1:6,1:8]

sort(impScore["Machiavelli",],decreasing=TRUE)[1:6]
sort(impScore["Jean_Piaget",],decreasing=TRUE)[1:6]
sort(impScore["oam_Chomsky",],decreasing=TRUE)[1:6]
sort(impScore["ohn_Paul_II",],decreasing=TRUE)[1:6]


#########################
# Topic Models

dateSet = rep(0L,length(wikiFiles))
for (j in 1:length(wikiFiles)) {
  anno = readRDS(wikiFiles[j])
  tx = getToken(anno)$Timex
  tx = substr(tx[!is.na(tx)],1,4)
  tx = as.numeric(tx)
  tx = tx[!is.na(tx)]
  dateSet[j] = tx[1]
}

wikiFiles = wikiFiles[order(dateSet)]
wikiNames = wikiNames[order(dateSet)]

shortNames = sapply(strsplit(wikiNames, "_"), function(v) rev(v)[1])
shortNames[115] = "Godel"
shortNames[135] = "John Paul II"
shortNames[156] = "Newman"
shortNames[174] = "Mao"

bagOfWords = rep("",length(wikiFiles))
for (j in 1:length(wikiFiles)) {
  anno = readRDS(wikiFiles[j])
  token = getToken(anno)
  theseLemma = token$lemma[token$POS %in% c("NNS","NN")]
  bagOfWords[j] = paste(theseLemma,collapse=" ")
}

tf = tempfile()
writeLines(c(letters,LETTERS),tf)

instance = mallet.import(wikiNames, bagOfWords, tf)

tm = MalletLDA(9)
tm$loadDocuments(instance)
tm$setAlphaOptimization(30,50)
tm$train(200)
tm$maximize(10)

topics = mallet.doc.topics(tm, smoothed=TRUE, normalized=TRUE)
words = mallet.topic.words(tm, smoothed=TRUE, normalized=TRUE)
vocab = tm$getVocabulary()

#tmSaved = list(topics=topics,words=words,vocab=vocab)
#saveRDS(tmSaved, "~/Desktop/tm.Rds")

tm = readRDS("../data/ch10/tm.Rds")
topics = tm$topics
words = tm$words
vocab = tm$vocab

dim(topics)
dim(words)
length(vocab)

t(apply(words,1,function(v) vocab[order(v,decreasing=TRUE)[1:5]]))

topicNames = c("politics","biography","social-science","existentialism",
               "philosophy","logic","poetry","culture","language")

index = order(apply(words,2,max),decreasing=TRUE)[1:50]
set = unique(as.character(apply(words,1,function(v)
                vocab[order(v,decreasing=TRUE)[1:5]])))
index = match(set,vocab)

mat = round(t(words[,index]),3)
mat = mat / max(mat)

pdf(paste0(OUTDIR, "topicWordDistribution.pdf"), 6, 8.5)
par(mar=c(0,0,0,0))
plot(0,0,col="white",ylim=c(-1,nrow(mat)),xlim=c(-2,ncol(mat)))
for(i in 1:nrow(mat)) lines(x=c(1,ncol(mat)),y=c(i,i), col=grey(0.5,0.5))
for(i in 1:ncol(mat)) lines(x=c(i,i),y=c(1,nrow(mat)), col=grey(0.5,0.5))
points(col(mat), nrow(mat) - row(mat) + 1,
       pch=19,cex=mat*3,col=rainbow(ncol(mat),alpha=0.33)[col(mat)])
text(0.5, nrow(mat):1, vocab[index], adj=c(1,0.5),cex=0.7)
text(1:ncol(mat), -0.75, topicNames, adj=c(0.5,0),cex=0.7,srt=60)
dev.off()

colnames(topics) = topicNames

pdf(paste0(OUTDIR, "documentTopicDistro.pdf"), 6, 8.5)
par(mfrow=c(1,3))
par(mar=c(0,0,0,0))
for (index in list(1:60,61:120,121:179)) {
  mat = topics[index,]
  mat = mat / max(mat)

  plot(0,0,col="white",ylim=c(-1,nrow(mat)),xlim=c(-5,ncol(mat)),axes=FALSE)
  for(i in 1:nrow(mat)) lines(x=c(1,ncol(mat)),y=c(i,i), col=grey(0.5,0.5))
  for(i in 1:ncol(mat)) lines(x=c(i,i),y=c(1,nrow(mat)), col=grey(0.5,0.5))

  points(col(mat), nrow(mat) - row(mat) + 1,
         pch=19,cex=mat*3,col=rainbow(ncol(mat),alpha=0.33)[col(mat)])

  text(0.5, nrow(mat):1, shortNames[index], adj=c(1,0.5),cex=0.7)
  text(1:ncol(mat), 0.5, topicNames, adj=c(1,0),cex=0.7,srt=60)
}
dev.off()

pc = prcomp(topics,scale.=TRUE)
topicsScale = scale(topics,center=pc$center,scale=pc$scale)
wordScale = scale(diag(ncol(topics)),center=pc$center,scale=pc$scale)
topicPC = topicsScale %*% pc$rotation
wordPC = wordScale %*% pc$rotation

pdf(paste0(OUTDIR, "topicPCA.pdf"), 10, 5)
par(mfrow=c(1,2))
par(mar=c(0,0,0,0))
index = c(1,21,32,38,48,64,115,135,174,175)

plot(wordPC[,1],wordPC[,2],pch=19,xlim=c(-6,6.5),axes=FALSE,cex=5,col="white")
box()
points(topicPC[,1], topicPC[,2], pch=19, cex=0.3, col=grey(0.5,0.3))
points(wordPC[,1],wordPC[,2],pch=19,cex=5,col=rgb(0,0,0,0.3))
text(wordPC[,1], wordPC[,2],topicNames)
text(topicPC[index,1], topicPC[index,2],shortNames[index],cex=0.7,pos=4)

plot(wordPC[,1],wordPC[,3],pch=19,xlim=c(-7,7),axes=FALSE,cex=5,col="white")
box()
points(topicPC[,1], topicPC[,3], pch=19, cex=0.3, col=grey(0.5,0.3))
points(wordPC[,1],wordPC[,3],pch=19,cex=5,col=rgb(0,0,0,0.3))
text(wordPC[,1], wordPC[,3],topicNames)
text(topicPC[index,1], topicPC[index,3],shortNames[index],cex=0.7,pos=1)
dev.off()

temp = round(topics[index,],3)
rownames(temp) = shortNames[index]
colnames(temp) = topicNames
temp

#########################
# Stylometrics

gutenFiles = c("pg76.Rds", "pg74.Rds", "pg1837.Rds", "pg102.Rds", "pg7193.Rds",
               "pg98.Rds", "pg1400.Rds", "pg730.Rds", "pg766.Rds", "pg883.Rds",
                "pg19337.Rds", "pg653.Rds", "pg33.Rds", "pg13707.Rds", "pg77.Rds",
                "pg2081.Rds", "pg976.Rds", "pg9255.Rds", "pg513.Rds", "pg2852.Rds",
                "pg244.Rds", "pg2097.Rds", "pg3289.Rds", "pg139.Rds", "pg126.Rds",
                "pg7964.Rds")
gutenFiles = paste0("../data/ch10/gutenbergClean_annotations/", gutenFiles)

gutenNames = auth = c(rep("Mark Twain",5),rep("Charles Dickens",7),
        rep("Nathaniel Hawthorne",7),rep("Sir Arthur Conan Doyle",7))
cols = c(rep("#DB9D85",5),rep("#86B875",7),rep("#4CB9CC",7),
         rep("#CD99D8",7))

utPos = c(".", "CONJ", "NUM", "X", "DET", "ADP", "ADJ", "VERB", "NOUN",
          "PRT", "PRON", "ADV")
pos2gram = matrix(0L,nrow=length(gutenFiles),ncol=144L)
rownames(pos2gram) = gutenNames
colnames(pos2gram) = apply(expand.grid(utPos,utPos),1,paste,collapse="-")

for (j in 1:length(gutenFiles)) {
  anno = readRDS(gutenFiles[j])
  ut = universalTagset(getToken(anno)$POS)
  ut = paste(ut[-length(ut)],ut[-1],sep="-")
  tab = table(ut)
  index = match(colnames(pos2gram),names(tab))
  pos2gram[j,!is.na(index)] = tab[index[!is.na(index)]] / sum(tab[index[!is.na(index)]])
}

pc = prcomp(pos2gram)
pos2gramPC = scale(pos2gram, center=pc$center, scale=pc$scale) %*% pc$rotation

centroid = apply(pos2gramPC,2,function(v) tapply(v, gutenNames, mean))

pdf(paste0(OUTDIR, "stylometricAuthors.pdf"), 10, 5)
par(mfrow=c(1,2))
par(mar=c(0,0,0,0))
plot(pos2gramPC[,1],pos2gramPC[,2], col=cols, pch=19, axes=FALSE)
box()
text(centroid[,1], centroid[,2], rownames(centroid),
      col=c("#86B875", "#DB9D85", "#4CB9CC","#CD99D8"))
plot(pos2gramPC[,1],pos2gramPC[,3], col=cols, pch=19, axes=FALSE)
box()
text(centroid[,1], centroid[,3], rownames(centroid),
      col=c("#86B875", "#DB9D85", "#4CB9CC","#CD99D8"))
dev.off()

pc$rotation = round(pc$rotation,3)
head(pc$rotation[,1][order(abs(pc$rotation[,1]),decreasing=TRUE)])
head(pc$rotation[,2][order(abs(pc$rotation[,2]),decreasing=TRUE)])
head(pc$rotation[,3][order(abs(pc$rotation[,3]),decreasing=TRUE)])


lemma = auth = NULL
for (j in 1:length(gutenFiles)) {
  anno = readRDS(gutenFiles[j])
  temp = getToken(anno)$lemma
  temp = matrix(temp,nrow=5000)
  temp = temp[,-ncol(temp),drop=FALSE]
  lemma = cbind(lemma, temp)
  auth = c(auth, rep(gutenNames[j],ncol(temp)))
}

these = names(sort(table(lemma),decreasing=TRUE))[1:50]
these

mat = cbind(match(lemma, these),as.numeric(col(lemma)))
mat = mat[!is.na(mat[,1]),]
tab = table(mat[,2],mat[,1])

pc = prcomp(tab,scale.=FALSE)
tabScale = scale(tab,center=pc$center,scale=pc$scale)
tabPC = tab %*% pc$rotation

centroid = apply(tabPC,2,function(v) tapply(v, auth, mean))
cols = c(rep("#DB9D85",5),rep("#86B875",7),rep("#4CB9CC",7),
         rep("#CD99D8",7))[match(auth, gutenNames)]

pdf(paste0(OUTDIR, "stylometricAuthorsWords.pdf"), 10, 5)
par(mfrow=c(1,2))
par(mar=c(0,0,0,0))
plot(tabPC[,1], tabPC[,2], col=cols, pch=19, cex=0.5, axes=FALSE)
text(centroid[,1], centroid[,2], rownames(centroid),
      col=c("#86B875", "#DB9D85", "#4CB9CC","#CD99D8"))
box()
plot(tabPC[,1], tabPC[,3], col=cols, pch=19, cex=0.5, axes=FALSE)
text(centroid[,1], centroid[,3], rownames(centroid),
      col=c("#86B875", "#DB9D85", "#4CB9CC","#CD99D8"))
box()
dev.off()




