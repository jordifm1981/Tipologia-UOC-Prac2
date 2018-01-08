# Carreguem l'arxiu de dades
tennistes <- read.table("C:/Users/Manel/Downloads/Tipologia/Practica2/ATP_jugadors_guanys_2017.csv", 
  header=TRUE, sep=",", encoding="UTF-8", na.strings="NA", dec=".", strip.white=TRUE)

# Observacions nul·les
sapply(tennistes, function(x)(sum(is.na(x)))) # NA counts
# Observacions amb elements a zero
sapply(tennistes, function(x)(sum(x==0))) # Zero counts
# Observacions amb elements en blanc
sapply(tennistes, function(x)(sum(x==""))) # Blancs
# Observacions amb elements en blanc
sapply(tennistes, function(x)(sum(x=="0-0"))) # Sense partits


# Gràfica outliers
boxplot(tennistes$GUANYS_IND, main="Boxplot Guanys", boxwex=0.4)

summary(tennistes)

# Càlcul outliers
IQR<-IQR(tennistes$GUANYS_IND)
tall.outlier<-1.5*IQR+68222
# Data frame de tennistes amb ingressos extrems
ten_outlier<-tennistes[tennistes$GUANYS_IND > tall.outlier,]
nrow(ten_outlier)

# Data frame de tennistes sense ingressos extrems
ten_normal<-tennistes[!(tennistes$JUGADOR %in% ten_outlier$JUGADOR),]
nrow(ten_normal)

summary(ten_outlier)
summary(ten_normal)

# Dividem l'atribut W-L en WIN i LOSE
class(ten_normal$WL)
a<-strsplit(as.character(ten_normal$WL),split='-')
b<-sapply(a, unlist)
ten_normal$WIN<-as.numeric(b[1,])
ten_normal$LOSE<-as.numeric(b[2,])
head(ten_normal)

#Repetim procés per dataframe tennistes i ten_outlier
a<-strsplit(as.character(tennistes$WL),split='-')
b<-sapply(a, unlist)
tennistes$WIN<-as.numeric(b[1,])
tennistes$LOSE<-as.numeric(b[2,])

a<-strsplit(as.character(ten_outlier$WL),split='-')
b<-sapply(a, unlist)
ten_outlier$WIN<-as.numeric(b[1,])
ten_outlier$LOSE<-as.numeric(b[2,])


# Creem grups ATP i NO-ATP i els guardem en un nou atribut ATP
ten_normal$ATP <- ifelse((ten_normal$WIN>0 | ten_normal$LOSE>0), "ATP", "NOATP")
ten_outlier$ATP <- ifelse((ten_outlier$WIN>0 | ten_outlier$LOSE>0), "ATP", "NOATP")
# Factoritzem
ten_normal$ATP<-factor(ten_normal$ATP)
ten_outlier$ATP<-factor(ten_outlier$ATP)
# Quantitat de cada grup
tapply(ten_normal$ATP, ten_normal$ATP, length)

# Guanys per grup ATP i NOATP
ten_norm_ATP_guanys<-ten_normal$GUANYS_IND[ten_normal$ATP=="ATP"]
ten_norm_NOATP_guanys<-ten_normal$GUANYS_IND[ten_normal$ATP=="NOATP"]

# Anàlisi de normalitat ATP
qqnorm(ten_norm_ATP_guanys, main="Anàlisi normalitat jugadors ATP") 
qqline(ten_norm_ATP_guanys)
# Anàlisi de normalitat NO ATP
qqnorm(ten_norm_NOATP_guanys, main="Anàlisi normalitat jugadors NO ATP") 
qqline(ten_norm_NOATP_guanys)

# Comprovem normalitat a través de gràfiques
par(mfrow=c(2,2))
hist(ten_normal$GUANYS_IND,freq=F, breaks = 100, main="Histograma Anàlisi Normalitat")
hist(ten_norm_NOATP_guanys,freq=F,breaks = 100, main="Histograma NO ATP")
hist(ten_norm_ATP_guanys,freq=F,breaks = 100, main="Histograma ATP")
hist(ten_normal$GUANYS_IND[400:600],freq=F,breaks = 100, main="Histograma Fragment dades")
# Afegim curva de distribució normal
xbar=mean(ten_normal$GUANYS_IND[400:600])
s=sd(ten_normal$GUANYS_IND[400:600])
curve(dnorm(x, mean=xbar,sd=s), 0, 30000, add=T, col="blue")

# Altres proves: Normalitat del fragment de guanys
frag<-ten_normal$GUANYS_IND[400:600]
dev.off()
qqnorm(frag) 
qqline(frag)

# Més proves
qqnorm(ten_normal$GUANYS_IND) 
qqline(ten_normal$GUANYS_IND)

qqnorm(ten_outlier$GUANYS_IND) 
qqline(ten_outlier$GUANYS_IND)

# Testos de normalitat numèrics
shapiro.test(ten_norm_ATP_guanys)
shapiro.test(ten_norm_NOATP_guanys)
normalityTest(GUANYS_IND ~ ATP, test="pearson.test", data=ten_normal)

# Comparem variances dels conjunta ATP i NO ATP a través de 3 testos diferents
# Bartlett test
bartlett.test(ten_normal$GUANYS_IND~ATP, data=ten_normal)
library(car)
# Levene test
leveneTest(ten_normal$GUANYS_IND~ATP, data=ten_normal)
# Test F de comparació de variances
with(ten_normal, tapply(GUANYS_IND, ATP,  var, na.rm=TRUE))
var.test(GUANYS_IND ~ ATP, alternative='two.sided', conf.level=.95, data=ten_normal)


# Normalitzem dades a través de la funció scale
ten_outlier$GUANYSNORM<-round(scale(ten_outlier$GUANYS_IND),3)
ten_normal$GUANYSNORM<-round(scale(ten_normal$GUANYS_IND),3)

# Histograma normalitzat
hist(ten_normal$GUANYSNORM,freq=F,breaks = 100, main="Histograma Normalitzat")
xbar=mean(ten_normal$GUANYSNORM)
s=sd(ten_normal$GUANYSNORM)
curve(dnorm(x, mean=xbar,sd=s), -1, 1, add=T, col="blue")

# Traiem les "T" de ranking
summary(ten_normal)
ten_normal$RANKING<-gsub("T", "", ten_normal$RANKING)
ten_outlier$RANKING<-gsub("T", "", ten_outlier$RANKING)
ten_normal$RANKING<-as.numeric(ten_normal$RANKING)
ten_outlier$RANKING<-as.numeric(ten_outlier$RANKING)
ten_outlier$ATP<-factor(ten_outlier$ATP)

summary(ten_normal[c(1,2,4,6,10,13,14,15)])


# Correlació tennistes normals
corGuanysN<-c(cor(ten_normal$RANKING,ten_normal$GUANYS_IND),cor(ten_normal$EDAT,ten_normal$GUANYS_IND),cor(ten_normal$TORNEJOS,ten_normal$GUANYS_IND), 
cor(ten_normal$WIN,ten_normal$GUANYS_IND),cor(ten_normal$LOSE,ten_normal$GUANYS_IND))
names(corGuanysN)=c("Rànking","Edat","Tornejos","WIN","LOSE")
corGuanysN

# Correlacions tennistes amb guanys extrems
corGuanysO<-c(cor(ten_outlier$RANKING,ten_outlier$GUANYS_IND),cor(ten_outlier$EDAT,ten_outlier$GUANYS_IND),cor(ten_outlier$TORNEJOS,ten_outlier$GUANYS_IND), 
cor(ten_outlier$WIN,ten_outlier$GUANYS_IND),cor(ten_outlier$LOSE,ten_outlier$GUANYS_IND),cor(ten_outlier$LOSE,ten_outlier$TITOLS))
names(corGuanysO)=c("Rànking","Edat","Tornejos","WIN","LOSE","TITOLS")
corGuanysO

# Definim funció estad.basic
estad.basic<-function(x){
	est<-cbind(mean(x),var(x),sd(x),t(quantile(x)))
	colnames(est)<-c("mitjana","var","desv.est","min","Q1","Q2","Q3","max")
	return(round(est,2))}

# mirem estadístiques per edat, rànquing i guanys
edat<-tapply(ten_normal$EDAT,ten_normal$ATP,estad.basic)
rank<-tapply(ten_normal$RANKING,ten_normal$ATP,estad.basic)
guanys<-tapply(ten_normal$GUANYS_IND,ten_normal$ATP,estad.basic)


# Gràfica de secció per guanys
dev.off()
gcalc<-tapply(ten_normal$GUANYS_IND,ten_normal$ATP,sum)
numten<-tapply(ten_normal$GUANYS_IND,ten_normal$ATP,length)
percentlabels<- round(100*gcalc/sum(gcalc, 2))
pielabels<- paste(percentlabels, "%", sep="")
par(mfrow=c(1,2))
pie(gcalc, main="Proporció de guanys per conjunt", col=c("green","yellow"), labels=pielabels, cex=0.8)
legend("topright", c("ATP","NO ATP"), cex=0.8, fill=c("green","yellow"))
percentlabels<- round(100*numten/sum(numten, 2))
pielabels<- paste(percentlabels, "%", sep="")
pie(numten, main="Proporció de tennistes per conjunt", col=c("green","yellow"), labels=pielabels, cex=0.8)
legend("topright", c("ATP","NO ATP"), cex=0.8, fill=c("green","yellow"))



# Fem la mateixa gràfica respecte les dades totals
tennistes$ATP <- ifelse((tennistes$WL!="0-0"), "ATP", "NOATP")
# Factoritzem
tennistes$ATP<-factor(tennistes$ATP)

gcalcTot<-tapply(tennistes$GUANYS_IND,tennistes$ATP,sum)
numtenTot<-tapply(tennistes$GUANYS_IND,tennistes$ATP,length)
dev.off()
percentlabels<- round(100*gcalcTot/sum(gcalcTot, 2))
pielabels<- paste(percentlabels, "%", sep="")
par(mfrow=c(1,2))
pie(gcalcTot, main="Proporció de guanys per conjunt", col=c("green","yellow"), labels=pielabels, cex=0.8)
legend("topright", c("ATP","NO ATP"), cex=0.8, fill=c("green","yellow"))
percentlabels<- round(100*numtenTot/sum(numtenTot, 2))
pielabels<- paste(percentlabels, "%", sep="")
pie(numtenTot, main="Proporció de tennistes per conjunt", col=c("green","yellow"), labels=pielabels, cex=0.8)
legend("topright", c("ATP","NO ATP"), cex=0.8, fill=c("green","yellow"))

# Discretitzem atribut GUANYS_IND

summary(with(ten_normal, binVariable(ten_normal$GUANYS_IND, bins=5, method='intervals', labels = c("BAIX", "MIG-BAIX","MIG", "MIG-ALT", "ALT"))))
ten_normal$GUANYSDISC<-with(ten_normal, binVariable(ten_normal$GUANYS_IND, bins=5, method='intervals', labels = c("BAIX", "MIG-BAIX","MIG", "MIG-ALT", "ALT")))

dev.off()
with(ten_normal, Barplot(GUANYSDISC, by=GUANYSDISC, style="divided", ylim=c(0,800),legend.pos="right", main="Distribució de freqüencia per guanys",xlab="Guanys per franges", 
  ylab="Freqüència"))

with(ten_normal, Barplot(ATP, by=GUANYSDISC, style="divided", ylim=c(0,800),legend.pos="right", main="Distribució de freqüencia per guanys",xlab="Guanys per franges", 
  ylab="Freqüència"))

# Càlcul de diverses dades de comparació entre els guanys dels grups ATP i NO ATP
dfATP<-ten_normal[ten_normal$ATP=="ATP",]
dfNOATP<-ten_normal[ten_normal$ATP=="NOATP",]
meanATP<-tapply(dfATP$GUANYS_IND,dfATP$GUANYSDISC,mean)
minATP<-tapply(dfATP$GUANYS_IND,dfATP$GUANYSDISC,min)
maxATP<-tapply(dfATP$GUANYS_IND,dfATP$GUANYSDISC,max)
meanNOATP<-tapply(dfNOATP$GUANYS_IND,dfNOATP$GUANYSDISC,mean)
minNOATP<-tapply(dfNOATP$GUANYS_IND,dfNOATP$GUANYSDISC,min)
maxNOATP<-tapply(dfNOATP$GUANYS_IND,dfNOATP$GUANYSDISC,max)

countATP<-tapply(dfATP$GUANYS_IND,dfATP$GUANYSDISC,length)
countNOATP<-tapply(dfNOATP$GUANYS_IND,dfNOATP$GUANYSDISC,length)
# Posem a zero valors nuls de comptar elements
countNOATP[is.na(countNOATP)] <- 0

totATP<-length(dfATP$GUANYS_IND)
totNOATP<-length(dfNOATP$GUANYS_IND)
# Posem a zero valors nuls de comptar elements
totNOATP[is.na(totNOATP)] <- 0
countRelATP<-round(100*(countATP/totATP),2)
countRelNOATP<-round(100*(countNOATP/totNOATP),2)
countRelATPTOT<-round(100*(countATP/(countATP+countNOATP)),2)
countRelNOATPTOT<-round(100*(countNOATP/(countATP+countNOATP)),2)

rbind(meanATP,minATP,maxATP,meanNOATP,minNOATP,maxNOATP,countATP,countRelATP,countNOATP,countRelNOATP, countRelATPTOT,countRelNOATPTOT)

# Gràfica comparativa de distribució per grups
gr<-rbind(countATP,countNOATP)
grBaix<-gr[,1]
grMBaix<-gr[,2]
grMig<-gr[,3]
grMigA<-gr[,4]
grAlt<-gr[,5]

par(mfrow=c(3,2))
percentlabels<- round(100*grBaix/(countATP[1]+countNOATP[1]), 2)
pielabels<- paste(percentlabels, "%", sep="")
pie(grBaix, main="Ingressos nivell Baix", col=c("green","yellow"), labels=pielabels, cex=0.8)
legend("topright", c("ATP","NO ATP"), cex=0.8, fill=c("green","yellow"))

percentlabels<- round(100*grMBaix/(countATP[2]+countNOATP[2]), 2)
pielabels<- paste(percentlabels, "%", sep="")
pie(grMBaix, main="Ingressos nivell Mig-Baix", col=c("green","yellow"), labels=pielabels, cex=0.8)
legend("topright", c("ATP","NO ATP"), cex=0.8, fill=c("green","yellow"))

percentlabels<- round(100*grMig/(countATP[3]+countNOATP[3]), 2)
pielabels<- paste(percentlabels, "%", sep="")
pie(grMig, main="Ingressos nivell Mig", col=c("green","yellow"), labels=pielabels, cex=0.8)
legend("topright", c("ATP","NO ATP"), cex=0.8, fill=c("green","yellow"))

percentlabels<- round(100*grMigA/(countATP[4]+countNOATP[4]), 2)
pielabels<- paste(percentlabels, "%", sep="")
pie(grMigA, main="Ingressos nivell Mig-Alt", col=c("green","yellow"), labels=pielabels, cex=0.8)
legend("topright", c("ATP","NO ATP"), cex=0.8, fill=c("green","yellow"))

percentlabels<- round(100*grAlt/(countATP[5]+countNOATP[5]), 2)
pielabels<- paste(percentlabels, "%", sep="")
pie(grAlt, main="Ingressos nivell Alt", col=c("green","yellow"), labels=pielabels, cex=0.8)
legend("topright", c("ATP","NO ATP"), cex=0.8, fill=c("green","yellow"))

# Informació del rang de guanys que ens interessa
rankMigBaix<-ten_normal$RANKING[ten_normal$GUANYSDISC=="MIG-BAIX"]
summary(rankMigBaix)
sort(rankMigBaix)

dev.off()
# Boxplot comparatiu de grup ATO i NO ATP
Boxplot(GUANYS_IND~ATP, data=ten_normal, id.method="none", 
  main="Boxplot per grups ATP i NO ATP", ylab="Guanys")


# Escollim atributs pel conjunt de dades resultant
ten_normal_result<-ten_normal[c(1,2,4,6,8,10,13,14,15,16,17)]
head(ten_normal_result)
# Creem el fitxer de dades resultant
write.csv(ten_normal_result, file = "ATP_jugadors_guanys_2017_result.csv", row.names = FALSE)
