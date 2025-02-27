#����������� ����������� ���������
library(rattle)
library(gWidgets)
library(gWidgetstcltk)
library(rpart)


set.seed(123)

stop <- FALSE


#�������� ���� � ������ ������� ��������� �������� � ��������� ���������� 
window<-gwindow(title="���������� 1.0")
#���� ���� � �������
pathFrame <-gframe("�������� �������", container=window, horizontal = TRUE)
datat <- gedit(container=pathFrame)
obj <- gbutton("�����", container=pathFrame, handler = function(h) {svalue(datat)<-gfile(text = "Select file", type = "open")})
data<-read.csv(svalue(datat))
#���� �������� ��������
mainParmFrame <- gframe("�������� �������", container=window)
mainParmName <- gedit(container = mainParmFrame)
targetVar <- svalue(mainParmName)
#���� ��������� ���������� ��� �������
otherParmFrame<-gframe("��������� ���������",container=window,horizontal=FALSE)
dataPartCoef2G <- ggroup(container=otherParmFrame,horizontal=TRUE)
dataPartCoef2L <- glabel("������� ������ ��� ����������",container=dataPartCoef2G)
dataPartCoef2E <- gedit("0.5",container=dataPartCoef2G)
iterationsG <- ggroup(container=otherParmFrame,horizontal=TRUE)
iterationsL <- glabel("���������� ��������",container=iterationsG)
iterationsE <- gedit("10001",container=iterationsG)
#����� ������������ ���������
rows<-0
tableNames<-names(data)[-which(names(data)==svalue(mainParmName))]
ChooseRows<-gbutton("����� ��������", container=window, handle=function(h){
    tableNames<<-names(data)[-which(names(data)==svalue(mainParmName))]    
    if(rows!=0){
    delete(window,rows)
    }
    data<<-read.csv(svalue(datat))
    targetVar<<-svalue(mainParmName)
    rows <<- gcheckboxgroup(names(data)[-which(names(data)==svalue(mainParmName))],container=window,checked=TRUE)
})
#���������� ������ ������ ����������
apply <- gbutton("������", container=window, handle = function(h){
stop<-FALSE
#������ �������� � ��������� ���� ���������� � ����������
tableNames<<-names(data)[-which(names(data)==svalue(mainParmName))]
data<<-read.csv(svalue(datat))
targetVar <- svalue(mainParmName)
#�������� ���� � ������������
resultWin <- gwindow(title = "����������")
#�����  ���� resultWin ��������� �������� � ���������
accurFrame <- gframe("��������� ��������", container = resultWin)
bestAccure <- glabel(text = 0, container = accurFrame)
paramsFrame<-gframe("���������", container=resultWin, horizontal=FALSE)
ParDepth<-glabel(text = "Depth: ", container=paramsFrame)
ParSplit<-glabel(text = "InSplit: ", container=paramsFrame)
ParInBucket<-glabel(text = "InBucket: ", container=paramsFrame)
ParPruneFlag<-glabel(text = "Pruning On-Off: ", container=paramsFrame)
#����� ������� ������������� ���������
VarFriquenceFrame<-gframe("������� ������������� ���������", container=resultWin)
#���������� ������ ���������
stopButton <- gbutton("stop", container=resultWin, handle=function(h){stop<<-TRUE})
#���������� ������� � ����������� ������������ �������
#������� ������ �� ������������� � ��������
Index <- sample(1:nrow(data), 0.8*nrow(data))
train <- data[Index,]
test <- data[-Index,]
#
dataPartCoef1 <- 0.9
dataPartCoef2 <- as.numeric(svalue(dataPartCoef2E))
#�������, ������������ �������� ������ �� �������� ������
calcAccuracyTree<-function(tree, test, target)
{
   prediction <- predict(tree, test, type = 'class')
   res <- sum(test[,target]==prediction)/length(prediction)
   res
}
#������� ���������� ������ � �����������
trainTree<-function(trainData, ctrl, formula, pruneFlag)
{  
 
   tree <- rpart(formula, data = trainData, method = 'class', parms = list(split = "gini"), control=ctrl)
   #�������
   if( pruneFlag )
   {
       tree <- prune(tree, cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
   }
   tree
}
#���������� formula ������������ ��������, ��������� �������������
formula <- paste(targetVar,' ~ .')
#���������� ���������� ������ � ������� ������������ �����������
tree<- rpart(formula, data = train, method = 'class', parms = list(split = "gini"))
#������ ������ � �������� ��� �������� ��� ��������� � ���������� 
bestRes <- calcAccuracyTree(tree,test,targetVar)
bestRes0 <- bestRes 
bestTree	 <- tree
#print(paste("Initial res =", bestRes))

#�������� ������� ���������� ��������� ����������
MinDepth <- 3
MaxDepth <- 20
MinInSplit <- 1
MaxInSplit <- 200
MinInBucket <- 1
MaxInBucket <- 100
PruneFlag <- 1

#���������� ������ ������� ������������� ���������
VarFriquence<-c()
for(i in 1:length(tableNames)){
VarFriquence<-c(VarFriquence,0)
}
VarFriquenceShow <- c()
lab<-glabel(0,container=VarFriquenceFrame)
#������ ����
for (i in 1:as.numeric(svalue(iterationsE)))
{
   
   #������������� ���������� ��������� ��������
   ms <- sample(MinInSplit:MaxInSplit, 1)
   md <- sample(MinDepth:MaxDepth, 1)
   mb <- sample(MinInBucket:MaxInBucket, 1)
   mp <- sample(0:1, 1)
   #����������� ���������, ������� ����� �������������� � ����������
   sign<-""
   for (j in svalue(rows))
   {
   	sign<-paste(sign,j,"+")
   }
   sign<-substring(sign,1,nchar(sign)-2)
   formula <- paste(targetVar,' ~ ',sign)
   #�� ������ ����� ���������� ����� ��������� ������
   ctrl=rpart.control(cp=0, minsplit = ms, maxdepth = md, minbucket = mb)
   #��� ��������� ������ ���������� ���� �������� ������ 
   #�� ������������� � �������������
   Index<-sample(1:nrow(train),dataPartCoef2*nrow(train))
   trainPart<-train[Index,]
   testPart<-train[-Index,]
   #���������� ������ � ���������� ����������� 
   tree <- trainTree(trainPart, ctrl, formula, mp)
   #������������ �������� ����� ������ �� ������������� ������
   res <- calcAccuracyTree(tree,testPart,targetVar)
   #���������� ��������
   checkCoef = 0.95
   #��������� ������� ������, ����������� �� ��������� ���������� � ��������
   if( res > bestRes*checkCoef )
   {
      #�������� ��������� �����, ��� ��������� ������ � ���� �� �����������,
	#�� ��� �� ������ ������������� ������, ����������� �� �����
      res <- calcAccuracyTree(tree,test,targetVar)
      tree2 <- trainTree(train, ctrl, formula, mp)
      res2 <- calcAccuracyTree(tree2,test,targetVar)
      reducedFlag = 1
      #���� ����������� �����, �� ����� ��������������
      if( res2 > res )
      {
         res <- res2
         tree <- tree2
         reducedFlag <- 0
      }
      #������������� ��������� � ������� ������ ������� � ����� ���������� �� ����� � ��������� ������
      if( res > bestRes )
      {
         #print(paste("Best res =", res, "Reduced data used:", reducedFlag, "Pruning on-off=",mb, "Params: MaxDepth=", md, "MinSplit=",ms, "MinBucket=",mb))
         svalue(bestAccure)<-paste("Best result:", res)
	   svalue(ParDepth)<-paste("Depth:", md)
	   svalue(ParSplit)<-paste("Split:", ms)
	   svalue(ParInBucket)<-paste("Bucket:", mb)
	   bestRes <- res
         bestTree <- tree
         plot(tree)
         text(tree)
      } 
   }
   #����� ������� ������������� ������� ���������
   for(j in 1:length(tableNames)){
       if(tableNames[j] %in% names(tree$variable)){
           VarFriquence[j]<-VarFriquence[j]+1
	 }
	VarFriquenceShow<-paste(VarFriquenceShow,tableNames[j] ,round(VarFriquence[j]/i*100,2),"%\n")
   }
   svalue(lab)<-VarFriquenceShow
   VarFriquenceShow<-""
   #��������� �����, ��� ������� �� "����"
   if(stop==TRUE){
	break
   }
}
#�������� ������� ������ �� ���������� ����������� � � ������������� �������
print(paste("Initial res =", bestRes0))
bestRes = calcAccuracyTree(bestTree, test, targetVar)
print(paste("Current res =", bestRes))
if( bestRes0 > bestRes )
{
   bestTree <- bestTree0 
   bestRes <- bestRes0
}
print(paste("Final res =", bestRes))
#plot(bestTree)
#text(bestTree)

})
