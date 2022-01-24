#Подключение необходимых библиотек
library(rattle)
library(gWidgets)
library(gWidgetstcltk)
library(rpart)


set.seed(123)

stop <- FALSE


#создание окна с вводом таблицы ключевого признака и начальных параметров 
window<-gwindow(title="ГИПЕРПОИСК 1.0")
#ввод пути к таблице
pathFrame <-gframe("Выберите таблицу", container=window, horizontal = TRUE)
datat <- gedit(container=pathFrame)
obj <- gbutton("Обзор", container=pathFrame, handler = function(h) {svalue(datat)<-gfile(text = "Select file", type = "open")})
data<-read.csv(svalue(datat))
#ввод ключеого признака
mainParmFrame <- gframe("ключевой признак", container=window)
mainParmName <- gedit(container = mainParmFrame)
targetVar <- svalue(mainParmName)
#ввод начальных параметров для запуска
otherParmFrame<-gframe("начальные параметры",container=window,horizontal=FALSE)
dataPartCoef2G <- ggroup(container=otherParmFrame,horizontal=TRUE)
dataPartCoef2L <- glabel("Процент данных для тренировки",container=dataPartCoef2G)
dataPartCoef2E <- gedit("0.5",container=dataPartCoef2G)
iterationsG <- ggroup(container=otherParmFrame,horizontal=TRUE)
iterationsL <- glabel("Количество итераций",container=iterationsG)
iterationsE <- gedit("10001",container=iterationsG)
#выбор используемых признаков
rows<-0
tableNames<-names(data)[-which(names(data)==svalue(mainParmName))]
ChooseRows<-gbutton("Выбор столбцов", container=window, handle=function(h){
    tableNames<<-names(data)[-which(names(data)==svalue(mainParmName))]    
    if(rows!=0){
    delete(window,rows)
    }
    data<<-read.csv(svalue(datat))
    targetVar<<-svalue(mainParmName)
    rows <<- gcheckboxgroup(names(data)[-which(names(data)==svalue(mainParmName))],container=window,checked=TRUE)
})
#реализация кнопки начала тренировки
apply <- gbutton("Начать", container=window, handle = function(h){
stop<-FALSE
#запись введённых в начальном окне параметров в переменные
tableNames<<-names(data)[-which(names(data)==svalue(mainParmName))]
data<<-read.csv(svalue(datat))
targetVar <- svalue(mainParmName)
#создание окна с результатами
resultWin <- gwindow(title = "результаты")
#вывод  окно resultWin наилучшую точность и параметры
accurFrame <- gframe("наилучшая точность", container = resultWin)
bestAccure <- glabel(text = 0, container = accurFrame)
paramsFrame<-gframe("параметры", container=resultWin, horizontal=FALSE)
ParDepth<-glabel(text = "Depth: ", container=paramsFrame)
ParSplit<-glabel(text = "InSplit: ", container=paramsFrame)
ParInBucket<-glabel(text = "InBucket: ", container=paramsFrame)
ParPruneFlag<-glabel(text = "Pruning On-Off: ", container=paramsFrame)
#вывод частоты использования признаков
VarFriquenceFrame<-gframe("частота использования признаков", container=resultWin)
#реализация кнопки остановки
stopButton <- gbutton("stop", container=resultWin, handle=function(h){stop<<-TRUE})
#построение деревва с параметрами выстроенными вручную
#деление данных на тренировочные и тестовые
Index <- sample(1:nrow(data), 0.8*nrow(data))
train <- data[Index,]
test <- data[-Index,]
#
dataPartCoef1 <- 0.9
dataPartCoef2 <- as.numeric(svalue(dataPartCoef2E))
#функция, возвращающая точность дерева на тэстовых данных
calcAccuracyTree<-function(tree, test, target)
{
   prediction <- predict(tree, test, type = 'class')
   res <- sum(test[,target]==prediction)/length(prediction)
   res
}
#вункция тренировки дерева с параметрами
trainTree<-function(trainData, ctrl, formula, pruneFlag)
{  
 
   tree <- rpart(formula, data = trainData, method = 'class', parms = list(split = "gini"), control=ctrl)
   #прунинг
   if( pruneFlag )
   {
       tree <- prune(tree, cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
   }
   tree
}
#переменная formula записываются признаки, выбранные пользователем
formula <- paste(targetVar,' ~ .')
#тренировка начального дерева с вручную настроенными параметрами
tree<- rpart(formula, data = train, method = 'class', parms = list(split = "gini"))
#запись дерева и точности его точности для сравнения в дальнейшем 
bestRes <- calcAccuracyTree(tree,test,targetVar)
bestRes0 <- bestRes 
bestTree	 <- tree
#print(paste("Initial res =", bestRes))

#создание области генерацции случайных параметров
MinDepth <- 3
MaxDepth <- 20
MinInSplit <- 1
MaxInSplit <- 200
MinInBucket <- 1
MaxInBucket <- 100
PruneFlag <- 1

#подготовка вывода частоты использования признаков
VarFriquence<-c()
for(i in 1:length(tableNames)){
VarFriquence<-c(VarFriquence,0)
}
VarFriquenceShow <- c()
lab<-glabel(0,container=VarFriquenceFrame)
#запуск цика
for (i in 1:as.numeric(svalue(iterationsE)))
{
   
   #присваиввание параметрам случайные значения
   ms <- sample(MinInSplit:MaxInSplit, 1)
   md <- sample(MinDepth:MaxDepth, 1)
   mb <- sample(MinInBucket:MaxInBucket, 1)
   mp <- sample(0:1, 1)
   #определение признаков, которые будут использоваться в тренировке
   sign<-""
   for (j in svalue(rows))
   {
   	sign<-paste(sign,j,"+")
   }
   sign<-substring(sign,1,nchar(sign)-2)
   formula <- paste(targetVar,' ~ ',sign)
   #на основе каких параметров будет строиться дерево
   ctrl=rpart.control(cp=0, minsplit = ms, maxdepth = md, minbucket = mb)
   #для ускорения работы разделение базы тестовых данных 
   #на тренировочные и валидационные
   Index<-sample(1:nrow(train),dataPartCoef2*nrow(train))
   trainPart<-train[Index,]
   testPart<-train[-Index,]
   #построение дерева с случайными параметрами 
   tree <- trainTree(trainPart, ctrl, formula, mp)
   #высчитывание точности этого дерева на валидационных данных
   res <- calcAccuracyTree(tree,testPart,targetVar)
   #коэффицент проверки
   checkCoef = 0.95
   #сравнение лучшего дерева, построеного на случайных параметрах и текущего
   if( res > bestRes*checkCoef )
   {
      #точность оказалась лучше, идёт постройка дерева с теми же параметрами,
	#но уже на основе тренировочных данных, определённых до цикла
      res <- calcAccuracyTree(tree,test,targetVar)
      tree2 <- trainTree(train, ctrl, formula, mp)
      res2 <- calcAccuracyTree(tree2,test,targetVar)
      reducedFlag = 1
      #если оказывается лучше, то дерео переписывается
      if( res2 > res )
      {
         res <- res2
         tree <- tree2
         reducedFlag <- 0
      }
      #окончательное сравнение с прошлым лучшим деревом и вывод параметров на экран и зарисовка дерева
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
   #вывод частоты использования каждого параметра
   for(j in 1:length(tableNames)){
       if(tableNames[j] %in% names(tree$variable)){
           VarFriquence[j]<-VarFriquence[j]+1
	 }
	VarFriquenceShow<-paste(VarFriquenceShow,tableNames[j] ,round(VarFriquence[j]/i*100,2),"%\n")
   }
   svalue(lab)<-VarFriquenceShow
   VarFriquenceShow<-""
   #остановка цикла, при нажатии на "стоп"
   if(stop==TRUE){
	break
   }
}
#сранение лучшего дерева со случайными параметрами и с выставленными вручную
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
