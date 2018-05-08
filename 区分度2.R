setwd("C:\\Users\\wn006\\Desktop\\transfer\\儿童发音\\last data")
datsm <- read.csv("1.儿童-声母明细-错误率.csv" , header = TRUE)
datsm$声母 <- as.character(datsm$声母)
compsm <- datsm$声母[1:length(unique(datsm$声母))]

datym <- read.csv("1.儿童-韵母明细-错误率.csv" , header = TRUE)
datym$韵母 <- as.character(datym$韵母)
compym <- datym$韵母[1:length(unique(datym$韵母))]

datsd <- read.csv("1.儿童-声调明细-错误率.csv" , header = TRUE)
datsd$声调 <- as.character(datsd$声调)
compsd <- datsd$声调[1:length(unique(datsd$声调))]


#下面生成所有的文件名
allfile <- paste0(c(compsm , compym , compsd) , ".csv")

setwd("C:\\Users\\wn006\\Desktop\\transfer\\儿童发音\\last data\\last")
dat_list <- read.csv("7.2声韵调_加入综合错误率.csv" , header = TRUE)

dat_list$性别 <- as.character(dat_list$性别)
allkey <- unique(dat_list$X)
library(plyr)
library(readr)



for(i in 1:length(allfile))
{
  print(i)
  xfile <- allfile[i]
  setwd("C:\\Users\\wn006\\Desktop\\transfer\\儿童发音\\区分度\\8.分词错误率")
  datthis <- read_csv(xfile , locale = locale(encoding = "GBK"))
  dati <- datthis[datthis$发声总次数 != 0,c(1:9,12,13)]
  dati$键 <- paste0(dati$儿童编号 , dati$测试序号)
  
  dati <- dati[dati$声母 == names(which.max(table(dati$声母))),]
  dati$错误次数 <- as.numeric(as.character(dati$错误次数))
  dati$词 <- as.character(dati$词)
  dati$错误率 <- dati$错误次数/dati$发声总次数
  dati$认识率 <- as.character(dati$认识率)
  dati$是否属于500词表 <- as.numeric(as.character(dati$是否属于500词表))
  
  report_shouldnot <- unique(dati$键[as.logical(1-(dati$键 %in% allkey))])
  #这些小孩不应该出现在dati数据集里的
  
  if(prod(dati$键 %in% allkey) != 1)
  {
    print("Error:没有全部包含键")
    dati <- dati[dati$键 %in% allkey,]
  }
  dati$性别 <- NA
  dati$年龄段6 <- NA
  dati$熵权法错误率 <- NA
  sapply(unique(dati$键) , function(x)
  {genv <- globalenv()
  genv$dati[dati$键 ==x,"性别"] <- unique(dat_list[dat_list$X ==x,"性别"])
  genv$dati[dati$键 == x,"年龄段6"] <- unique(dat_list[dat_list$X ==x,"年龄段_6个"])
  genv$dati[dati$键 == x,"熵权法错误率"] <- 
    unique((dat_list[dat_list$X == x,"综合错误率_按6个年龄段聚类"]))
  })
  all_comb <- expand.grid(年龄段 = unique(dati$年龄段6) , 
                             性别 = unique(dati$性别),KEEP.OUT.ATTRS = FALSE)
  all_comb$性别 <- as.character(all_comb$性别)
  output <- function(xr)
  {
    dati_s <- subset(dati , 年龄段6 == as.numeric(xr[1]) & 
                       性别 == as.character(xr[2]) , select = 月龄:熵权法错误率)
    allword <- unique(dati_s$词)
    var2 <- function(x)
    {
      x <- x[as.logical(1-is.na(x))]
      return(sum((x - mean(x))^2)/length(x))
    }
    getdiff <- function(x)#算出区分度的函数
    {
      tempdata <- dati_s[dati_s$词 == x,"错误率"]
      tempdata <- tempdata$错误率
      return(var2(tempdata))
    }
    alldiff <- sapply(allword , getdiff)#第一种区分度算法：方差
    
    getdiff2 <- function(x)
    {
      tempdata <- dati_s[dati_s$词 == x,"错误率"]
      tempdata <- tempdata$错误率
      data_sorted <- sort(tempdata)
      bound <- round(length(tempdata) * 0.27 , 0)#算出27%的长度
      tail_27 <- mean(data_sorted[(length(data_sorted) + 1 - bound):length(data_sorted)])
      head_27 <- mean(data_sorted[1 : bound])
      return(tail_27 - head_27)
    }
    alldiff2 <- sapply(allword , getdiff2) #第二种区分度算法：前27%与后27%平均值之差
    
    
    getlong <- function(x)#算出词的次数
    {
      tempdata <- dati_s[dati_s$词 == x,"错误率"]
      tempdata <- tempdata$错误率
      return(length(tempdata))
    }
    allnum <- sapply(allword , getlong)
    
    getfr <- function(x)
    {
      tempdata <- dati_s[dati_s$词 == x,]
      tep <- apply(tempdata[,c("发声总次数","错误次数")] , 2,sum,na.rm = TRUE)
      return(tep[2]/tep[1])
    }
    allfr <- sapply(allword , getfr)
    
    getkown <- function(x)
    {
      tempdata <- dati_s[dati_s$词 == x,"认识率"]
      return(unique(tempdata$认识率))
    }
    allknow <- sapply(allword , getkown)
    
    get500 <- function(x)
    {
      tempdata <- dati_s[dati_s$词 == x,"是否属于500词表"]
      return(unique(tempdata$是否属于500词表))
    }
    all500 <- sapply(allword , get500)
    
    get_qn <- function(x)#计算区分度3和新的难度
    {
      tempdata <- dati_s[dati_s$词 == x,c("错误率" , "熵权法错误率")]
      tempdata2 <- arrange(tempdata , desc(熵权法错误率))
      n <- dim(tempdata2)[1]
      bound <- round(n * 0.27 , 0)#算出27%的长度
      head_27 <- mean(tempdata2$错误率[1:bound] , na.rm = TRUE)
      tail_27 <- mean(tempdata2$错误率[(n+1-bound):n] , na.rm = TRUE)
      qfd3 <- head_27 - tail_27
      nandu <- (head_27 + tail_27)/2
      consit <- cor(tempdata$错误率 , tempdata$熵权法错误率,
                    use = "pairwise.complete.obs",
                    method = "spearman")
      return(c(区分度3 = qfd3 , 难度 = nandu , 区分度4 = consit))
    }
    allqn <- t(sapply(allword , get_qn))
    
    qfd3 <- allqn[,1]
    nd <- allqn[,2]
    qfd4 <- allqn[,3]
    

    diffdata <- data.frame(词=allword , 区分度=alldiff,区分度4 = qfd4 ,
                            区分度2 = alldiff2 , 词的次数 = allnum,
                            总体错误率 = allfr , 认识率 = allknow,
                            是否属于500词表 = all500 , 
                            区分度3 = qfd3 , 
                            难度 = nd)
    rownames(diffdata) <- NULL
    diffdata <- arrange(diffdata,desc(区分度3),desc(词的次数),desc(难度))
    out_wd <- paste0("C:\\Users\\wn006\\Desktop\\transfer\\儿童发音\\last data\\分词-分年龄段-结果\\",
                     as.character(xr[2]) , "\\" , as.numeric(xr[1]))
    setwd(out_wd)
    write.csv(diffdata , file = xfile)
  }
  apply(all_comb , 1 , output)
}








