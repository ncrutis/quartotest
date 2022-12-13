desc_table_bygroup <- function(dfr,
                                  vars,
                                  namsvars,
                                  group,
                                  compareGroups=F,
                                  na.rm=F,
                                  digits=4){
  #group <- as.factor(group)

  res_sum <- c()
  namtab <- cbind(vars,namsvars)
  # print(namtab)

  for (cur_var in unique(vars) )  {


    cur_name_var<-namtab[vars==cur_var,][2]
    # print(cur_name_var)
    # print(cur_var)


    if (is.numeric(dfr[,cur_var])==T){

      #Mean and Missing value
      tab1 <- Publish::univariateTable(as.formula(paste(group," ~ ",paste(cur_var,collapse = " + "))),
                                       data=dfr,compareGroups=F,na.rm=F,digits=c(1,1,3), big.mark=",")
      tab1 <- as.matrix(print(tab1))
      tab1[1,2] <- c("Mean (SD)")
      tab1[which(tab1[,1]!=""),1] <- ""


      #Median and iqr
      tab2 <- Publish::univariateTable(as.formula(paste(group," ~ ",paste(cur_var,collapse = " + "))),
                                       data=dfr,compareGroups=F,summary.format = "median(x) (iqr(x))",
                                       na.rm=F,digits=c(1,1,3), big.mark = ",")
      tab2 <- as.matrix(print(tab2))
      tab2[1,2] <- c("Median (IQR)")
      tab2[1,c(3:(ncol(tab2)-1))] <- gsub(",","-",tab2[1,c(3:(ncol(tab2)-1))])
      tab2[1,c(3:(ncol(tab2)-1))] <- gsub(" ","",tab2[1,c(3:(ncol(tab2)-1))],perl = T)
      tab2[which(tab2[,1]!=""),1] <- ""

      #range
      tab3 <- Publish::univariateTable(as.formula(paste(group," ~ ",paste(cur_var,collapse = " + "))),
                                       data=dfr,compareGroups=F,summary.format = "range(x)",na.rm=F,digits=c(1,1,3), big.mark = ",")
      tab3 <- as.matrix(print(tab3))
      tab3[1,2] <- c("Min-Max")
      tab3[1,c(3:(ncol(tab3)-1))] <- gsub(",","-",tab3[1,c(3:(ncol(tab3)-1))])
      tab3[1,c(3:(ncol(tab3)-1))] <- gsub(" ","",tab3[1,c(3:(ncol(tab3)-1))],perl = T)
      tab3[which(tab3[,1]!=""),1] <- ""


      #N
      tab4 <- as.data.frame(
        t(summaryBy(as.formula(paste(paste(cur_var,collapse = " + ")," ~ ",group)) ,
                    data=as.data.frame(dfr),
                    FUN=function(x){c(N=length(na.omit(x)))}))
      )


      tab4 <- tab4[-1,]

      tab4[,1:length(tab4)] <- sapply(as.matrix(tab4[,1:length(tab4)]), FUN= function(x) as.numeric(as.character(x))  )
      tab4$total <- sum(tab4[,1:c(length(tab4))])
      tab4[,1:length(tab4)] <- sapply(as.matrix(tab4[,1:length(tab4)]), FUN= function(x) format(x, big.mark = ",")  )
      tab4$Variable <- cur_name_var
      tab4$Level <- "n"
      tab4$p <- ""

      tab4 <- tab4[,c("Variable","Level",1:(length(tab4)-4),"total","p")]
      #tab4[,3:length(tab4)]<-apply(tab4[,3:length(tab4)],2,function(x) as.character(x))

      row.names(tab4) <- NULL

      colnames(tab4) <- colnames(tab1)

      #missing

      tab5 <- as.data.frame(
        t(summaryBy(as.formula(paste(paste(cur_var,collapse = " + ")," ~ ",group)) ,
                    data=as.data.frame(dfr),
                    FUN=function(x){c(missing=sum(is.na(x)==T))}))
      )


      tab5 <- tab5[-1,]

      tab5[,1:length(tab5)] <- sapply(as.matrix(tab5[,1:length(tab5)]), FUN= function(x) as.numeric(as.character(x))  )
      tab5$total<-sum(tab5[,1:c(length(tab5))])

      tab5$Variable <- ""
      tab5$Level <- "Missing values"
      tab5$p <- ""

      # print(tab5)
      tab5 <- tab5[,c("Variable","Level",1:(length(tab5)-4),"total","p")]
      tab5[,3:length(tab5)] <- apply(tab5[,3:length(tab5)],2,function(x) as.character(x))
      row.names(tab5) <- NULL

      colnames(tab5) <- colnames(tab1)



      res_sum <- rbind(res_sum,tab4,tab1[1,],tab2[1,],tab3[1,],tab5)

    }else{

      #Frequency and percentage
      tab1 <- Publish::univariateTable(as.formula(paste(group," ~ ",paste(cur_var,collapse = " + "))),data=dfr,
                                       compareGroups=F,na.rm=F,digits=c(1,1,3), big.mark = ",")
      tab1 <- as.matrix(print(tab1))
      tab1[which(tab1[,1]!=""),1] <- ""
      # print(tab1)

      #N
      tab4<-as.data.frame(
        t(summaryBy(as.formula(paste(paste(cur_var,collapse = " + ")," ~ ",group)) ,
                    data=as.data.frame(dfr),
                    FUN=function(x){c(N=length(na.omit(x)))}))
      )


      tab4<-tab4[-1,]

      tab4[,1:length(tab4)] <- sapply(as.matrix(tab4[,1:length(tab4)]), FUN= function(x) as.numeric(as.character(x))  )

      tab4$total<-sum(tab4[,1:c(length(tab4))])


      tab4[,1:length(tab4)] <- sapply(as.matrix(tab4[,1:length(tab4)]), FUN= function(x) format(x, big.mark = ",")  )

      tab4$Variable <- cur_name_var
      tab4$Level <- "n"


      tab4$p <- ""

      tab4 <- tab4[,c("Variable","Level",1:(length(tab4)-4),"total","p")]
      #tab4[,3:length(tab4)]<-apply(tab4[,3:length(tab4)],2,function(x) as.character(x))

      row.names(tab4) <- NULL

      colnames(tab4) <- colnames(tab1)

      #missing

      tab5 <- as.data.frame(
        t(summaryBy(as.formula(paste(paste(cur_var,collapse = " + ")," ~ ",group)) ,
                    data=as.data.frame(dfr),
                    FUN=function(x){c(missing=sum(is.na(x)==T))}))
      )


      tab5 <- tab5[-1,]

      tab5[,1:length(tab5)] <- sapply(as.matrix(tab5[,1:length(tab5)]), FUN= function(x) as.numeric(as.character(x))  )
      tab5$total <- sum(tab5[,1:c(length(tab5))])

      tab5$Variable <- ""
      tab5$Level <- "Missing values"
      tab5$p <- ""

      # print(tab5)
      tab5 <- tab5[,c("Variable","Level",1:(length(tab5)-4),"total","p")]
      tab5[,3:length(tab5)] <- apply(tab5[,3:length(tab5)],2,function(x) as.character(x))
      row.names(tab5) <- NULL

      colnames(tab5) <- colnames(tab1)

      tab1 <- tab1[which(tab1[,2]!="missing"),]

      res_sum <- rbind(res_sum,tab4,tab1,tab5)

    }
  }

  res_sum <- as.data.frame(res_sum[,-length(res_sum)])


  # print(colnames(res_sum)[3:length(res_sum)])

  # colnames(res_sum)<-c("Variable","",colnames(res_sum)[3:length(res_sum)])

  return(res_sum)

}


