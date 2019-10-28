library(plyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(tester)

kano<-function(dataset){
    pre_convert_data<-dataset
    FR<-ncol(pre_convert_data)/2

    # check for valid data input
    if(is.data.frame(pre_convert_data)==FALSE){stop("dataset is not a dataframe")
    }
    if(is_positive_integer(ncol(pre_convert_data)/FR)==FALSE){stop("number of FR does not match size of dataframe")
    }
    if(any(is.na(pre_convert_data))==TRUE){stop("dataset contains missing values")
    }

    if(ncol(pre_convert_data) == 2){
        converted_data<-pre_convert_data
    }
    else {
        converted_data=matrix(ncol=2,nrow=0)
        column_csv<-(1)
        row_csv<-(1)

        while(column_csv <= ncol(pre_convert_data) && row_csv <= nrow(pre_convert_data))
        {
            converted_data <- rbind(converted_data,c(pre_convert_data[[row_csv,column_csv]],
                                                    pre_convert_data[[row_csv,column_csv+1]]));
            
            row_csv <- row_csv+1;
            if (row_csv > nrow(pre_convert_data)) {
                column_csv <- column_csv+2;
                row_csv <- 1;
            }
        }
    }

    data<-converted_data

    x=seq(from = 0.0, to = 1, by = 0.01)
    y=seq(from = 0.0, to = 1, by = 0.01)
    datacount=(1)

    # Here we create the empty list called collect
    collect<-c()

    # Here is the evaluation table created
    evtable=matrix(c("Q","R","R","R","R","A","I","I","I","R","A","I","I",
                    "I","R","A","I","I","I","R","O","M","M","M","Q"), nrow=5,ncol=5)

    #Here the answers are evaluated based on the evaluation table

    column=matrix(ncol=0,nrow=0)
    counter<-(1)
    counter2<-(1)
    funcnames<-c()

    while(counter <= nrow(data)*ncol(data))
    {
        column <- append(column,(evtable[data[[counter,counter2]],
                                        data[[counter,counter2+1]]]));
        counter <- counter+1;
        counter2 <- 1;
        if (counter-1==nrow(data)*ncol(data)/2) break;
    }

    # Here the evaluated answers are converted to a dataframe

    col2=matrix(column, ncol=FR)
    col3=matrix((rep(1:FR, each = (nrow(data)/FR), len = nrow(data))), 
                nrow=(nrow(data)/FR), ncol=FR)

    # Here the classified data is combined with the original dataset which now includes 
    # classification of answers

    class=data.frame(cbind(data,column))
    splitted=as.data.frame(sapply(split(class, col3), "[[", "column"))

    # Here we count the entries of classifications.

    for (j in 1:FR) {
        assign(paste0("frc", j), data.frame(table(splitted[,j])))
    }

    # Here we isolate the answer with the highest count

    for (j in 1:FR) {
        assign(paste0("class", j), as.data.frame(which.max(table(splitted[,j]))))
    }

    # Here we construct the CSi and DSi values, note that here I must make a loop!
    cslist=c()
    for (i in 1:FR) {
        cslist=append(cslist,(sum(splitted[[i]]=="A")+sum(splitted[[i]]=="O"))/
                        ((sum(splitted[[i]]=="A")+sum(splitted[[i]]=="O")
                        +sum(splitted[[i]]=="M")+sum(splitted[[i]]=="I"))))
    }

    dslist=c()
    for (k in 1:FR) {
        dslist=append(dslist,-(sum(splitted[[k]]=="O")+sum(splitted[[k]]=="M"))/
                        ((sum(splitted[[k]]=="A")+sum(splitted[[k]]=="O")
                        +sum(splitted[[k]]=="M")+sum(splitted[[k]]=="I"))))
    }

    functions=data.frame()
    finalpoint<-matrix(ncol=0,nrow=0)
    classifier<-c()
    indifferent<-c()
    reverse<-c()
    questionable<-c()

    # Here we calculate the Must-be values, this must only be done if and only if we have 
    # Must-be attributes
    for (h in 1:FR) 
        if (rownames(as.data.frame(which.max(table(splitted[,h]))))=="M")
        {
        functions=rbind(functions,((((-(exp(1)*(cslist[h]-(dslist[h])))/
                                        (exp(1)-1))*exp(-x)+((exp(1)*cslist[h])-(dslist[h]))/(exp(1)-1)))))
        funcnames=append(funcnames,row.names(splitted[h,]))
        
        classifier=append(classifier,rownames(data.frame(which.max(table(splitted[,h])))))
        finalpoint=append(finalpoint,colnames(splitted[h])) 
        }

    # Here we calculate the One-dimensional values, this must only be done if have 
    # and only if we one-dimensional attributes
    for (h in 1:FR) 
        if (rownames(as.data.frame(which.max(table(splitted[,h]))))=="O")
        {
        functions=rbind(functions,((cslist[h]-(dslist[h]))*x)+dslist[h])
        funcnames=append(funcnames,row.names(splitted[h,]))
        
        classifier=append(classifier,rownames(data.frame(which.max(table(splitted[,h])))))
        finalpoint=append(finalpoint,colnames(splitted[h]))
        }

    # Here we calculate the Attractive values, this must only be done if and only if we have 
    # Attractive attributes

    for (h in 1:FR) 
        if (rownames(as.data.frame(which.max(table(splitted[,h]))))=="A")
        {
        functions=rbind(functions,((((cslist[h]-dslist[h])/(exp(1)-1)*exp(x))-
                                        (cslist[h]-exp(1)*dslist[h])/(exp(1)-1))))
        funcnames=append(funcnames,row.names(splitted[h,]))
        
        classifier=append(classifier,rownames(data.frame(which.max(table(splitted[,h])))))
        finalpoint=append(finalpoint,colnames(splitted[h]))
        }

    #Here we look for Indifferent values

    for (h in 1:FR) 
        if (rownames(as.data.frame(which.max(table(splitted[,h]))))=="I")
        {
        indifferent=append(indifferent,colnames(splitted[h]))  
        indifferent=append(indifferent,rownames(data.frame(which.max(table(splitted[,h])))))
        }

    #Here we look for Reverse values

    for (h in 1:FR) 
        if (rownames(as.data.frame(which.max(table(splitted[,h]))))=="R")
        {
        reverse=append(reverse,colnames(splitted[h]))
        reverse=append(reverse,rownames(data.frame(which.max(table(splitted[,h])))))
        }

    #Here we look for Reverse values

    for (h in 1:FR) 
        if (rownames(as.data.frame(which.max(table(splitted[,h]))))=="Q")
        {
        reverse=append(questionable,colnames(splitted[h]))
        reverse=append(questionable,rownames(data.frame(which.max(table(splitted[,h])))))
        }


    results=cbind(funcnames,classifier,format(round(functions[,100],2),nsmall=2),format(round(functions[,1],2),nsmall=2))
    colnames(results)<- c("Attribute Number","Classification","CS Values","DS Values")

    resultList<-list("resultTable" = results, "FR" = FR, "funcnames" = funcnames, "functions" = functions)
    return(resultList)
}