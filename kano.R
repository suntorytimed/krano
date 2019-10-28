# based on code released under CC-BY-4.0 by Reynir S. Atlason and Davide Giacalone
# https://creativecommons.org/licenses/by/4.0/

# modified by Stefan Weiberg <sweiberg@suse.com>, SUSE LLC. and released under
# EUPL v1.2 (https://joinup.ec.europa.eu/collection/eupl/eupl-text-11-12)

# This is an R function designed to conduct a quantitative Kano analysis.

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

    collect<-c()

    # create evaluation matrix
    evtable=matrix(c("Q","R","R","R","R","A","I","I","I","R","A","I","I",
                    "I","R","A","I","I","I","R","O","M","M","M","Q"), nrow=5,ncol=5)

    column=matrix(ncol=0,nrow=0)
    counter<-(1)
    counter2<-(1)
    funcnames<-c()
    unimportant<-c()

    # evaluate answers based on the evaluation matrix
    while(counter <= nrow(data)*ncol(data))
    {
        column <- append(column,(evtable[data[[counter,counter2]],
                                        data[[counter,counter2+1]]]));
        counter <- counter+1;
        counter2 <- 1;
        if (counter-1==nrow(data)*ncol(data)/2) break;
    }

    # convert evaluated answers to a dataframe
    col2=matrix(column, ncol=FR)
    col3=matrix((rep(1:FR, each = (nrow(data)/FR), len = nrow(data))), 
                nrow=(nrow(data)/FR), ncol=FR)

    # combine classisfied data with the original dataset which now includes 
    # classification of answers
    class=data.frame(cbind(data,column))
    splitted=as.data.frame(sapply(split(class, col3), "[[", "column"))

    # count the entries of classifications.
    for (j in 1:FR) {
        assign(paste0("frc", j), data.frame(table(splitted[,j])))
    }

    # isolate the answer with the highest count
    for (j in 1:FR) {
        assign(paste0("class", j), as.data.frame(which.max(table(splitted[,j]))))
    }

    # construct the CSi and DSi values
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
    unimportantClassifier<-c()

    for (h in 1:FR) 
        if (rownames(as.data.frame(which.max(table(splitted[,h]))))=="M") {
            # Looking and calculating values for must-have features
            functions=rbind(functions,((((-(exp(1)*(cslist[h]-(dslist[h])))/
                                            (exp(1)-1))*exp(-x)+((exp(1)*cslist[h])-(dslist[h]))/(exp(1)-1)))))
            funcnames=append(funcnames,row.names(splitted[h,]))
            
            classifier=append(classifier,rownames(data.frame(which.max(table(splitted[,h])))))
            finalpoint=append(finalpoint,colnames(splitted[h])) 
        } else if (rownames(as.data.frame(which.max(table(splitted[,h]))))=="O") {
            # Looking and calculating values for one-dimensional features
            functions=rbind(functions,((cslist[h]-(dslist[h]))*x)+dslist[h])
            funcnames=append(funcnames,row.names(splitted[h,]))
            
            classifier=append(classifier,rownames(data.frame(which.max(table(splitted[,h])))))
            finalpoint=append(finalpoint,colnames(splitted[h]))
        } else if (rownames(as.data.frame(which.max(table(splitted[,h]))))=="A") {
            # Looking and calculating values for attractive features
            functions=rbind(functions,((((cslist[h]-dslist[h])/(exp(1)-1)*exp(x))-
                                            (cslist[h]-exp(1)*dslist[h])/(exp(1)-1))))
            funcnames=append(funcnames,row.names(splitted[h,]))
            
            classifier=append(classifier,rownames(data.frame(which.max(table(splitted[,h])))))
            finalpoint=append(finalpoint,colnames(splitted[h]))
        } else {
            # Looking for unimportant features
            unimportant=append(unimportant,row.names(splitted[h,]))
            unimportantClassifier=append(unimportantClassifier,rownames(data.frame(which.max(table(splitted[,h])))))
        }

    results=cbind(funcnames,classifier,format(round(functions[,100],2),nsmall=2),format(round(functions[,1],2),nsmall=2))
    colnames(results)<- c("Attribute Number","Classification","CS Values","DS Values")

    unimportant=cbind(unimportant,unimportantClassifier)
    colnames(unimportant)<- c("Attribute Number", "Classification")

    resultList<-list("resultTable" = results, "FR" = FR, "funcnames" = funcnames, "functions" = functions, "unimportant" = unimportant)
    return(resultList)
}