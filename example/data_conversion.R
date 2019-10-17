# This is an R function that converts the csv output from the
# SUSE Kano form to a two column data structure to use it in
# the example code of the research paper by Reynir S. Atlason
# and Davide Giacalone

library(tester)

data_conversion<-function(dataset){
data<-dataset

# check for valid data input
datacheck<-ncol(data)/2
if(is.data.frame(data)==FALSE){stop("dataset is not a dataframe")
  }
if(is_positive_integer(ncol(data)/datacheck)==FALSE){stop("number of FR does not match size of dataframe")
  }
if(any(is.na(data))==TRUE){stop("dataset contains missing values")
  }

converted_data=matrix(ncol=2,nrow=0)
column_csv<-(1)
row_csv<-(1)

while(column_csv <= ncol(data) && row_csv <= nrow(data))
{
  converted_data <- rbind(converted_data,c(data[[row_csv,column_csv]],
                            data[[row_csv,column_csv+1]]));
                    
  row_csv <- row_csv+1;
  if (row_csv > nrow(data)) {
      column_csv <- column_csv+2;
      row_csv <- 1;
  }
}

write.table(converted_data,file="data.csv",row.names=FALSE,na="",col.names=FALSE,sep=",");

}
