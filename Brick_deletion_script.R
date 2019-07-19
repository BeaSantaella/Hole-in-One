#example randomly delete solid elements from a nastran file
data_set <- read.table(file.choose(),
				row.names=NULL,fill=TRUE,col.names=1:9)
data_set[is.na(data_set)] <- " "
dim(data_set)
Solid_cylinder_5mmx10mm<-data_set

#######################################################################################
#RANDOM BRICK DELETION FUNCTION                                                       #
#NOTE: RESULTING MODEL FILE NEEDS TO  BE CLEANED BEFORE FURTHER PROCESSING OR SOLVING #
#######################################################################################

#main function to remove random brick elements (as percentage of all bricks) from model file
random_brick_deletion <- function(nastran_file, deletion_percent){
#start recording computational time
ptm <- proc.time()
#main model file
set_data<-nastran_file
#beginning number of the tetra4 count
start_num=1
#end number of tetra4 count, obtained by extracting length of Tetra4 block in file
end_num=nrow(set_data[is.element(set_data[,1],"Tetra4"),])
#generating a numerical sequence to randomly sample from
tetra_data_range<-seq(from=start_num, to=end_num, by=1)
#number of elements to sample as provide by percentage value in function call
count_to_remove<-(deletion_percent/100)*end_num
#generation of random number sequence the length of the designated percentage from the numerical sequence without replacement
rand_sample<-sample(tetra_data_range,count_to_remove,replace=FALSE)
#pulling out the row numbers representing the elements chosen by random number sequence
cut_list<-as.numeric(rownames(set_data[is.element(set_data[,2],rand_sample)&
					is.element(set_data[,1],"Tetra4"),]))
#cutting the chosen elements from the master Tetra4 block
set_data<-set_data[-cut_list,]
#pulling out name of the input file for use in output file
object_name<-deparse(substitute(nastran_file))
#writing output file in Strand7 txt format
write.table(set_data, file=paste(object_name,"_",deletion_percent,"percent_removed",".txt"
		,sep=""),row.names=FALSE,col.names=FALSE,quote=FALSE)
#end tracking of computational time
proc.time()-ptm
}

#examining processed file
data_set2 <- read.table("randomly_deleted_bricks.txt",row.names=NULL,fill=TRUE)
dim(data_set2)

##sample runs
#35.8% bricks randomly removed
random_brick_deletion(Solid_cylinder_5mmx10mm,35.8)

##The file will be save at Documents folder

