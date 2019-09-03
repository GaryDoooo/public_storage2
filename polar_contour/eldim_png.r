eldim_png <- function (
	file_name #filename to process one csv 92 col 360 rows
	){
	# library("ggplot2")
	# library("reshape2")

	m=as.matrix(read.table(file_name, sep=","))
	m=m[,3:92]
	s=matrix(nrow=120,ncol=30)
	for(j in 1:30)
		for(i in 1:120)
			s[i,j]=mean(m[(i*3-2):(i*3),(j*3-2):(j*3)])
	# file_name_png <- paste(file_name,".png")
	# png(filename = file_name_png)
	ggplot(melt(s))+geom_tile(aes(x=Var1,y=Var2,fill=value))+coord_polar(theta="x")
	# dev.off()
}
