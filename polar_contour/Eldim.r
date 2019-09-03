eldim_png <- function (
	# file_name #filename to process one csv 92 col 360 rows
	){
	# library("ggplot2")
	# library("reshape2")
	
	d <-dir()
	dpng <- paste(d,".png",sep="")
	for(t in 1:length(d)) {
	m=as.matrix(read.table(d[t], sep=","))
	m=m[,3:92]
	s=matrix(nrow=120,ncol=30)
	for(j in 1:30)
		for(i in 1:120)
			s[i,j]=mean(m[(i*3-2):(i*3),(j*3-2):(j*3)])
	# file_name_png <- paste(file_name,".png")
	png(filename = dpng[t])
	ggplot(melt(s))+geom_tile(aes(x=Var1,y=Var2,fill=value))+coord_polar(theta="x")
	dev.off()
	}
}
