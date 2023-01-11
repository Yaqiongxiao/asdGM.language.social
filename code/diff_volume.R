diff_volume <- function(volume,ylab) {
	
	# violin plot
	a <- ggplot(G_WMV_all,aes_string("Group",volume))+ 
		theme(axis.title = element_blank()) + #delete x,y,title 
		geom_violin(trim = F,width = 1.0) +
		geom_boxplot(width=0.1,fill = c('#77933B','#498399'))+ # add box
		geom_jitter(size = 4,width = 0.2)+ # higher width, more scattered dots
		theme_classic() +# set y/x color as black, set backgroud as white
		theme(legend.position  = c(1.5,0.5),
		      axis.text=element_text(size=18,face = "bold",color = 'black'),
		      axis.title.x = element_blank(),
		      axis.title.y = element_text(size=20,face="bold"),
		      axis.line.x = element_line(size = 1,color = 'black'),
		      axis.line.y = element_line(size = 1,color = 'black'))+
		ylab(ylab)# change y label
	
	print(a)
	
	ggsave(here(paste0("results/global_diff_",volume,".png")), 
	       height = 4, width = 5)
	
	# t-test and cohen's d
	diff_volume <- as.data.frame(matrix(0,1,4))
	colnames(diff_volume) <- c("t value","p value","Cohen's d", "95% CI")
	
	tt <- t.test(G_WMV_all[G_WMV_all$Group =='ASD', volume],
		     G_WMV_all[G_WMV_all$Group =='nonASD', volume])
	
	dd <- effsize::cohen.d(G_WMV_all[G_WMV_all$Group =='ASD', volume],
			       G_WMV_all[G_WMV_all$Group =='nonASD', volume])
	
	diff_volume[,1:4] <- c(round(tt$statistic,2), round(tt$p.value,3),
			       round(dd$estimate,2), paste0("[",round(dd$conf.int[1],2), ", ",
			       			     round(dd$conf.int[2],2),"]"))
	
	knitr::kable(diff_volume)
	
	
	
}