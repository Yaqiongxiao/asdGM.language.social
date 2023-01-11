pplot <- function(subtest, gmv,xlabel) {
	
	p <- ggplot(G_WMV[G_WMV$Group =="nonASD",],aes_string(x=subtest,y=gmv,xmin = 40, xmax = 90)) +
		geom_point(aes(color=Group), size = 6) +
		geom_smooth(method = lm, se = F, size = 2,col = 'black',fullrange = T, aes(color = Group))+ 
		theme_classic()+
		xlab(xlabel)+ ylab('Mean GMV') + 
		guides(colour = "none") +
		coord_cartesian(ylim=c(0.2,0.7)) + 
		
		
		theme(axis.text=element_text(size=18,face = "bold"),
		      axis.title.x = element_text(size=25,face="bold"),
		      axis.title.y = element_text(size=25,face="bold"),
		      axis.line.x = element_line(size = 1),
		      axis.line.y = element_line(size = 1))
	
	print(p)
	ggsave(here(paste0("results/",'GMV_', gmv,'.png')),width = 7,height = 6)
}
