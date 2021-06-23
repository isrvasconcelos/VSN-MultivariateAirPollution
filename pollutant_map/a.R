library(ggplot2)

raw_st = read.csv("summary_table.csv")
st_pred = read.csv("st_pred.csv")


timestamps <- 2*nrow(st_pred) # Disregard two first columns

na_pred <- length(which(is.na(raw_st)))
raw_st_qty <- length(which(!is.na(raw_st))) - timestamps
st_pred_qty <- length(which(!is.na(st_pred))) - timestamps - raw_st_qty - na_pred

df <- data.frame(
	group=c("Raw data","2nd Prediction step: Multivariate Cokriging", "1st Prediction step: Merged Gaussians"),
	value=c(raw_st_qty, st_pred_qty, na_pred)
	)

head(df)

#pie(src_data_ratio, c("1","2","3"), main="abc")#, col, clockwise)

#[1] 11229 - 816
#> length(which(!is.na(st_pred)))
#[1] 21624 - 816
#> length(which(is.na(raw_st)))
#[1] 603

blank_theme <- theme_minimal()+
  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold")
  )


bp<- ggplot(df, aes(x="", y=value, fill=group)) + geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)
#pie <- pie + scale_fill_brewer(palette="Blues")+ theme_minimal()

postscript( "src_data_ratio.eps", width=5, height=2 )

	pie + scale_fill_brewer("Amount of samples at each condition:") + blank_theme +
	  theme(axis.text.x=element_blank())+
	  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
		        label = value), size=3)
dev.off()



