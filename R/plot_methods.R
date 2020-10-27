# Copy of cooccur source 

my_cooccur_plot <- function(x, ...)  {

    ##
  allargs <- match.call(expand.dots = TRUE)
  plotrand <- allargs$plotrand
    plotrand <- ifelse(test = is.null(plotrand),yes = FALSE,no = plotrand)
  randsummary<- allargs$randsummary
    randsummary <- ifelse(test = is.null(randsummary),yes = FALSE,no = randsummary)

  ##
  
  dim <- x$species
  comat_pos <- comat_neg <- matrix(nrow=dim,ncol=dim)
  
  co_tab <- x$result
  for (i in 1:nrow(co_tab)){
    comat_pos[co_tab[i,"sp1"],co_tab[i,"sp2"]] <- co_tab[i,"p_gt"]
    comat_pos[co_tab[i,"sp2"],co_tab[i,"sp1"]] <- co_tab[i,"p_gt"]
    
    row.names(comat_pos[co_tab[i,"sp2"],co_tab[i,"sp1"]])
    
  }
  for (i in 1:nrow(co_tab)){
    comat_neg[co_tab[i,"sp1"],co_tab[i,"sp2"]] <- co_tab[i,"p_lt"]
    comat_neg[co_tab[i,"sp2"],co_tab[i,"sp1"]] <- co_tab[i,"p_lt"]
  }
  comat <- ifelse(comat_pos>=0.05,0,1) + ifelse(comat_neg>=0.05,0,-1)
  colnames(comat) <- 1:dim
  row.names(comat) <- 1:dim
  
  if ("spp_key" %in% names(x)){
    
    sp1_name <- merge(x=data.frame(order=1:length(colnames(comat)),sp1=colnames(comat)),y=x$spp_key,by.x="sp1",by.y="num",all.x=T)
    sp2_name <- merge(x=data.frame(order=1:length(row.names(comat)),sp2=row.names(comat)),y=x$spp_key,by.x="sp2",by.y="num",all.x=T)
    
    colnames(comat) <- sp1_name[with(sp1_name,order(order)),"spp"]  
    row.names(comat) <- sp2_name[with(sp2_name,order(order)),"spp"]
      
  }  
  
  #ind <- apply(comat, 1, function(x) all(is.na(x)))
  #comat <- comat[!ind,]
  #ind <- apply(comat, 2, function(x) all(is.na(x)))
  #comat <- comat[,!ind]

  comat[is.na(comat)] <- 0
  
  origN <- nrow(comat)
  
  # SECTION TO REMOVE SPECIES INTERACTION WITH NO OTHERS

  #rmrandomspp <- function(orimat,plotrand = FALSE,randsummary = FALSE){
  if(plotrand == FALSE){
    ind <- apply(comat, 1, function(x) all(x==0))
    comat <- comat[!ind,]    
    ind <- apply(comat, 2, function(x) all(x==0))
    comat <- comat[,!ind]
    #ind <- apply(orimat, 1, function(x) all(x==0))
    #orimat <- orimat[!ind,]    
    #ind <- apply(orimat, 2, function(x) all(x==0))
    #orimat <- orimat[,!ind]
  }
  #return(orimat)
  #}
  
  #comat <- rmrandomspp(orimat = comat, dots)
  ####################################################### 

  postN <- nrow(comat)


  comat <- comat[order(rowSums(comat)),]
  comat <- comat[,order(colSums(comat))]
  
  #comat <- rmrandomspp(orimat = comat, ...)
  
  #ind <- apply(comat, 1, function(x) all(x==0))
  #comat <- comat[!ind,]
  #ind <- apply(comat, 2, function(x) all(x==0))
  #comat <- comat[,!ind]
  
  ind <- apply(comat, 1, function(x) all(x==0))
  comat <- comat[names(sort(ind)),]
  ind <- apply(comat, 2, function(x) all(x==0))
  comat <- comat[,names(sort(ind))]
  
  #comat
  data.m = reshape::melt(comat)
  colnames(data.m) <- c("X1","X2","value")
  data.m$X1 <- as.character(data.m$X1)
  data.m$X2 <- as.character(data.m$X2)
 
  meas <- as.character(unique(data.m$X2))

  dfids <- subset(data.m, X1 == X2)
  
  X1 <- data.m$X1
  X2 <- data.m$X2
  
  df.lower = subset(data.m[lower.tri(comat),],X1 != X2)
  
  ##### testing the rand summary
    if(randsummary == FALSE){  
      }else{
        dim <- nrow(comat)
        ext.dim <- round(dim*0.2,digits = 0)
          if(ext.dim<0){ext.dim<-1}
        placehold <- paste("ext_", rep(c(1:ext.dim),each = dim), sep="")
      
        randcol.df <- data.frame(
          X1 = placehold,
          X2 = rep(meas,times = ext.dim),
          value = rep(x = c(-2), times = dim*ext.dim))
        
        df.lower <- rbind(df.lower,randcol.df)
        meas <- c(meas,unique(placehold))
      }

  


  #####

  X1 <- df.lower$X1
  X2 <- df.lower$X2
  value <- df.lower$value

  p <- ggplot(df.lower,
    aes(X1, X2)) +
  geom_tile(aes(fill = factor(value,levels=c(-1,0,1))), colour ="white") 

p <- p +
  scale_fill_manual(values = c("#FFCC66","dark gray","light blue"), name = "",
    labels = c("negative","random","positive"),drop=FALSE) + 
  theme(axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(vjust=-4,size=20, face="bold"),
    panel.background = element_rect(fill='white', colour='white'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.9, 0.5),
    legend.text=element_text(size=18)) + 
  #ggtitle("Species Co-occurrence Matrix") + 
  xlab("") + ylab("") + 
  scale_x_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) + 
  scale_y_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) 
p <- p + geom_text(data=dfids,aes(label=str_to_title(X1)),hjust=1,vjust=0,angle = -22.5)#, color="dark gray")

 
  return(p)

}

plot_nmds_envfit <- function (
  nmds = NULL,
  envfit = NULL,
  arrow_scale = 3,
  vec_label_dist = .05,
  col_vec_label = "black",
  col_fac_label = "black",
  ...
  ) {

  # Extract site position 
  NMDS = data.frame(MDS1 = nmds$points[,1], MDS2 = nmds$points[,2])
  #vec.sp<-envfit(nmds$points, NMDS.log, perm=1000)

  # Extract vectors and scale eigen values by correlation with ordination: 
  envfit.df<-as.data.frame(envfit$vectors$arrows*sqrt(envfit$vectors$r)* arrow_scale)

  # Replace vector label 
  vectors_replacement <- c("Lat", "Long", "SpringDist", "Alt")
  names(vectors_replacement) <- rownames(envfit.df)
  envfit.df$species<- str_replace_all(rownames(envfit.df), vectors_replacement)

  # Extract factors
  envfit.fac <- as.data.frame(compo_env$factors$centroids)
  # Replace factor label 
  factors_replacement <- c("Acid", "Calc", "Mixed")
  names(factors_replacement) <- rownames(envfit.fac)
  envfit.fac$substrate <- str_replace_all(rownames(envfit.fac), factors_replacement)

  g <- ggplot(data = NMDS, aes(MDS1, MDS2)) +
    geom_point(size = .5) +
    geom_segment(data = envfit.df,
      aes(x = 0, xend = NMDS1 ,y = 0, yend = NMDS2),
      arrow = arrow(length = unit(0.5, "cm")), colour = "blue", size = 1, inherit.aes = FALSE) + 
    geom_text(data = envfit.fac,
      aes(x = NMDS1, y = NMDS2, label = substrate),
      size = 5, color = col_fac_label) +
    geom_text(data = envfit.df, aes(
	x = NMDS1,
	y = ifelse(NMDS2< 0, NMDS2 - vec_label_dist, NMDS2 + vec_label_dist),
	label = species),
      size = 5,
      color = col_vec_label)+
    coord_fixed() +
    labs(x = "NMDS1", y = "NMDS2") +
    theme_bw()
  return(g)

}
