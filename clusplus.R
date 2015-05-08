function(cluster_model, data)
{
  ## Add mean for all rows.
  all_cluster_results=round(as.data.frame(rbind(cluster_model$centers, colMeans(data))),2)
  
  
  ######################################################################
  ##  Cluster profiling. Extracting main characteristics from each one.
  ######################################################################
  
  ## Scale data to plot all in only one graph
  maxs <- apply(all_cluster_results, 2, max)
  mins <- apply(all_cluster_results, 2, min)
  cl_scaled=as.data.frame(scale(all_cluster_results, center = mins, scale = maxs - mins))
  
  ## Assign cluster number (label)
  cl_scaled$cluster=c(paste("C",1:max(cluster_model$cluster), sep="_"),"All")
  
  ## This transform the data according to needed input of ggplot. The best way to understand this is to take a look at the data.
  melted_data=melt(cl_scaled, id.vars = "cluster")
  
  ## Coordinate plot
  coor_plot=ggplot(melted_data, aes(x=variable, y=value,  group=cluster, color=cluster),  environment = environment()) +  
    geom_path(alpha = 0.9) +
    geom_point() + 
    xlab("Variables") +
    ylab("Incidence percentage") + 
    ggtitle("Coordinate Plot") +
    theme(axis.text.x=element_text(angle = 90, vjust = 0.5), plot.title=element_text(size=14,face="bold"))
  
  plot(coor_plot)
}