
help_console <- function(topic, format=c("text", "html", "latex", "Rd"),
                         lines=NULL, before=NULL, after=NULL) {  
  format=match.arg(format)
  if (!is.character(topic)) topic <- deparse(substitute(topic))
  helpfile = utils:::.getHelpFile(help(topic))
  
  hs <- capture.output(switch(format, 
                              text=tools:::Rd2txt(helpfile),
                              html=tools:::Rd2HTML(helpfile),
                              latex=tools:::Rd2latex(helpfile),
                              Rd=tools:::prepare_Rd(helpfile)
  )
  )
  if(!is.null(lines)) hs <- hs[lines]
  hs <- c(before, hs, after)
  #cat(hs, sep="\n")
  pos = c(grep("_\bD_\be_\bs_\bc_\br_\bi_\bp_\bt_\bi_\bo_\bn:", hs), grep("_\bU_\bs_\ba_\bg_\be:" , hs))
  hs = hs[(pos[1]+1):(pos[2]-1)]
  hs = hs[hs!=""]
  hs = sapply(hs, trim, sihsplify = T)
  hs[2:length(hs)] = paste("", hs[2:length(hs)])
  invisible(hs)
}


graph.metrics = function(g, FUN = closeness){
  require(igraph)
  require(gdata)
  m = help_console(as.character(substitute(FUN)))
  message(m)
  FUN(g)
}
plot.metric = function(FUN = mean_distance){
  require(ggplot2)
  n = seq(10,10000,100)
  metric = sapply(n, function(x){
    g <- barabasi.game(x, power = 1, m=20)
    FUN(g)
  }, simplify = T)
  temp = data.frame(n = n, metric = metric)
  desc = help_console(as.character(substitute(FUN)))
  p = ggplot(temp, aes(x = n, y = metric)) + labs(title = as.character(substitute(FUN)), subtitle = desc) +
                                                    geom_point()
  p
}
plot.metric(diameter)

library(igraph)
n=1000
g <- barabasi.game(n, power = 1, m=20)

graph.metrics(g, FUN = transitivity)
graph.metrics(g, FUN = mean_distance)
graph.metrics(g, FUN = degree)
graph.metrics(g, FUN = reciprocity)

graph.metrics(g, FUN = diameter)
graph.metrics(g, FUN = degree_distribution)
graph.metrics(g, FUN = centr_degree)
graph.metrics(g, FUN = closeness)
mean(graph.metrics(g, FUN = betweenness))
graph.metrics(g, FUN = hub_score)
graph.metrics(g, FUN = authority_score)



