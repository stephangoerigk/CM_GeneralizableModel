CTQ = pf[, c("ID", 
             "CTQ_EmotionalAbuse",
             "CTQ_PhysicalAbuse",
             "CTQ_SexualAbuse",        
             "CTQ_EmotionalNeglect",
             "CTQ_PhysicalNeglect")]

CTQ = na.omit(CTQ)

dt = dist(na.omit(CTQ[,2:5]), method = "euclidean")
clust = hclust(dt, method = "ward.D2")
dend <- as.dendrogram(clust, hang = -1)
pruned = dynamicTreeCut::cutreeDynamic(clust, distM = as.matrix(dt), method = "hybrid", minClusterSize = 1)
table(pruned)

CTQ$pruned = factor(pruned)

levels(CTQ$pruned) = paste("Type", levels(CTQ$pruned))

radar_traumatype = ggiraphExtra::ggRadar(data = CTQ[,c("CTQ_PhysicalNeglect", "CTQ_PhysicalAbuse", "CTQ_SexualAbuse", "CTQ_EmotionalAbuse", "CTQ_EmotionalNeglect", "pruned")], aes(colour = pruned, fill = pruned), rescale = F, use.label = T, ylim = c(5,25), axis.label.size = 3, grid.label.size = 3, size = 1) + 
  labs(colour = "") +
  scale_color_manual(values = c("Type 1" = col_blue,
                                "Type 2" = col_orange,
                                "Type 3" = "red",
                                "Type 4" = "purple",
                                "Type 5" = col_green,
                                "Type 7" = "grey")) +
  scale_fill_manual(values = c("Type 1" = col_blue,
                               "Type 2" = col_orange,
                               "Type 3" = "red",
                               "Type 4" = "purple",
                               "Type 5" = col_green,
                               "Type 7" = "grey")) +
  theme_bw() +
  facet_grid(rows = vars(pruned)) +
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25), labels = c("5", "", "15", "", "25")) +
  theme(legend.position = "none", axis.text.x = element_blank()) 