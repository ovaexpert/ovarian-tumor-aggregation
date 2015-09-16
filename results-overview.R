library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(gtable)
library(plyr)
library(dplyr)
library(reshape2)


LIM.X = c(0, 0.5)
BREAKS.X = seq(0, 0.5, 0.1)

MARGINS.REALDATA.LEFT  = unit(c(0, 0.1, 0.07, 0), "npc")
MARGINS.REALDATA.RIGHT = unit(c(0, 0.02, 0.07, 0.07), "npc")
LEGEND.SPACING = 5


plotStatsTrainingSet = function(stats, performanceMeasure)
{
    shapes.ids = c(21,22,23,24,25,21,22,23)

    ymax.val = 1.1*max(subset(stats, Class=="Model" & Measure==performanceMeasure)$Value)

    stats$Method = gsub("orig. ", "", stats$Method)
    stats$Method = gsub("unc. ",  "", stats$Method)

    a = ggplot(data=subset(stats, Class=="Model" & Subclass=="Original" & Measure==performanceMeasure),
               aes(x=ObscureLevel, y=Value, group=Method, colour=Method, shape=Method, fill=Method)) +
        geom_line() +
        geom_point(size=3, color="black") +
        ggtitle("Original models") +
        scale_shape_manual(values=shapes.ids) +
        scale_color_manual(values=c("#D73027","#FC8D59","#FCBE23","#77D9F4","#91BFDB","#91BFDB")) +
        scale_fill_brewer(palette = "RdYlBu") +
        xlab("Level of missing data") +
        ylab(ifelse(performanceMeasure=="Cost matrix", "Total cost", performanceMeasure)) +
        theme_bw() +
        theme(legend.position="bottom",
              plot.margin=unit(c(0.02, 0.05, 0.00, 0.05), "npc"),
              legend.margin=unit(0.05, "npc"),
              panel.margin = unit(0.07, "npc"),
              axis.text.x=element_text(size=9),
              strip.text.x=element_text(size=9),
              axis.title.x = element_text(vjust=-0.5),
              axis.title.y = element_text(vjust=1.5),
              title = element_text(vjust=1.2)) +
        coord_cartesian(xlim=LIM.X, ylim=c(50, ymax.val)) +
        scale_x_continuous(breaks=BREAKS.X)

    b = ggplot(data=subset(stats, Class=="Model" & Subclass=="Uncertaintified" &
                               Measure==performanceMeasure),
               aes(x=ObscureLevel, y=Value, group=Method, colour=Method, shape=Method, fill=Method)) +
        geom_line() +
        geom_point(size=3, color="black") +
        ggtitle("Uncertaintified models") +
        scale_shape_manual(values=shapes.ids) +
        scale_color_manual(values=c("#D73027","#FC8D59","#FCBE23","#77D9F4","#91BFDB","#91BFDB")) +
        scale_fill_brewer(palette = "RdYlBu") +
        xlab("Level of missing data") +
        ylab(ifelse(performanceMeasure=="Cost matrix", "Total cost", performanceMeasure)) +
        theme_bw() +
        theme(legend.position="bottom",
              plot.margin=unit(c(0.02, 0.05, 0.00, 0.05), "npc"),
              legend.margin=unit(0.05, "npc"),
              panel.margin = unit(0.07, "npc"),
              axis.text.x=element_text(size=9),
              strip.text.x=element_text(size=9),
              axis.title.x = element_text(vjust=-0.5),
              axis.title.y = element_text(vjust=1.5),
              title = element_text(vjust=1.2)) +
        coord_cartesian(xlim=LIM.X, ylim=c(50, ymax.val)) +
        scale_x_continuous(breaks=BREAKS.X)


    c = ggplot(data=subset(stats, Class=="Aggregation" & Measure==performanceMeasure),
               aes(x=ObscureLevel, y=Value, group=Method, colour=Method)) +
        geom_line(colour="black", alpha=0.5) +
        facet_grid(Class ~ Subclass + Subsubclass) +
        ggtitle("Top 5 aggregation operators") +
        xlab("Level of missing data") +
        ylab(ifelse(performanceMeasure=="Cost matrix", "Total cost", performanceMeasure)) +
        theme_bw() +
        theme(legend.position="none",
              plot.margin=unit(c(0.05, 0.01, 0, 0.02), "npc"),
              panel.margin = unit(0.02, "npc"),
              axis.text.x=element_text(size=9),
              axis.title.x = element_text(vjust=-0.5),
              axis.title.y = element_text(vjust=1.5),
              title = element_text(vjust=1.2)) +
        coord_cartesian(xlim=LIM.X, ylim=c(0, ymax.val)) +
        scale_x_continuous(breaks=BREAKS.X) +
        scale_color_manual(values=colorRampPalette(brewer.pal(9, "Paired"))(length(unique(stats$Method))))

    grid.arrange(arrangeGrob(a, b, nrow=1),
                 arrangeGrob(c, nrow=1),
                 nrow=2, heights=unit(c(0.45, 0.45), "npc"))

}


plotStatsTestSet = function(stats, performanceMeasure,
                             performanceMeasureDescending, secondaryPerformanceMeasures)
{
    PERF.COLORS = c("#D3DDE2","#B2DF8A","#33A02C","#1B6699")

    stats$Measure = factor(stats$Measure,
                           levels=c(PERFORMANCE.MEASURE, "Accuracy",
                                    "Sensitivity", "Specificity", "Decisiveness"))

    orig.models.perf.measure = subset(stats, Measure==performanceMeasure & Class=="Model" & Subclass=="Original")
    orig.models.other.measures = filter(stats, Method %in% orig.models.perf.measure$Method &
                                            Measure %in% secondaryPerformanceMeasures)

    orig.models.perf.measure$Method = gsub("orig. ", "", orig.models.perf.measure$Method)
    orig.models.perf.measure$Method = factor(orig.models.perf.measure$Method,
                                             levels=rev(levels(as.factor(orig.models.perf.measure$Method))))

    orig.models.other.measures$Method = gsub("orig. ", "", orig.models.other.measures$Method)
    orig.models.other.measures$Measure = revalue(as.factor(orig.models.other.measures$Measure),
                                                 sapply(secondaryPerformanceMeasures,
                                                        paste0, # hack to add spacing
                                                        paste(rep(" ", LEGEND.SPACING),
                                                              collapse="")))


    a0 = ggplot(data=orig.models.perf.measure, aes(x=Method, weight=Value, fill=Method),
                environment=environment()) +
        geom_bar(color="black") +
        scale_fill_brewer(palette = "RdYlBu") +
        xlab("Original model") +
        ylab(ifelse(performanceMeasure=="Cost matrix", "Total cost", performanceMeasure)) +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              legend.position="none",
              plot.margin=MARGINS.REALDATA.LEFT) +
        geom_text(aes(x=Method,y=Value+5, label=round(Value, 1)),
                  hjust=0, vjust=0.5, size=3) +
        geom_text(aes(x=Method, y=0.05*max(Value), label=Method),
                  hjust=0, vjust=0.5, size=4) +
        coord_cartesian() + coord_flip(ylim=c(0, 1.2*max(orig.models.perf.measure$Value)))

    b0 = ggplot(data=orig.models.other.measures, aes(x=Method, weight=Value, fill=Measure)) +
        geom_bar(position=position_dodge(width = 0.9), width=0.9) +
        geom_bar(position=position_dodge(width = 0.9), width=0.9, color="black", show_guide=FALSE) +
        scale_fill_manual(values=PERF.COLORS, name="Performance measure") +
        coord_cartesian(ylim=c(0.0, 1.1)) +
        xlab("Original model") +
        ylab("Value") +
        theme_bw() +
        theme(legend.position="top",
              plot.margin=MARGINS.REALDATA.RIGHT) +
        geom_text(aes(y=Value+0.01, label=round(Value, 2), ymax=1),
                  position=position_dodge(width=0.9), vjust=0, size=2.8)

    models.perf.measure = subset(stats, Measure==performanceMeasure &
                                        Class=="Model" & Subclass=="Uncertaintified")

    models.other.measures = filter(stats, Method %in% models.perf.measure$Method &
                                          Measure %in% secondaryPerformanceMeasures)

    models.perf.measure$Method = gsub("unc. ", "", models.perf.measure$Method)
    models.perf.measure$Method = factor(models.perf.measure$Method,
                                        levels=rev(levels(as.factor(models.perf.measure$Method))))

    models.other.measures$Method = gsub("unc. ", "", models.other.measures$Method)
    models.other.measures$Measure = revalue(as.factor(models.other.measures$Measure),
                                            sapply(secondaryPerformanceMeasures,
                                                   paste0, # hack to add spacing
                                                   paste(rep(" ", LEGEND.SPACING),
                                                         collapse="")))

    a = ggplot(data=models.perf.measure, aes(x=Method, weight=Value, fill=Method),
               environment=environment()) +
        geom_bar(color="black") +
        scale_fill_brewer(palette = "RdYlBu") +
        xlab("Uncertaintified model") +
        ylab(ifelse(performanceMeasure=="Cost matrix", "Total cost", performanceMeasure)) +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              legend.position="none",
              plot.margin=MARGINS.REALDATA.LEFT) +
        geom_text(aes(x=Method,y=Value+5, label=round(Value, 1)),
                  hjust=0, vjust=0.5, size=3) +
        geom_text(aes(x=Method, y=0.05*max(orig.models.perf.measure$Value), label=Method),
                  hjust=0, vjust=0.5, size=4) +
        coord_cartesian() + coord_flip(ylim=c(0, 1.2*max(orig.models.perf.measure$Value)))

    b = ggplot(data=models.other.measures, aes(x=Method, weight=Value, fill=Measure)) +
        geom_bar(position=position_dodge(width = 0.9), width=0.9) +
        geom_bar(position=position_dodge(width = 0.9), width=0.9, color="black", show_guide=FALSE) +
        scale_fill_manual(values=PERF.COLORS, name="Performance measure") +
        coord_cartesian(ylim=c(0.0, 1.1)) +
        xlab("Uncertaintified model") +
        ylab("Value") +
        theme_bw() +
        theme(legend.position="none",
              plot.margin=MARGINS.REALDATA.RIGHT) +
        geom_text(aes(y=Value+0.01, label=round(Value, 2), ymax=1),
                  position=position_dodge(width=0.9), vjust=0, size=2.8)

    aggrs.perf.measure = subset(stats, Measure==performanceMeasure & Class=="Aggregation") %>%
        group_by(Class, Subclass, Subsubclass)

    if (performanceMeasureDescending) {
        aggrs.perf.measure = slice(aggrs.perf.measure, which.max(Value))
    } else {
        aggrs.perf.measure = slice(aggrs.perf.measure, which.min(Value)) }

    aggrs.other.measures = filter(stats, Method %in% aggrs.perf.measure$Method &
                                      Measure %in% secondaryPerformanceMeasures)

    c = ggplot(data=aggrs.perf.measure,
               aes(x=factor(paste(Subclass, Subsubclass),
                            levels=rev(levels(factor(paste(Subclass, Subsubclass))))),
                   weight=Value,
                   fill=paste(Subclass, Subsubclass)), environment=environment()) +
        geom_bar(width = 0.8, position = position_identity(width=0.8), color="black") +
        scale_fill_manual(values=c("#FE9929","#1D91C0","#42AB5D","#EF3B2C","#7A1977","#53278F","#EF6547","#ADDD8E")) +
        xlab("Aggregation group by the lowest cost") +
        ylab(ifelse(performanceMeasure=="Cost matrix", "Total cost", performanceMeasure)) +
        theme_bw() +
        theme(axis.text.y = element_blank(),
              legend.position="none",
              plot.margin=MARGINS.REALDATA.LEFT) +
        geom_text(aes(x=paste(Subclass, Subsubclass), y=Value+5,
                      label=round(Value, 1)),
                  hjust=0, vjust=0.5, size=3) +
        geom_text(aes(x=paste(Subclass, Subsubclass), y=1.15*max(orig.models.perf.measure$Value),
                      label=paste(Subclass, Subsubclass)),
                  hjust=1, vjust=0.5, size=3) +
        coord_cartesian() +
        coord_flip(ylim=c(0, 1.2*max(orig.models.perf.measure$Value)))

    d = ggplot(data=aggrs.other.measures, aes(x=Method, weight=Value, fill=Measure)) +
        geom_bar(position=position_dodge(width = 0.9), width=0.9) +
        geom_bar(position=position_dodge(width = 0.9), width=0.9, color="black", show_guide=FALSE) +
        scale_fill_manual(values=PERF.COLORS, name="Performance measure") +
        coord_cartesian(ylim=c(0.0, 1.1)) +
        xlab("Aggregation strategy") +
        ylab("Value") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              legend.position="none",
              plot.margin=MARGINS.REALDATA.RIGHT,
              strip.text.x=element_text(size=8.35)) +
        geom_text(aes(y=Value+0.01, label=round(Value, 2), ymax=1),
                  position=position_dodge(width=0.9), vjust=0, size=2.7) +
        facet_grid(. ~ Subclass + Subsubclass, scales = "free_x")


    g_legend<-function(a.gplot){
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)}
    mylegend<-g_legend(b0)

    gwidths = c(0.3, 1)

    grid.arrange(arrangeGrob(nullGrob(), mylegend, nrow=1, widths=gwidths),
                 arrangeGrob(a0, b0 + theme(legend.position="none"), nrow=1, widths=gwidths),
                 arrangeGrob(a, b, nrow=1, widths=gwidths),
                 arrangeGrob(c, d, nrow=1, widths=gwidths),
                 nrow=4,heights=unit(c(0.12, 0.24,0.24,0.4), "npc"))
}

plotSelectedAggregationOperators = function(stats, performanceMeasure,
                                            secondaryPerformanceMeasures)
{
    stats = melt(stats, id.vars=c("Method", "Class", "Subclass", "Subsubclass")) %>%
        rename(Measure=variable, Value=value)

    stats = transform(stats, Subsubclass=as.character(Subsubclass))

    PERF.COLORS = c("#D3DDE2","#B2DF8A","#33A02C","#1B6699")

    stats$Measure = factor(stats$Measure,
                           levels=c(PERFORMANCE.MEASURE, "Accuracy",
                                    "Sensitivity", "Specificity", "Decisiveness"))

    stats.perf.measure = subset(stats, Measure==performanceMeasure)
    stats.other.measures = subset(stats, Measure %in% secondaryPerformanceMeasures)


    f = function(x)
    {
        ggplot(data=x, aes(x=Method, weight=Value,
                           fill=Method), environment=environment()) +
            geom_bar(color="black", width=0.5, position=position_dodge(width = 0.5)) +
            scale_fill_manual(values = x$Color) +
            facet_grid(Subsubclass + Subclass~.) +
            theme_bw() +
            theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  legend.position="none",
                  plot.margin=unit(c(0,0.1,0,0), "npc"),
                  strip.text.y=element_text(size=9),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank()) +
            geom_text(aes(x=Method, y=0.01*max(stats.perf.measure$Value),
                          label=Method),
                      hjust=0, vjust=0.5, size=2.8) +
            geom_text(aes(x=Method, y=Value+2,
                          label=round(Value, 1)),
                      hjust=0, vjust=0.5, size=3) +
            coord_flip(ylim=c(0, 1.2*max(stats.perf.measure$Value)))
    }

    grouped.stats.perf.measure = stats.perf.measure %>%
        mutate(Color = substr(rainbow(nrow(stats.perf.measure)), 1, 7)) %>%
        group_by(Subclass, Subsubclass)

    df = grouped.stats.perf.measure %>%
        do(plots=f(.))

    df$plots[length(df$plots)][[1]] = df$plots[length(df$plots)][[1]] +
        theme(axis.text.x=element_text(size=10))

    stats.other.measures$Measure = revalue(as.factor(stats.other.measures$Measure),
                                           sapply(secondaryPerformanceMeasures,
                                                  paste0, # hack to add spacing
                                                  paste(rep(" ", LEGEND.SPACING),
                                                        collapse="")))

    d = ggplot(data=stats.other.measures, aes(x=Method, weight=Value, fill=Measure)) +
        geom_bar(position=position_dodge(width = 0.9), width=0.9) +
        geom_bar(position=position_dodge(width = 0.9), width=0.9, color="black", show_guide=FALSE) +
        scale_fill_manual(values=PERF.COLORS, name="Performance measure    ") +
        coord_cartesian(ylim=c(0.0, 1.1)) +
        xlab("Aggregation strategy") +
        ylab("Value") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              legend.position="top",
              plot.margin=unit(c(0.1, 0.02, 0.1, 0.06), "npc"),
              strip.text.x=element_text(size=9)) +
        geom_text(aes(y=Value+0.01, label=round(Value, 2), ymax=1),
                  position=position_dodge(width=0.9), vjust=0, size=2.8) +
        facet_grid(.~Subclass+Subsubclass, scales="free", space="free")

    grid.newpage()


    pheights = c((0.88*as.data.frame(summarise(grouped.stats.perf.measure,
                                               length(Method)))[,3]+
                      c(rep(0, length(as.data.frame(summarise(grouped.stats.perf.measure,
                                                              length(Method)))[,3])-1),0.1))/(nrow(stats.perf.measure)),
                 0.04)

    pushViewport(viewport(layout = grid.layout(length(df$plots)+1, 3,
                                               heights = unit(pheights, "npc"),
                                               widths = unit(c(0.022, 0.328, 0.65), "npc"))))

    grid.text("Aggregation strategy", vp = viewport(layout.pos.row = 1:(length(df$plots)),
                                                    layout.pos.col = 1),
              gp=gpar(fontsize=12), rot=90)

    vplayout = function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

    for (i in 1:(length(df$plots)))
        print(df$plots[[i]], vp = vplayout(i, 2))

    grid.text(ifelse(performanceMeasure=="Cost matrix", "Total cost", performanceMeasure),
              vp = viewport(layout.pos.row = length(df$plots)+1, layout.pos.col = 2),
              gp=gpar(fontsize=12), x=unit(c(0.4), "npc"), y=unit(c(0.8), "npc"))

    print(d, vp = vplayout(1:(length(df$plots)+1), 3))

}