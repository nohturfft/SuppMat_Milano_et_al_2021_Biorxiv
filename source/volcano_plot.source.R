## Functions to draw volcano plots

# https://www.rapidtables.com/web/color/brown-color.html

my.volcano.plot <-
  function(dafra,
           colors = c("green", "gray", "blue", "darkred"),
           highlight.colors = c("#cc1d6f", RColorBrewer::brewer.pal(9, "Set1")),
           dot.sizes = c(1, 1, 2, 2),
           alphas = NULL,
           rug.size=1,
           title = NULL,
           subtitle = NULL,
           highlight = NULL,
           rugs=FALSE,
           cutoff.fc = NULL,
           cutoff.adj.p = NULL,
           x.max = NULL,
           y.max = NULL,
           bar.height=0.4,
           gap.height=0.1,
           y.intercept=-0.2,
           y.position="left",
           font.size=10) {
    
    stopifnot("data.frame" %in% class(tt$wt.mms.vs.wt.ctrl))
    stopifnot(nrow(dafra) > 0)

    df.barcodes <- lapply(seq_along(highlight), function(i, dafra, highlight) {
      lib.set <- highlight[[i]]
      g <- lib.set$genes

      # Not highlighted, not significant:
      bar.1 <- dafra %>%
        dplyr::filter(toupper(SYMBOL) %in% toupper(g)) %>%
        dplyr::filter(abs(logFC) < cutoff.fc & adj.P.Val > cutoff.adj.p) %>%
        dplyr::mutate(significant=FALSE) %>%
        dplyr::mutate(i=i) %>%
        dplyr::mutate(y=(-bar.height*i) + y.intercept) %>%
        dplyr::mutate(label=lib.set$name) %>%
        dplyr::mutate(highlight.color=highlight.colors[i])

      # Highlighted, significant:
      bar.2 <- dafra %>%
        dplyr::filter(toupper(SYMBOL) %in% toupper(g)) %>%
        dplyr::filter(abs(logFC) >= cutoff.fc & adj.P.Val <= cutoff.adj.p) %>%
        dplyr::mutate(significant=TRUE) %>%
        dplyr::mutate(i=i) %>%
        dplyr::mutate(y=(-bar.height*i) + y.intercept) %>%
        dplyr::mutate(label=lib.set$name) %>%
        dplyr::mutate(highlight.color=highlight.colors[i])
      bar.12 <- rbind(bar.1, bar.2)
      bar.12
    }, dafra=dafra, highlight=highlight) %>%
      data.table::rbindlist(.) %>% as.data.frame
    

    # Highlight Xbp1 ChIP-Seq targets
    #--------------------------------
    # Not highlighted, not significant:
    df.1 <- dafra %>%
      dplyr::filter(!toupper(SYMBOL) %in% toupper(highlight[[1]]$genes)) %>%
      dplyr::filter(abs(logFC) < cutoff.fc | adj.P.Val > cutoff.adj.p)
    on.exit(print(paste0(highlight[[1]]$name, ". Not highlighted, not significant: ", nrow(df.1))), add=TRUE)
    
    # Not highlighted, significant:
    df.2 <- dafra %>%
      dplyr::filter(!toupper(SYMBOL) %in% toupper(highlight[[1]]$genes)) %>%
      dplyr::filter(abs(logFC) >= cutoff.fc & adj.P.Val <= cutoff.adj.p)
    on.exit(print(paste0(highlight[[1]]$name, ". Not highlighted, significant: ", nrow(df.2))), add=TRUE)
    
    # Highlighted, not significant:
    df.3 <- dafra %>%
      dplyr::filter(toupper(SYMBOL) %in% toupper(highlight[[1]]$genes)) %>%
      dplyr::filter(abs(logFC) < cutoff.fc | adj.P.Val > cutoff.adj.p)
    on.exit(print(paste0(highlight[[1]]$name, ". Highlighted, not significant: ", nrow(df.3))), add=TRUE)
    
    # Highlighted, significant:
    df.4 <- dafra %>%
      dplyr::filter(toupper(SYMBOL) %in% toupper(highlight[[1]]$genes)) %>%
      dplyr::filter(abs(logFC) >= cutoff.fc & adj.P.Val <= cutoff.adj.p)
    on.exit(print(paste0(highlight[[1]]$name, ". Highlighted, significant: ", nrow(df.4))), add=TRUE)

    df.rows <- sapply(list(df.1, df.2, df.3, df.4), nrow)
    if (!all(df.rows > 0 )) {
      stop("Missing data. Something wrong.")
    }
    
    if (is.null(x.max)) x.max <- max(abs(dafra$logFC))
    limits.x <- c(-ceiling(x.max), ceiling(x.max))
    breaks.x <- seq(-ceiling(x.max), ceiling(x.max), by=ceiling(x.max)/2)
    
    gg.volcano <- ggplot() +
      # Not highlighted, not significant:
      geom_point(
        data = df.1,
        aes(x = logFC, y = -log10(adj.P.Val)),
        size = dot.sizes[1],
        color=colors[1],
        alpha = alphas[1]
      ) +
      # Not highlighted, significant:
      geom_point(
        data = df.2,
        aes(x = logFC, y = -log10(adj.P.Val)),
        size = dot.sizes[2],
        color=colors[2],
        alpha = alphas[2]
      ) +
      # Highlighted, not significant:
      geom_point(
        data = df.3,
        aes(x = logFC, y = -log10(adj.P.Val)),
        size = dot.sizes[3],
        color=colors[3],
        alpha = alphas[3]
      ) +
      # Highlighted, significant:
      geom_point(
        data = df.4,
        aes(x = logFC, y = -log10(adj.P.Val)),
        size = dot.sizes[4],
        color=colors[4],
        alpha = alphas[4]
      ) +
      # X-Axis:
      geom_hline(yintercept = y.intercept, colour="black") +
      # Horizontal line at p.cutoff:
      geom_hline(
        yintercept = -log10(cutoff.adj.p),
        colour = "gray20",
        linetype = "dashed"
      ) +
      scale_x_continuous(limits=limits.x, breaks = breaks.x) +
      theme_bw(base_size=font.size) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=font.size, face="bold"),
            axis.text.x = element_text(size=font.size, face="bold"),
            axis.text.y = element_text(size=font.size, face="bold"),
            axis.title.x = element_text(size=font.size, face="bold"),
            axis.title.y = element_text(size=font.size, face="bold"),
            plot.title = element_text(size=font.size, face="bold"),
            legend.position="none") +
      labs(title = title, subtitle = subtitle)

    # BARCODE PLOTS
    gg.volcano <- gg.volcano +
      geom_segment(data=subset(df.barcodes, significant==FALSE),
                   aes(x=logFC, xend=logFC, y=y, yend=y+(bar.height-gap.height)), size = 0.7, color=colors[3], alpha=alphas[3]) +
      geom_segment(data=subset(df.barcodes, significant==TRUE),
                   aes(x=logFC, xend=logFC, y=y, yend=y+(bar.height-gap.height)), size = 0.7, color=colors[4], alpha=alphas[4])

    # BARCODE LABELS:
    # browser()
    x <- subset(df.barcodes, significant==FALSE)
    x <- x[!duplicated(x$label),]
    gg.volcano <- gg.volcano +
      geom_text(data=x,
                aes(x=-ceiling(x.max), y=y, label=label),
                hjust="left", vjust="bottom", fontface="bold", size=font.size * 0.3,
                color="black")

    # Y-Axis:
    if (!is.null(y.max)) {
      gg.volcano <- gg.volcano +
        scale_y_continuous(limits = c(min(df.barcodes$y)-gap.height, y.max), position = y.position)
    } else {
      scale_y_continuous(position = y.position)
    }
    gg.volcano <- gg.volcano + geom_vline(xintercept = 0, colour="black")

    gg.volcano
  }
