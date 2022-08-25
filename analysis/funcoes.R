#' A box and whiskers plot (in the style of Tukey)
#'
#' The boxplot compactly displays the distribution of a continuous variable.
#' It visualises five summary statistics (the median, two hinges
#' and two whiskers), and all "outlying" points individually.
#'
#' @inheritParams ggplot2::geom_boxplot
#' @inheritSection ggplot2::geom_boxplot Summary statistics
#' @param width.errorbar the width of errorbar (default 0.7)
#' 
#' @eval ggplot2:::rd_aesthetics("geom", "boxplot")
#' @seealso [geom_quantile()] for continuous `x`,
#'   [geom_violin()] for a richer display of the distribution, and
#'   [geom_jitter()] for a useful technique for small data.
#' 
#' @references
#' 1. McGill, R., Tukey, J. W. and Larsen, W. A. (1978) Variations of box plots.
#'    The American Statistician 32, 12-16.
#' @example R/examples/ex-geom_boxplot2.R
#' @import ggplot2
#' @importFrom grid grobTree
#' @export
geom_boxplot2 <- function(mapping = NULL, data = NULL,
                          stat = "boxplot", position = "dodge2",
                          ...,
                          outlier.colour = NULL,
                          outlier.color = NULL,
                          outlier.fill = NULL,
                          outlier.shape = 19,
                          outlier.size = 1.5,
                          outlier.stroke = 0.5,
                          outlier.alpha = NULL,
                          show.errorbar = TRUE,
                          width.errorbar = 0.7,
                          notch = FALSE,
                          notchwidth = 0.5,
                          varwidth = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      warning("Can't preserve total widths when varwidth = TRUE.", call. = FALSE)
      position$preserve <- "single"
    }
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      show.errorbar = show.errorbar,
      width.errorbar = width.errorbar,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      ...
    )
  )
}

# ' @format NULL
# ' @usage NULL
#' @export
GeomBoxplot2 <- ggproto("GeomBoxplot2", Geom,

  # need to declare `width`` here in case this geom is used with a stat that
  # doesn't have a `width` parameter (e.g., `stat_identity`).
  extra_params = c("na.rm", "width"),
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    data$outliers <- NULL
    if (!is.null(data$outliers)) {
      suppressWarnings({
        out_min <- vapply(data$outliers, min, numeric(1))
        out_max <- vapply(data$outliers, max, numeric(1))
      })

      data$ymin_final <- pmin(out_min, data$ymin)
      data$ymax_final <- pmax(out_max, data$ymax)
    }

    # if `varwidth` not requested or not available, don't use it
    if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
      data$xmin <- data$x - data$width / 2
      data$xmax <- data$x + data$width / 2
    } else {
      # make `relvarwidth` relative to the size of the largest group
      data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
      data$xmin <- data$x - data$relvarwidth * data$width / 2
      data$xmax <- data$x + data$relvarwidth * data$width / 2
    }
    # data$width <- NULL
    if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL
    data
  },
  draw_group = function(data, panel_params, coord, fatten = 2,
                        outlier.colour = NULL, outlier.fill = NULL,
                        outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5,
                        outlier.alpha = NULL,
                        show.errorbar = TRUE,
                        width.errorbar = 0.7,
                        notch = FALSE, notchwidth = 0.5, varwidth = FALSE) {
    common <- list(
      colour   = data$colour,
      size     = data$size,
      linetype = data$linetype,
      fill     = alpha(data$fill, data$alpha),
      group    = data$group
    )

    whiskers <- new_data_frame(c(
      list(
        x     = c(data$x, data$x),
        xend  = c(data$x, data$x),
        y     = c(data$upper, data$lower),
        yend  = c(data$ymax, data$ymin),
        alpha = c(NA_real_, NA_real_)
      ),
      common
    ), n = 2)

    box <- new_data_frame(c(
      list(
        xmin = data$xmin,
        xmax = data$xmax,
        ymin = data$lower,
        y = data$middle,
        ymax = data$upper,
        ynotchlower = ifelse(notch, data$notchlower, NA),
        ynotchupper = ifelse(notch, data$notchupper, NA),
        notchwidth = notchwidth,
        alpha = data$alpha
      ),
      common
    ))

    errorbar <- new_data_frame(c(
      list(
        xmin = data$x - width.errorbar / 2,
        xmax = data$x + width.errorbar / 2,
        x = data$x,
        ymin = data$ymin,
        ymax = data$ymax,
        alpha = data$alpha
      ),
      common
    ))

    grob_whiskers <- GeomSegment$draw_panel(whiskers, panel_params, coord)
    grob_errorbar <- NULL

    if (show.errorbar) {
      grob_errorbar <- GeomErrorbar$draw_panel(errorbar, panel_params, coord)
    }
    # if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
    #     outliers <- new_data_frame(
    #         y = data$outliers[[1]],
    #         x = data$x[1],
    #         colour = outlier.colour %||% data$colour[1],
    #         fill = outlier.fill %||% data$fill[1],
    #         shape = outlier.shape %||% data$shape[1],
    #         size = outlier.size %||% data$size[1],
    #         stroke = outlier.stroke %||% data$stroke[1],
    #         fill = NA,
    #         alpha = outlier.alpha %||% data$alpha[1],
    #         stringsAsFactors = FALSE
    #     )
    #     outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    # } else {
    #     outliers_grob <- NULL
    # }
    ggplot2:::ggname("geom_boxplot2", grobTree(
      # outliers_grob,
      grob_errorbar,
      # grob_whiskers,
      GeomCrossbar$draw_panel(box, fatten = fatten, panel_params, coord)
    ))
  },
  draw_key = draw_key_boxplot,
  default_aes = aes(
    weight = 1, colour = "grey20", fill = "white", size = 0.5,
    alpha = NA, shape = 19, linetype = "solid"
  ),
  required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax")
)

# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) stop("Elements must be named", call. = FALSE)
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) stop("Elements must equal the number of rows or 1", call. = FALSE)
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

#' @export
box_qtl <- function(x) {
  x <- stats::na.omit(x)
  quantile(x, c(0.1, 0.9)) %>% set_names(c("ymin", "ymax"))
}

#############################################################
# Minhas funções ----
#############################################################

#calcula a primeira derivada
detectaPicos = function(sinal){
  if(ncol(sinal)!=2 | !all(sapply(sinal, is.numeric))){
    stop("O dataframe deve possuir duas colunas numéricas!")
  }
  colnames(sinal)<-c('tempo','y')
  #calcula a 1a diferencial
  dif<-data.frame(tempo=sinal$tempo[-1], 
                  dif=diff(sinal$y)/diff(sinal$tempo))
  #calcula a segunda derivada
  dif$dif2 <- c(diff(dif$dif)/diff(dif$tempo),0)
  
  dif$n1[dif$dif <0] <- -1
  dif$n1[dif$dif >=0] <- 1
  picos <- list()
  idx=183
  idx2=0
  for(idx in 2:nrow(dif)){
    idx2 = idx2+1
    if(dif$n1[idx - 1] != dif$n1[idx]){
      tempo <- dif$tempo[idx]
      #pico 1 é maximo e -1 mínimo
      if(dif$dif2[idx] < 0){
        pico <- 1
      }else{
        pico <- -1
      }
      picos[[idx2]] <- data.frame(tempo = tempo, pico = pico)
    }
  }
  
  picos <- do.call(rbind, picos)
  return(picos)
}

detectaTs <- function(sinal, thT1Dw=0.3, thT1Up = 10, thT2=50){
  if(ncol(sinal)!=2 | !all(sapply(sinal, is.numeric))){
    stop("O dataframe deve possuir duas colunas numéricas!")
  }
  colnames(sinal)<-c('tempo','y')
  picos <- detectaPicos(sinal)
  if(picos$pico[1] == -1){
    picos <- rbind(data.frame(tempo = 0, pico = 1), picos)
  }
  inicio = 1
  picos$T <- NA
  #existem pelo menos os 4 Ts?
  maxIdx <- nrow(picos)
  if(picos$pico[maxIdx] == -1){
    maxIdx <- maxIdx - 1
  }
  if(nrow(picos[picos$pico == 1,]) < 2){
    stop('Não há os quatro Ts no sinal')
  }
  ini <- inicio
  amplitude <- max(sinal$y)-min(sinal$y)
  #detecta T1
  idx = 1
  found = F
  for(idx in inicio:floor(maxIdx/2)){
    y1 <- sinal$y[sinal$tempo == picos$tempo[idx]]
    y2 <- sinal$y[sinal$tempo == picos$tempo[idx+1]]
    if((y1 - y2) >= (thT1Dw*amplitude/100) & 
       (y1 - y2) < (thT1Up*amplitude/100)){
      picos$T[idx] <- 1
      inicio = idx + 1
      found = T
      break
    }
  }
  #detecta T 2 , 3 e 4 
  if(!found){
    inicio <- ini
  }
  idx = 4
  for(idx in inicio:(nrow(picos)-1)){
    y1 <- sinal$y[sinal$tempo == picos$tempo[idx]]
    y2 <- sinal$y[sinal$tempo == picos$tempo[idx+1]]
    if((y2 - y1) >= (thT2*amplitude/100)){
      picos$T[idx + 1] <- 2
      if(maxIdx >= idx + 3){
        picos$T[idx + 3] <- 3
      }
      if(maxIdx >= idx + 5){
        picos$T[idx + 5] <- 4
      }
      break
    }
    idx<-idx+2
  }
  #preenche com NA os faltantes
  ts<-c(picos$tempo[!is.na(picos$T)],rep(NA,4))
  # ggplot()+
  #   geom_line(data = sinal, aes(x=tempo,y=y))+
  #   geom_point(data = sinal[sinal$tempo %in% t,], aes(x=tempo,y=y),color='blue')
  #retorna só os 4 Ts
  return(ts[1:4])
}

# Carregar funções a serem utilizadas
getData <- function(file_name, moment, grupo, id, trat,rep, samp = 1000, probs = c(0.25, 0.75)) {
  
  # O arquivo a ser lido foi preprocessado previamente. Contém as seguintes medidas
  # Time, Fx, Fy, Fz, Mx, My, Mz, CoPx (Co), and CoPy.
  # Argumento samp = frequência utilizada na plataforma
  
  # Nomes das colunas
  cols <- c("tempo", "Fx", "Fy", "Fz", "Mx", "My", "Mz", "CoPx", "CoPy")
  
  # Importar dados
  df <- data.table::fread(file = file_name)
  colnames(df) <- cols
  
  # Remove center of pressure signal tendency and transform to milimeters
  # Remover a tendência no sinal do centro de pressão (CoP) e transformar em milímetros
  tempo<- df$tempo
  cop_ml <- gsignal::detrend(df$CoPy)*1000
  fz <- gsignal::detrend(df$CoPx)*1000
  
  # Centralizar CoP
  cop_ml <- scale(cop_ml, center = TRUE, scale = FALSE)
  cop_ap <- scale(fz, center = TRUE, scale = FALSE)
  #calcula o centro de pressão nas duas dimensões
  cop_v <- sqrt(cop_ml**2 + fz**2) 
  
  # Filtro Butterworth ordem 4
  n <- 4
  Wn <- 10/(samp/2)
  bw <- gsignal::butter(n, Wn, "low")
  copml_f <- gsignal::filtfilt(bw$b, bw$a, cop_ml)[,1]
  copap_f <- gsignal::filtfilt(bw$b, bw$a, cop_ap)[,1]
  copv_f <- gsignal::filtfilt(bw$b, bw$a, cop_v)[,1]
  # Retornar dataframe
  res <- data.frame(moment,grupo,id,trat,rep,tempo,cop_ml, cop_ap, cop_v, fz = df$Fz)
  res
}

calculate_area_cop <- function(df, samp = 1000) {
  
  # Cálculo das métricas de biomecânica
  
  # Deslocamento
  disp            <- sum(sqrt(df$cop_ml^2 + df$cop_ap^2))
  
  # Amplitude AP
  ampl_ap         <- max(df$cop_ap) - min(df$cop_ap)
  
  # Amplitude ML
  ampl_ml         <- max(df$cop_ml) - min(df$cop_ml)
  
  # Root mean square (equivalente ao desvio padrão quando a média = 0)
  rms_ap          <- sqrt(sum(df$cop_ap^2) / length(df$cop_ap))
  rms_ml          <- sqrt(sum(df$cop_ml^2) / length(df$cop_ml))
  
  # Eigenvalues (autovalores) da covariância entre CoP AP e CoP ML
  ei              <- eigen(cov(df$cop_ap, df$cop_ml), only.values = T)
  
  # Área do CoP
  area_var        <- pi*prod(2.4478*sqrt(svd(ei$values)$d))
  
  # Velocidade médio-lateralmédia total
  vel_ap <- sum(abs(diff(df$cop_ap)))*(samp/length(df$cop_ap))
  vel_ml <- sum(abs(diff(df$cop_ml)))*(samp/length(df$cop_ml))
  
  # Velocidade média total
  vel_media_total <- sum(sqrt(diff(df$cop_ap)^2+diff(df$cop_ml)^2))*(samp/length(df$cop_ap))
  
  res <- c(
    "disp"            = disp,
    "ampl_ml"         = ampl_ml,
    "ampl_ap"         = ampl_ap,
    "rms_ml"          = rms_ml,
    "rms_ap"          = rms_ap,
    "area_var"        = area_var,
    "vel_media_total" = vel_media_total,
    "vel_ml" = vel_ml,
    "vel_ap" = vel_ap
  )
  res
}

get_metaData <- function(folder){
  # Lê metadados pré-tratados, com as informações dos indivíduos para o M1 e M2
  metaTmp <- read.csv('../data/mt.csv')
  metaTmp$X<-NULL
  
  meta <- list()
  for(idx in 1:2){
    path_m1 <- paste0("../data/M",idx,"/",folder,"/")
    
    # Lista de arquivos
    fn <- list.files(path_m1, recursive = F)
    fn <- fn[!grepl(".docx", fn)]
    fn <- fn[!grepl("Ausente", fn)]
    
    # Cria metadados tratados, com as informações dos indivíduos para o M1 e M2
    meta[[idx]] <- data.frame(
      momento = idx, 
      file     = fn,
      id      = sapply(strsplit(fn, "\\_"), "[[", 1),
      path    = paste0(path_m1,fn),
      trat    = tolower(sapply(strsplit(fn, "_"), "[", 2)),
      rep    = substr(tolower(sapply(strsplit(fn, "_"), "[", 3)),1,1)
    )
  }
  
  meta <- rbind(meta[[1]],meta[[2]])
  
  # Combina dados pré-tratados com os obtidos dos arquivos
  meta<- merge(meta, metaTmp, by = 'id')
  return(meta)
}

get_picos <- function(ctlM1, ctlM2, itvM1, itvM2){
  #encontra pico máximo em cada indivíduo e reajuste para o eixo dos Y
  picos <- list()
  idx = 1
  ind = 'Ind16'
  for(ind in unique(ctlM1$id)){
    tmp <- ctlM1[ctlM1$id == ind &
                   ctlM1$tempo <= meanTime,] 
    tempo <- tmp$tempo[tmp$fzS == max(tmp$fzS)]
    corrY <- tmp$fzS[tmp$tempo == 0]
    
    #detectaPicos(tmp[,c('tempo','fzS')])
    tryCatch(
      expr = {Ts <- detectaTs(tmp[,c('tempo','fzS')])},
      error = function(e){ cat('Sem Ts para ',ind,'\n' )
        Ts <- c(NA, NA, NA, NA)      
      })
    picos[[idx]] <- data.frame(df = 'ctlM1', id = ind, tempo = tempo, corrY = corrY, T1 = Ts[1], T2 = Ts[2], T3 = Ts[3], T4 = Ts[4])
    idx = idx+1
    ggplot()+
      geom_line(data = tmp, aes(x=tempo,y=fzS))
    
  }
  ind='Ind25'
  for(ind in unique(ctlM2$id)){
    tmp <- ctlM2[ctlM2$id == ind &
                   ctlM2$tempo <= meanTime,] 
    tempo <- tmp$tempo[tmp$fzS == max(tmp$fzS)]
    corrY <- tmp$fzS[tmp$tempo == 0]
    tryCatch(
      expr = {Ts <- detectaTs(tmp[,c('tempo','fzS')])},
      error = function(e){ cat('Sem Ts para ',ind,'\n' )
        Ts <- c(NA, NA, NA, NA)      
      })
    
    picos[[idx]] <- data.frame(df = 'ctlM2', id = ind, tempo = tempo, corrY = corrY, T1 = Ts[1], T2 = Ts[2], T3 = Ts[3], T4 = Ts[4])
    idx = idx+1
  }
  ind = 'Ind12'
  for(ind in unique(itvM1$id)){
    tmp <- itvM1[itvM1$id == ind &
                   itvM1$tempo <= meanTime,]  
    tempo <- tmp$tempo[tmp$fzS == max(tmp$fzS)]
    corrY <- tmp$fzS[tmp$tempo == 0]
    tryCatch(
      expr = {Ts <- detectaTs(tmp[,c('tempo','fzS')])},
      error = function(e){ cat('Sem Ts para ',ind,'\n' )
        Ts <- c(NA, NA, NA, NA)      
      })
    picos[[idx]] <- data.frame(df = 'itvM1', id = ind, tempo = tempo, corrY = corrY, T1 = Ts[1], T2 = Ts[2], T3 = Ts[3], T4 = Ts[4])
    idx = idx+1
  }
  for(ind in unique(itvM2$id)){
    tmp <- itvM2[itvM2$id == ind &
                   itvM2$tempo <= meanTime,]   
    tempo <- tmp$tempo[tmp$fzS == max(tmp$fzS)]
    corrY <- mean(tmp$fzS[1:100])
    tryCatch(
      expr = {Ts <- detectaTs(tmp[,c('tempo','fzS')])},
      error = function(e){ cat('Sem Ts para ',ind,'\n' )
        Ts <- c(NA, NA, NA, NA)      
      })
    picos[[idx]] <- data.frame(df = 'itvM2', id = ind, tempo = tempo, corrY = corrY, T1 = Ts[1], T2 = Ts[2], T3 = Ts[3], T4 = Ts[4])
    idx = idx+1
  }
  
  picos <- do.call(rbind, picos)
  picos$max <- 0
  picos$min <- 0
  return(picos)
}

ajusta_XY <- function(picos, picosFile, ctlM1, ctlM2, itvM1, itvM2){
  #Ajusta eixos X e Y
  ind = 'Ind30'
  for(ind in picos$id[picos$df == 'ctlM1']){
    tmp <- picos[picos$id == ind &
                   picos$df == 'ctlM1',] 
    #Shift horizontal para posicionar todos T2 em um mesmo local
    ctlM1$tempo[ctlM1$id == ind] <- ctlM1$tempo[ctlM1$id == ind]# - tmp$T1 + corrX
    #Shift vertical para iniciar o gráfico em zero
    ctlM1$fzS[ctlM1$id == ind] <- ctlM1$fzS[ctlM1$id == ind] - tmp$corrY
    max <- max(ctlM1$fzS[ctlM1$id == ind])
    min <- min(ctlM1$fzS[ctlM1$id == ind])
    #Normalização entre -1 e 1
    ctlM1$fzN[ctlM1$id == ind &
                ctlM1$fzS >=0 ] <- ctlM1$fzS[ctlM1$id == ind &
                                               ctlM1$fzS >=0 ]/max
    ctlM1$fzN[ctlM1$id == ind &
                ctlM1$fzS < 0] <- -ctlM1$fzS[ctlM1$id == ind &
                                               ctlM1$fzS < 0 ]/min
    picos$max[picos$id == ind & picos$df == 'ctlM1'] <- max
    picos$min[picos$id == ind & picos$df == 'ctlM1'] <- min
    
  }
  for(ind in picos$id[picos$df == 'ctlM2']){
    tmp <- picos[picos$id == ind &
                   picos$df == 'ctlM2',] 
    ctlM2$tempo[ctlM2$id == ind] <- ctlM2$tempo[ctlM2$id == ind] #- tmp$T1 + corrX
    ctlM2$fzS[ctlM2$id == ind] <- ctlM2$fzS[ctlM2$id == ind] - tmp$corrY
    max <- max(ctlM2$fzS[ctlM2$id == ind])
    min <- min(ctlM2$fzS[ctlM2$id == ind])
    #Normalização entre -1 e 1
    ctlM2$fzN[ctlM2$id == ind &
                ctlM2$fzS >=0 ] <- ctlM2$fzS[ctlM2$id == ind &
                                               ctlM2$fzS >=0 ]/max
    ctlM2$fzN[ctlM2$id == ind &
                ctlM2$fzS < 0] <- -ctlM2$fzS[ctlM2$id == ind &
                                               ctlM2$fzS < 0 ]/min
    picos$max[picos$id == ind & picos$df == 'ctlM2'] <- max
    picos$min[picos$id == ind & picos$df == 'ctlM2'] <- min
    
  }
  ind = 'Ind7'
  for(ind in picos$id[picos$df == 'itvM1']){
    tmp <- picos[picos$id == ind &
                   picos$df == 'itvM1',] 
    #itvM1$tempo[itvM1$id == ind] <- itvM1$tempo[itvM1$id == ind] #- tmp$T1 + corrX
    # itvM1$fzS[itvM1$id == ind] <- itvM1$fzS[itvM1$id == ind] - tmp$corrY
    itvM1$fzS[itvM1$id == ind] <- itvM1$fzS[itvM1$id == ind] - tmp$corrY
    max <- max(itvM1$fzS[itvM1$id == ind])
    min <- min(itvM1$fzS[itvM1$id == ind])
    #Normalização entre -1 e 1
    itvM1$fzN[itvM1$id == ind &
                itvM1$fzS >=0 ] <- itvM1$fzS[itvM1$id == ind &
                                               itvM1$fzS >=0 ]/max
    itvM1$fzN[itvM1$id == ind &
                itvM1$fzS < 0] <- -itvM1$fzS[itvM1$id == ind &
                                               itvM1$fzS < 0 ]/min
    picos$max[picos$id == ind & picos$df == 'itvM1'] <- max
    picos$min[picos$id == ind & picos$df == 'itvM1'] <- min
  }
  for(ind in picos$id[picos$df == 'itvM2']){
    tmp <- picos[picos$id == ind &
                   picos$df == 'itvM2',] 
    #itvM2$tempo[itvM2$id == ind] <- itvM2$tempo[itvM2$id == ind]# - tmp$T1 + corrX
    itvM2$fzS[itvM2$id == ind] <- itvM2$fzS[itvM2$id == ind] - tmp$corrY
    max <- max(itvM2$fzS[itvM2$id == ind])
    min <- min(itvM2$fzS[itvM2$id == ind])
    itvM2$fzN[itvM2$id == ind &
                itvM2$fzS >=0 ] <- itvM2$fzS[itvM2$id == ind &
                                               itvM2$fzS >=0 ]/max
    itvM2$fzN[itvM2$id == ind &
                itvM2$fzS < 0] <- -itvM2$fzS[itvM2$id == ind &
                                               itvM2$fzS < 0 ]/min
    picos$max[picos$id == ind & picos$df == 'itvM2'] <- max
    picos$min[picos$id == ind & picos$df == 'itvM2'] <- min
  }
  minMax<- picos[c('df','id','min','max')]
  
  #Ts reajustados manualmente
  picos <- read_csv(picosFile)
  
  picos$min <- NULL
  picos$max <- NULL
  
  picos <- merge(picos, minMax, by= c('df','id'))
  
  return(list(picos, ctlM1, ctlM2, itvM1, itvM2))
}

get_metrics <- function(picos, ctlM1, ctlM2, itvM1, itvM2){
  #calculando as métricas por paciente e momento, todas as fases e por fase
  
  metricAll <- list()
  metricFase<- list()
  idxAll = 0
  idxFase = 0
  ind = 'Ind16'
  idxDf = 1
  for(idxDf in 1:4){
    if(idxDf == 1){
      base <- ctlM1
      moment = 1
      group = 'c'
      name <- 'ctlM1'
    }
    if(idxDf == 2){
      base <- ctlM2
      moment = 2
      group = 'c'
      name <- 'ctlM2'
    }
    if(idxDf == 3){
      base <- itvM1
      moment = 1
      group = 'i'
      name <- 'itvM1'
    }
    if(idxDf == 4){
      base <- itvM2
      moment = 2
      group = 'i'
      name <- 'itvM2'
    }
    for(ind in unique(base$id)){
      #Todos
      idxAll = idxAll + 1
      tmp1 <- data.frame(t(calculate_area_cop(
        base[base$id == ind,])))
      tmp2 <- data.frame(id = ind, 
                         group = group, 
                         moment = moment)
      tmp1 <- cbind(tmp2, tmp1)
      metricAll[[idxAll]] <- tmp1
      
      #por fase
      fases <- picos[picos$df == name &
                       picos$id == ind, 
                     c('T1','T2','T3','T4')]
      idx = 1
      for (idx in 1:3){
        idxFase = idxFase + 1
        inicio <- as.numeric(fases[,idx][1])
        tmp <- base[base$id == ind &
                      base$tempo > 
                      as.numeric(fases[,idx][1]) &
                      base$tempo <
                      as.numeric(fases[1,(idx+1)]) ,]
        tmp1 <- data.frame(t(calculate_area_cop(tmp)))
        tmp2 <- data.frame(id = ind, 
                           group = group, 
                           moment = moment,
                           fase = idx)
        tmp1 <- cbind(tmp2, tmp1)
        metricFase[[idxFase]] <- tmp1
      }
    }
  }
  metricAll <- do.call(rbind, metricAll)
  metricFase <- do.call(rbind, metricFase)
  
  #reshape dataframe
  metricAll$disp <- NULL
  metricAll$rms_ml <- NULL
  metricAll$rms_ap <- NULL
  metricAll$vel_media_total <- NULL
  
  metricAll <- pivot_longer(metricAll, 
                            cols = 4:8, 
                            names_to = 'trat',
                            values_to = 'valor')
  
  metricFase$disp <- NULL
  metricFase$rms_ml <- NULL
  metricFase$rms_ap <- NULL
  metricFase$vel_media_total <- NULL
  
  metricFase <- pivot_longer(metricFase, 
                             cols = 5:9, 
                             names_to = 'trat',
                             values_to = 'valor')
  return(list(metricAll, metricFase))
  
}

get_subgrupos <- function(df_f){
  #cria subgrupos
  ctlM1<- df_f[df_f$grupo == 'Controle'& 
                 df_f$moment == '1',
               c('tempo','id','fz',
                 'cop_ml','cop_ap','cop_v')]%>%
    group_by(id,tempo) %>%
    summarise(fz = mean(fz), 
              cop_ml = mean(cop_ml),
              cop_ap = mean(cop_ap),
              cop_v  = mean(cop_v))
  
  ctlM2<- df_f[df_f$grupo == 'Controle'& 
                 df_f$moment == '2',
               c('tempo','id','fz',
                 'cop_ml','cop_ap','cop_v')]%>%
    group_by(id,tempo) %>%
    summarise(fz = mean(fz), 
              cop_ml = mean(cop_ml),
              cop_ap = mean(cop_ap),
              cop_v  = mean(cop_v))
  
  itvM1<- df_f[df_f$grupo == 'Intervenção'& 
                 df_f$moment == '1',
               c('tempo','id','fz',
                 'cop_ml','cop_ap','cop_v')]%>%
    group_by(id,tempo) %>%
    summarise(fz = mean(fz), 
              cop_ml = mean(cop_ml),
              cop_ap = mean(cop_ap),
              cop_v  = mean(cop_v))
  
  itvM2<- df_f[df_f$grupo == 'Intervenção'& 
                 df_f$moment == '2',
               c('tempo','id','fz',
                 'cop_ml','cop_ap','cop_v')]%>%
    group_by(id,tempo) %>%
    summarise(fz = mean(fz), 
              cop_ml = mean(cop_ml),
              cop_ap = mean(cop_ap),
              cop_v  = mean(cop_v))
  return(list(ctlM1, ctlM2, itvM1, itvM2))
}

trataTodos <- function(t0){
  t1<-normaliza(t0)
  t1$trat[t1$trat == "juntos"]<-'j'
  t1$trat[t1$trat == "semitandem"]<-'st'
  t1$trat[t1$trat == "separados"]<-'s'
  t1$trat[t1$trat == "tandem"]<-'t'
  trats<-c('j','st','s','t')
  reps<-c(1,2,3)
  namesRef<-colnames(t1[1:7])
  names<-'id'
  for(trat in trats){
    for (rep in reps) {
      for (name in namesRef) {
        name<- c(paste0(name,'_',trat,'_',rep))
        names <-c(names, name)
      }  
    }
  }
  #names<-names[-1]
  t1$name<-paste0(t1$id,"_",t1$trat)
  t1<- t1[order(t1$name),]
  t1$rep <- 0
  t1$rep[1] <- 1
  rep = 1
  idx =2
  
  for(idx in 2:nrow(t1)){
    ant <- t1$name[idx-1]
    atu <- t1$name[idx]
    if(ant == atu){
      rep <- rep + 1
    }else{
      rep <- 1
    }
    t1$rep[idx] <- rep
  }
  
  t2<-list()
  #t2[[1]]<-names
  ids = ''
  id = 'Ind1'
  trat = 'j'
  rep=1
  idx=1
  for(id in unique(t1$id)){
    linha = 0
    ids = c(ids, id)
    for(trat in trats){
      for (rep in reps) {
        tmp<-as.matrix(t1[t1$id == id & t1$trat == trat & t1$rep == rep, 1:7])
        linha <- c(linha,tmp)
        
      }
    }
    t2[[idx]]<-linha
    idx = idx+1
    
  }
  ids<-ids[-1]
  t2<-do.call(rbind.data.frame, t2)
  colnames(t2)<-names
  t2$id<-ids
  
  meta<-unique(meta_m1[,c(2,4)])
  meta$grupo<-as.factor(substr(meta$grupo,1, 1))
  t2<-merge(meta,t2,by='id')
  t2<-na.exclude(t2)
  targets<-t2$grupo
  t2$grupo<-NULL
  ids<-t2$id
  t2 <- t2 %>% remove_rownames %>% column_to_rownames(var="id")
  
  # Estabelecer o esquema de cores
  color.code<-colorRampPalette(c('blue','red'))(2)
  pch.code<-c(15,16)
  shapes<-as.character(pch.code[targets])
  
  pca <- prcomp(t2,scale = T)
  pcs<-data.frame(pca$x)
  
  fviz_pca_ind(pca, col.ind = "#00AFBB",
               repel = TRUE)
  
  g<-ggplot(data = pcs[,1:2],
            aes(PC1,PC2,
                label=ids,
                shape=shapes,
                color = color.code[targets]))+
    geom_point()+
    #geom_text(check_overlap = F, cex=3, nudge_x = 0.5, col=1)+
    theme_bw()+
    scale_shape_manual(name = "",
                       guide = "legend",
                       labels=c("Ctl", "Itv"),
                       values = c(15,16))+
    scale_color_manual(name = "",
                       guide = "legend",
                       labels=c("Ctl", "Itv"),
                       values = color.code)+
    theme(legend.position = c(0.9,0.9),
          legend.background = element_rect(fill = alpha("white",0)))+
    geom_text_repel(cex=3)
  
  
  pca=PCA(t2, graph=F, scale.unit = TRUE)
  ret<-list()
  ret[[1]]<-g
  ret[[2]]<-pca
  return(ret)  
}

trataMedia <- function(t0){
  t1<-normaliza(t0)
  t1$trat[t1$trat == "juntos"]<-'j'
  t1$trat[t1$trat == "semitandem"]<-'st'
  t1$trat[t1$trat == "separados"]<-'s'
  t1$trat[t1$trat == "tandem"]<-'t'
  trats<-c('j','st','s','t')
  namesRef<-colnames(t1[1:7])
  names<-'id'
  for(trat in trats){
    for (name in namesRef) {
      name<- c(paste0(name,'_',trat))
      names <-c(names, name)
    }
  }
  t1$name<-paste0(t1$id,"_",t1$trat)
  t1<- t1[order(t1$name),]
  t2<-list()
  ids = ''
  id = 'Ind1'
  trat = 'j'
  idx=1
  for(id in unique(t1$id)){
    linha = 0
    ids = c(ids, id)
    for(trat in trats){
      tmp<-(t1[t1$id == id & t1$trat == trat, 1:7])
      tmp<-apply(tmp,MARGIN = 2,FUN = mean)
      linha <- c(linha,tmp)
      
    }
    t2[[idx]]<-linha
    idx = idx+1
    
  }
  ids<-ids[-1]
  t2<-do.call(rbind.data.frame, t2)
  colnames(t2)<-names
  t2$id<-ids
  
  meta<-unique(meta_m1[,c(2,4)])
  meta$grupo<-as.factor(substr(meta$grupo,1, 1))
  t2<-merge(meta,t2,by='id')
  t2<-na.exclude(t2)
  targets<-t2$grupo
  t2$grupo<-NULL
  ids<-t2$id
  t2 <- t2 %>% remove_rownames %>% column_to_rownames(var="id")
  
  
  # Estabelecer o esquema de cores
  color.code<-colorRampPalette(c('blue','red'))(2)
  pch.code<-c(15,16)
  shapes<-as.character(pch.code[targets])
  
  pca <- prcomp(t2,scale = T)
  pcs<-data.frame(pca$x)
  
  fviz_pca_ind(pca, col.ind = "#00AFBB",
               repel = TRUE)
  
  g<-ggplot(data = pcs[,1:2],
            aes(PC1,PC2,
                label=ids,
                shape=shapes,
                color = color.code[targets]))+
    geom_point()+
    # geom_text(check_overlap = F, cex=3, nudge_x = 0.5, col=1)+
    theme_bw()+
    scale_shape_manual(name = "",
                       guide = "legend",
                       labels=c("Ctl", "Itv"),
                       values = c(15,16))+
    scale_color_manual(name = "",
                       guide = "legend",
                       labels=c("Ctl", "Itv"),
                       values = color.code)+
    theme(legend.position = c(0.9,0.9),
          legend.background = element_rect(fill = alpha("white",0)))+
    geom_text_repel(cex=3)
  
  pca=PCA(t2, graph=F, scale.unit = TRUE)
  ret<-list()
  ret[[1]]<-g
  ret[[2]]<-pca
  return(ret)  
}

# Função para obter a média das medidas de cada paciente
resumo_f <- function(momento, meta, variavel, m) {
  
  momento %>% 
    group_by(id, trat) %>% 
    summarise("media_{variavel}" := mean(.data[[variavel]], na.rm = T)) %>% 
    ungroup() %>% 
    inner_join(meta %>% dplyr::select(id, grupo) %>% ungroup(), by = "id") %>% 
    unique() %>% 
    mutate(momento = m)->  df_resumo 
  
  return(df_resumo)
  
}

obter_medias <- function(momento1, momento2, meta_m1, meta_m2, variavel) {
  
  # Obter as médias de cada medição M1 (tandem, semitandem, juntos, separados)
  df_medida_m1 <- resumo_f(momento1, meta_m1, variavel, m = "Pré")
  
  # Obter as médias de cada medição M2 (tandem, semitandem, juntos, separados)
  df_medida_m2 <- resumo_f(momento2, meta_m2, variavel, m = "Pós")
  
  df_medida <- bind_rows(df_medida_m1, df_medida_m2) 
  
  return(df_medida)
  
}

# Erro padrão
se <- function(x){ 
  sqrt(var(x[!is.na(x)]) / length(x[!is.na(x)]))
}

normaliza<- function(t1){
  for(idx in 1:7){
    mean<-mean(t1[,idx])
    sd<-sd(t1[,idx])
    t1[,idx]<- (t1[,idx]-mean)/sd
  }
  return(t1)
}
