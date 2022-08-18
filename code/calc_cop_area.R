

# Funções úteis -----------------------------------------------------------

filter_cop <- function(file_name, samp = 1000, probs = c(0.25, 0.75)) {
  
  # O arquivo a ser lido foi preprocessado previamente. Contém as seguintes medidas
  # Time, Fx, Fy, Fz, Mx, My, Mz, CoPx (Co), and CoPy.
  # Argumento samp = frequência utilizada na plataforma
  
  # Nomes das colunas
  cols <- c("tempo", "Fx", "Fy", "Fz", "Mx", "My", "Mz", "CoPx", "CoPy")
  
  # Importar dados
  df <- data.table::fread(file = file_name)
  colnames(df) <- cols
  
  # Zerar medidas de CoP
  filtro_zero_CoPx <- mean(df$CoPx[1:100])
  filtro_zero_CoPy <- mean(df$CoPy[1:100])
  filtro_zero_Fz <- mean(df$Fz[1:100])
  
  df$CoPx <- df$CoPx - filtro_zero_CoPx 
  df$CoPy <- df$CoPy - filtro_zero_CoPy
  
  # Remove center of pressure signal tendency and transform to milimeters
  # Remover a tendência no sinal do centro de pressão (CoP) e transformar em milímetros
  cop_ml <- gsignal::detrend(df$CoPy)*1000
  cop_ap <- gsignal::detrend(df$CoPx)*1000
  
  # Centralizar CoP
  cop_ml <- scale(cop_ml, center = TRUE, scale = FALSE)
  cop_ap <- scale(cop_ap, center = TRUE, scale = FALSE)
  
  # Filtro Butterworth ordem 4
  n <- 4
  Wn <- 10/(samp/2)
  bw <- gsignal::butter(n, Wn, "low")
  copml_f <- gsignal::filtfilt(bw$b, bw$a, cop_ml)[,1]
  copap_f <- gsignal::filtfilt(bw$b, bw$a, cop_ap)[,1]
  
  # Recortar o início e o fim dos resultados
  if(is.null(q)) q <- q <- c(2500, 27500)
  else q <- quantile(1:nrow(df), probs = probs)
  
  cop_ml_rec <- copml_f[q[1]:q[2]]
  cop_ap_rec <- copap_f[q[1]:q[2]]
  fz_rec <- df$Fz[q[1]:q[2]]
  
  # Retornar dataframe
  res <- data.frame(cop_ml_rec, cop_ap_rec, fz_rec)
  res
  
}

calculate_area_cop <- function(df_rec, samp = 1000) {
  
  # Cálculo das métricas de biomecânica
  
  # Deslocamento
  disp            <- sum(sqrt(df_rec$cop_ml_rec^2 + df_rec$cop_ap_rec^2))
  
  # Amplitude AP
  ampl_ap         <- max(df_rec$cop_ap_rec) - min(df_rec$cop_ap_rec)
  
  # Amplitude ML
  ampl_ml         <- max(df_rec$cop_ml_rec) - min(df_rec$cop_ml_rec)
  
  # Root mean square (equivalente ao desvio padrão quando a média = 0)
  rms_ap          <- sqrt(sum(df_rec$cop_ap_rec^2) / length(df_rec$cop_ap_rec))
  rms_ml          <- sqrt(sum(df_rec$cop_ml_rec^2) / length(df_rec$cop_ml_rec))
  
  # Eigenvalues (autovalores) da covariância entre CoP AP e CoP ML
  ei              <- eigen(cov(df_rec$cop_ap_rec, df_rec$cop_ml_rec), only.values = T)
  
  # Área do CoP
  area_var        <- pi*prod(2.4478*sqrt(svd(ei$values)$d))
  
  # Velocidade média total
  vel_media_total <- sum(sqrt(diff(df_rec$cop_ap_rec)^2+diff(df_rec$cop_ml_rec)^2))*(samp/length(df_rec$cop_ap_rec))
  
  res <- c(
    "disp"            = disp,
    "ampl_ml"         = ampl_ml,
    "ampl_ap"         = ampl_ap,
    "rms_ml"          = rms_ml,
    "rms_ap"          = rms_ap,
    "area_var"        = area_var,
    "vel_media_total" = vel_media_total
  )
  res
  
}

# Contar linhas em cada arquivo, para checar consistência entre eles
count_lines <- function(file_name) {
  linhas <- length(readLines(file_name))
  linhas
}