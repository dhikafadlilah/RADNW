ujisampel <- function(data, mu)
{
  n <- length(data)
  mean_data <- mean(data)
  sd_data <- sd(data)
  t_value <- (mean_data - mu) / (sd_data / sqrt(n))
  df <- n - 1
  p_value <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)

  print(paste("Hipotesis nol (H0): Rata-rata populasi =", mu))
  print(paste("Hipotesis alternatif (H1): Rata-rata populasi ≠ ", mu))

  print(paste("Jumlah data:", n))
  print(paste("Rata-rata sampel:", mean_data))
  print(paste("Deviasi standar sampel:", sd_data))

  print(paste("t-value:", t_value))
  print(paste("p-value:", p_value))

  if (p_value < 0.05) {
    cat("P-value < 0.05, maka H0 ditolak. Terdapat bukti yang cukup untuk menyatakan bahwa rata-rata populasi tidak sama dengan 10.\n")
  } else {
    cat("P-value >= 0.05, maka tidak cukup bukti untuk menolak H0. Tidak ada bukti yang cukup untuk menyatakan bahwa rata-rata populasi tidak sama dengan 10.\n")
  }
}
