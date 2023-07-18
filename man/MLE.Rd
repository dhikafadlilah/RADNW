\name{MLE}
\alias{MLE}
\title{MLE Distribusi Normal}
\description{Mencari nilai parameter yang memberi kemungkinan atau likelihood yang paling besar untuk mendapatkan data yang terobservasi sebagai estimator pada distribusi normal}
\usage{MLE(n,mean,sd)}
\arguments{
  \item{n}{banyak data}
  \item{mean}{rata-rata data}
  \item{sd}{standar deviasi data}
}
\details{Fungsi ini digunakan untuk mencari nilai parameter yang memberi kemungkinan atau likelihood yang paling besar untuk mendapatkan data yang terobservasi sebagai estimator pada distribusi normal}
\value{Nilai Estimasi Parameter dan Nilai Likelihood Distribusi Normal}
\references{Utari,2023,Modul Komputasi Statistika dan Aplikasinya dengan R,UII,Yogyakarta}
\author{Wulan,Najla,Dhika,Alya,Renata}
\examples{\dontrun{MLE(100,0,1)}}
