# install.packages("pacman",dependencies = T,repos = "http://cran.us.r-project.org")
library(pacman)

# Import data
library(readxl)
dataset <- read_excel('Input Beras Terbaru agregrasi tambah tahun.xlsx', sheet = 'Input')
head(dataset)
library(ivreg)
library(forecast)

# Persamaan-persamaan
# Blok Ketersediaan Beras

# APSU ~ HGSU/HFSU + AISU + CHSU + YISU + APSUt
#BKB11 <- ivreg(APSU ~ HGSU_HFSU + AISU + CHSU+ APSUt + PSSU|.-PSSU + APSU*YISU, data=dataset)
#summary(BKB11)

BKB12 <- ivreg(APSU ~ HGSU_HFSU + AISU + CHSU+ APSUt + TPSU|.-TPSU + PSSU + PLSU, data=dataset)
summary(BKB12)

BKB13 <- ivreg(APSU ~ HGSU_HFSU + AISU + CHSU+ APSUt + TPBSU|.-TPBSU + K*TPSU, data=dataset)
summary(BKB13)

BKB14 <- ivreg(APSU ~ HGSU_HFSU + AISU + CHSU+ APSUt + BSPI|.-BSPI + P*TPBI, data=dataset)
summary(BKB14)
plot(dataset$APSU)
lines(predict(BKB14),type="l")

# YISU ~ HGSU_HFSU + AISU + JPSU + APSU + YISUt
#BKB21 <- ivreg(YISU ~ HGSU_HFSU + AISU + JPSU + APSU + YISUt + PSSU|.-PSSU + APSU_YISU, data=dataset)
#summary(BKB21)

BKB22 <- ivreg(YISU ~ HGSU_HFSU + AISU + JPSU + APSU + YISUt + TPSU|.-TPSU + PSSU + PLSU, data=dataset)
summary(BKB22)

BKB23 <- ivreg(YISU ~ HGSU_HFSU + AISU + JPSU + APSU + YISUt + TPBSU|.-TPBSU + K*TPSU, data=dataset)
summary(BKB23)

BKB24 <- ivreg(YISU ~ HGSU_HFSU + AISU + JPSU + APSU + YISUt + BSPI|.-BSPI + P*TPBI, data=dataset)
summary(BKB24)
plot(predict(BKB24),type="l")

# JPSU ~ HGSU_HFSU + ATSU + JPSUt
#BKB31 <- ivreg(JPSU ~ HGSU_HFSU + ATSU + JPSUt + PSSU|.-PSSU + APSU_YISU, data=dataset)
#summary(BKB31)

BKB32 <- ivreg(JPSU ~ HGSU_HFSU + ATSU + JPSUt + TPSU|.-TPSU + PSSU + PLSU, data=dataset)
summary(BKB32)

BKB33 <- ivreg(JPSU ~ HGSU_HFSU + ATSU + JPSUt + TPBSU|.-TPBSU + K*TPSU, data=dataset)
summary(BKB33)

BKB34 <- ivreg(JPSU ~ HGSU_HFSU + ATSU + JPSUt + BSPI|.-BSPI + P*TPBI, data=dataset)
summary(BKB34)
plot(predict(BKB34),type="l")

# Blok Akses Beras
# ANSU ~ PPSU + KBSU + KKSU + PKSS + PJSS + ANSUt
BAB11 <- ivreg(ANSU ~ PPSU + KBSU + KKSU + PKSS + PJSS + ANSUt + PESU | .-PESU + HGSU_YISU - HFSU_JPSU - BUSU - BKSU + BLSU, data=dataset)
summary(BAB11)

BAB12 <- ivreg(ANSU ~ PPSU + KBSU + KKSU + PKSS + PJSS + ANSUt + PESS | .-PESS + HGSS...57*YISS - HFSS_JPSS - BUSS - BKSS + BLSS, data=dataset)
summary(BAB12)
plot(predict(BAB12),type="l")

# ANSS ~ PPSS + KBSS + KKSS + PKSS + PJSS + ANSSt
BAB21 <- ivreg(ANSS ~ PPSS + KBSS + KKSS + PKSS + PJSS + ANSSt + PESU | .-PESU + HGSU_YISU - HFSU_JPSU - BUSU - BKSU + BLSU, data=dataset)
summary(BAB21)

BAB22 <- ivreg(ANSS ~ PPSS + KBSS + KKSS + PKSS + PJSS + ANSSt + PESS | .-PESS + HGSS...57*YISS - HFSS_JPSS - BUSS - BKSS + BLSS, data=dataset)
summary(BAB22)
plot(predict(BAB22),type="l")

# KOBI ~ HBDI + HJGN + GDPI + POPI + RPKD + LOGtahun + KOBIt
BAB31 <- ivreg(KOBI ~ HBDI + HJGN + GDPI + POPI + RPKD + LOGtahun + KOBIt + PESU | .-PESU + HGSU_YISU - HFSU_JPSU - BUSU - BKSU + BLSU, data=dataset)
summary(BAB31)
BAB31$coefficients

BAB32 <- ivreg(KOBI ~ HBDI + HJGN + GDPI + POPI + RPKD + LOGtahun + KOBIt + PESS | .-PESS + HGSS...57*YISS - HFSS_JPSS - BUSS - BKSS + BLSS, data=dataset)
summary(BAB32)
BAB32$coefficients

# Blok Penyerapan Beras
# PYSU ~ KESU + AISU + CHSU + SKSS + IBSS + PYSUt
BPB11 <- ivreg(PYSU ~ KESU + AISU + CHSU + SKSS + IBSS + PYSUt, data=dataset)
summary(BPB11)
plot(predict(BPB11),type="l")

# PYSS ~ KESS + KBSS + FKSS + SKSS + IBSS + PYSSt
# BPB21 <- ivreg(PYSS ~ KESS + KGSS + FKSS + SKSS + IBSS + PYSSt, data=dataset)
# summary(reg_iv21)

# Blok Harga Beras
# HGBI ~ HDGB + IHBI_ECRI + MPBI + TPPI + HEPSt
BHP11 <- ivreg(HGBI ~ HDGB + IHBI_ECRI + MPBI + TPPI + HEPS + MPBI| .-MPBI + HBDI - HGBI/K, data=dataset)
summary(BHP11)

BHP12 <- ivreg(HGBI ~ HDGB + IHBI_ECRI + MPBI + TPPI + HEPS + MPSU| .-MPSU + HESU - HGSU/K, data=dataset)
summary(BHP12)
plot(predict(BHP12),type="l")

#BHP13 <- ivreg(HGBI ~ HDGB + IHBI_ECRI + MPBI + TPPI + HEPS + MPSS| .-MPSS + HESS - HGSS...57/K, data=dataset)
#summary(BHP13)

# HGSU ~ HDGB + IHBI_ECRI + MPSU + PSSU_PLSU + ASBI_TPPI + HGSU_T
BHP21 <- ivreg(HGSU ~ HDGB + IHBI_ECRI + MPSU + PSSU_PLSU + ASBI_TPPI + HGSUt + MPBI| .-MPBI + HBDI - HGBI/K, data=dataset)
summary(BHP21)
plot(predict(BHP21),type="l")

BHP22 <- ivreg(HGSU ~ HDGB + IHBI_ECRI + MPSU + PSSU_PLSU + ASBI_TPPI + HGSUt + MPSU| .-MPSU + HESU - HGSU/K, data=dataset)
summary(BHP22)

BHP23 <- ivreg(HGSU ~ HDGB + IHBI_ECRI + MPSU + PSSU_PLSU + ASBI_TPPI + HGSUt + MPSS| .-MPSS + HESS - HGSS...57/K, data=dataset)
summary(BHP23)

# HGSU ~ HDGB + IHBI_ECRI + MPSS + PSSS_PLSS + ASBI_TPPI + T + HGSS
BHP31 <- ivreg(HGSS...57 ~ HDGB + IHBI_ECRI + MPSS + PSSS_PLSS + ASBI_TPPI + T + HGSS...76 + MPBI| .-MPBI + HBDI - HGBI/K, data=dataset)
summary(BHP31)
BHP31$coefficients

BHP32 <- ivreg(HGSS...57 ~ HDGB + IHBI_ECRI + MPSS + PSSS_PLSS + ASBI_TPPI + T + HGSS...76 + MPSU| .-MPSU + HESU - HGSU/K, data=dataset)
summary(BHP32)
BHP32$coefficients

BHP33 <- ivreg(HGSS...57 ~ HDGB + IHBI_ECRI + MPSS + PSSS_PLSS + ASBI_TPPI + T + HGSS...76 + MPSS| .-MPSS + HESS - HGSS...57/K, data=dataset)
summary(BHP33)

# Blok Distribusi Beras

# IHBI ~ HIMI_ECRI + TPBI_TPBIT + LOGtahun
BDB11 <- ivreg(IHBI ~ HIMI_ECRI + TPBI_TPBIt + LSBI...22 + LOGtahun + PESU | .-PESU + HGSU_YISU - HFSU_JPSU - BUSU - BKSU + BLSU, data=dataset)
summary(BDB11)
BDB11$coefficients

BDB12 <- ivreg(IHBI ~ HIMI_ECRI + TPBI_TPBIt + LSBI...22 + LOGtahun + PESS | .-PESS + HGSS...57*YISS - HFSS_JPSS - BUSS - BKSS + BLSS, data=dataset)
summary(BDB12)
BDB12$coefficients

# HBDI ~ HIMI_ECRI + IHBI + QSBI_KOBI + HGBI + LSBI + HBDIt
BDB21 <- ivreg(HBDI ~ HIMI_ECRI + IHBI + QSBI*KOBI + HGBI + LSBI...22 + HBDIt + PESU | .-PESU + HGSU_YISU - HFSU_JPSU - BUSU - BKSU + BLSU, data=dataset)
summary(BDB21)

BDB22 <- ivreg(HBDI ~ HIMI_ECRI + IHBI + QSBI*KOBI + HGBI + LSBI...22 + HBDIt + PESS | .-PESS + HGSS...57*YISS - HFSS_JPSS - BUSS - BKSS + BLSS, data=dataset)
summary(BDB22)
plot(predict(BDB22),type="l")

# HESU ~ HIMI_ECRI + IHBI + HGSU + T + HESUt
BDB31 <- ivreg(HESU ~ HIMI_ECRI + IHBI + HGSU + T + HESUt + PESU | .-PESU + HGSU_YISU - HFSU_JPSU - BUSU - BKSU + BLSU, data=dataset)
summary(BDB31)
BDB31$coefficients

BDB32 <- ivreg(HESU ~ HIMI_ECRI + IHBI + HGSU + T + HESUt + PESS | .-PESS + HGSS...57*YISS - HFSS_JPSS - BUSS - BKSS + BLSS, data=dataset)
summary(BDB32)
plot(predict(BDB32),type="l")

