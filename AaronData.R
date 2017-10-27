# Lasker data for aaron
library(PamBinaries)
bin1 <- loadPamguardBinaryFile(paste0('C:/HICEAS Acoustics Cruise Folder/Data/HICEAS_sb_pgFolder/',
                                      'sonobuoy/binaries/20171002/',
                                      'DIFAR_Processing_DIFAR_Localisation_DIFAR_Localisation_20171002_052217.pgdf'))

demux1 <- data.frame(bin1$data[[1]]$demuxData)
colnames(demux1) <- c('Om', 'EW', 'NS')
# write.csv(demux1, file='LaskerQuiet.csv', row.names = FALSE)
LaskerLoud <- loadPamguardBinaryFile(paste0('C:/HICEAS Acoustics Cruise Folder/Data/HICEAS_sb_pgFolder/',
                               'sonobuoy/binaries/20170903/',
                               'DIFAR_Processing_DIFAR_Localisation_DIFAR_Localisation_20170903_070513.pgdf'))
demux2 <- data.frame(LaskerLoud$data[[1]]$demuxData)
colnames(demux2) <- c('Om', 'EW', 'NS')
# write.csv(demux2, file='LaskerLoud.csv', row.names = FALSE)
# save(LaskerLoud, file='LaskerLoud.RData')

LaskerQuiet1001Later <- loadPamguardBinaryFile(paste0('C:/HICEAS Acoustics Cruise Folder/Data/HICEAS_sb_pgFolder/',
                               'sonobuoy/binaries/20171002/',
                               'DIFAR_Processing_DIFAR_Localisation_DIFAR_Localisation_20171002_060004.pgdf'))
demux3 <- data.frame(LaskerQuiet1001Later$data[[1]]$demuxData)
colnames(demux3) <- c('Om', 'EW', 'NS')
# 1500m, correct around 98
# write.csv(demux3, file='LaskerQuiet1001Later.csv')
# save(LaskerQuiet1001Later, file='LaskerQuiet1001Later.RData')

LaskerQuiet0930 <- loadPamguardBinaryFile(paste0('C:/HICEAS Acoustics Cruise Folder/Data/HICEAS_sb_pgFolder/',
                                      'sonobuoy/binaries/20171001/',
                                      'DIFAR_Processing_DIFAR_Localisation_DIFAR_Localisation_20171001_060000.pgdf'))
demux4 <- data.frame(LaskerQuiet0930$data[[1]]$demuxData)
colnames(demux4) <- c('Om', 'EW', 'NS')
# 1-1100m. Correct 217-227 -8
# write.csv(demux4, file='LaskerQuiet0930.csv')
# save(LaskerQuiet0930, file='LaskerQuiet0930.RData')



