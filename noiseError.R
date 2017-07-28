noiseError <- function(SNR, noiseBearing, callBearing) {
      noiseRad <- noiseBearing * pi / 180
      callRad <- callBearing * pi / 180
      dbRatio <- 1 / (10^(SNR/20) - 1)
      (atan2(sin(callRad) + dbRatio*sin(noiseRad),
                 cos(callRad) + dbRatio*cos(noiseRad)) * 180 / pi) %% 360
}
