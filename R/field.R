source('fieldconfig.R')

hotspots <- function(nx, ny, nhotspot, intensity, dist) {
  if (dist <= 1) stop('dist must be integer, >= 2')
  if (!is.integer(dist)) stop('dist must be integer, >= 2')
  xii <- sample(1:nx, nhotspot)
  yii <- sample(1:ny, nhotspot)
  base <- array(0, dim = c(nx, ny))
  for (hi in 1:nhotspot) {
    xi <- xii[hi]
    yi <- yii[hi]
    xj <- xi
    yj <- yi
    add <- intensity
    for (di in 2:dist) {
      for (i in 1:(di - 1)) {
        xj <- c(xj, xi + c(     i, i - di,      i, i - di))
        yj <- c(yj, yi + c(di - i,     -i, i - di,      i))
        add <- c(add, rep(intensity / di, times = 4))
      }
      xj <- c(xj, xi + c(di, 0, -di, 0))
      yj <- c(yj, yi + c(0, -di, 0, di))
      add <- c(add, rep(intensity / di, times = 4))
    }
    print(data.frame(xj, yj, add))
    is.in.field <- (xj >= 1) & (xj <= nx) & (yj >= 1) & (yj <= ny)
    for (ri in 1:length(add)) {
      if (!is.in.field[ri]) next
      base[xj[ri], yj[ri]] <- base[xj[ri], yj[ri]] + add[ri]
    }
  }
  return(base)
}

field[ , , 1] <- array(1, dim = c(nx, ny))
field[ , , 2] <- apply(array(1:ny, dim = c(1, ny)), 2, rep, each = nx)
field[ , , 3] <- apply(array(1:nx, dim = c(nx, 1)), 1, rep, each = nx)
field[ , , 4] <- hotspots(nx, ny, nhotspot = 1, intensity = 100, dist = 4L)


    
