nx <- 100
ny <- 100
ne <- 5

field <- array(NA, dim = c(nx, ny, ne),
               dimnames = list(
                 sprintf('x%04i', 1:nx),
                 sprintf('y%04i', 1:ny),
                 NULL
                 )
               )
                 
                 
