nx <- 10
ny <- 10
ne <- 4

field <- array(NA, dim = c(nx, ny, ne),
               dimnames = list(
                 sprintf('x%04i', 1:nx),
                 sprintf('y%04i', 1:ny),
                 NULL
                 )
               )
                 
                 
