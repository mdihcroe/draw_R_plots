PrettyScatter <- function(x, y, main, bg, reg.line = F, panel.first.step = 1, cex.lab = 1.5, xlim = c(min(x), max(x)), 
                          ylim = c(min(y), max(y)), xlab = '', ylab = '', abline = F){
  plot(x, y, xlim = xlim, ylim = ylim,
       xlab = xlab, ylab = ylab, 
       cex.lab = cex.lab,
       bg = bg, # Fill colour
       col = bg,
       pch = 20,
       axes = FALSE, # Don't plot the axes
       frame.plot = FALSE, # Remove the frame 
       #panel.first = ,
       main = main)
  if (abline == T){
    abline(h = seq(round(min(x)), round(max(y)), panel.first.step), col = 'grey60')
  }
  if (reg.line == T){
    abline(lm(y ~ x), col = "grey60")
  }
  at = pretty(xlim)
  at = at[-c(1,length(at))]
  mtext(side = 1, text = at, at = at, 
        col = "grey20", line = 1, cex = 0.9)
  at = pretty(ylim)
  at = at[-c(1,length(at))]
  mtext(side = 2, text = at, at = at, col = "grey20", line = 1, cex = 0.9)
}
