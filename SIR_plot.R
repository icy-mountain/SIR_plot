library(deSolve)
library(ggplot2)

#parameters <- c(b = 1/2, k = 1/3)# �p�����[�^�̃Z�b�g
parameters <- c(b =3.60*(1/14), g = 1/14)#WHO�ɂ��Ɗ������Ԃ�14���AR0�͓K��
initial <- c(s = 1, i = 7.93*10^(-9), r = 0)# ���{
#initial <- c(s = 1, i = 1.27*10^(-6), r = 0)# ��������
times <- seq(1, 240, 1)# ��������

SIR <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    ds <- -b * s * i
    di <- (b * s - g)*i
    dr <- g * i
    list(c(ds, di, dr))
  })
}

out <- ode(y = initial, times = times, func = SIR, parms = parameters)
out_df <- as.data.frame(out)
SIRplot_base <- ggplot(data=out_df) +
  xlab("time")+ ylab("Y value") +
  ggtitle("SIR model")

SIRplot <- SIRplot_base +
  layer(
    mapping=aes(x=time, y=s, colour="s"), 
    geom="line", 
    stat="identity", 
    position="identity",
  ) +
  layer(
    mapping=aes(x=time, y=i, colour="i"), 
    geom="line", 
    stat="identity", 
    position="identity",
  ) +
  layer(
    mapping=aes(x=time, y=r,colour="r"), 
    geom="line", 
    stat="identity", 
    position="identity",
  ) 