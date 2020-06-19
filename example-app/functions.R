# Functions


cost <- function(trt, rdt, cfp, cfn, pv, sn, sp) {
  cost.presum = trt + cfp*(1-pv)
  cost.tnt = rdt + trt*(sn*pv + (1-sp)*(1-pv)) +
    cfp*((1-pv)*(1-sp)) + 
    cfn*(pv*(1-sn))
  round(cost.presum - cost.tnt, 3)
}


#-----

cost_t <- function(R, FP, FN, p, sn, sp) {
  k1 = FP*(p - 1)*(sp) + FN*((1 - sn)*p) 
  k2 = 1 - (sn*p + (1 - sp)* (1 - p))
  ct = round((k1 + R) / k2, 3)
  ct
}