b1 <- c(ac1 = 1, ac2 = 4, ac3 = 2)
b2 <- c(ac1 = 2, ac2 = 2, ac3 = 1)
b3 <- c(ac1 = 4, ac2 = 4, ac3 = 2)
b4 <- c(ac1 = 3, ac2 = 0, ac3 = 0)
ac_bundles <- cbind(b1, b2, b3, b4)

ac_bundles
t(ac_bundles)

plot(t(ac_bundles), xlim=c(0, 5), ylim=c(0, 5))