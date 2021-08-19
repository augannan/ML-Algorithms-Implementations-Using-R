# Fit a polynomial of degree 4 (use default in poly) in range
fit <- lm(logratio ~ poly(range, 4), data = lidar)
coef(summary(fit))
rangelims <- range(lidar$range)
range.grid <- seq(from = rangelims[1], to = rangelims[2])
preds <- predict(fit, newdata = list(range = range.grid), se = TRUE)
se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
head(se.bands)
plot(logratio ~ range, xlim = rangelims, cex = 0.5, col = "darkgrey", data = lidar)
lines(range.grid, preds$fit, lwd = 2, col = "blue")
matlines(range.grid, se.bands, lwd = 1, col = "blue", lty = 3)
title("Degree-4 Polynomial")

# cubic regression spline with 4 degrees of freedom
quantile(lidar$range)
mat1 <- bs(lidar$range, knots = c(472 , 555 , 637))
dim(mat1)
head(mat1)
mat2 <- bs(lidar$range, df = 6)
attr(mat2, "knots")
dim(mat2)
head(mat2)
fit <- lm(logratio ~ bs(range, knots = c(25, 40, 60)), data = lidar)
summary(fit)
pred <- predict(fit, newdata = list(range = range.grid), se = T)
plot(logratio ~ range, col = "gray", data = lidar)
lines(range.grid, pred$fit, lwd = 2)
lines(range.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(range.grid, pred$fit - 2 * pred$se, lty = "dashed")
title("Cubic regression spline fit with knots at (472 , 555 , 637)")


# natural cubic regression spline with 4 degrees of freedom
mat3 <- ns(lidar$range, df = 4)
dim(mat3)
head(mat3)
fit2 <- lm(logratio ~ ns(range, df = 4), data = lidar)
pred2 <- predict(fit2, newdata = list(range = range.grid), se = T)
plot(logratio ~ range, col = "gray", data = lidar)
lines(range.grid, pred2$fit, col = "red", lwd = 2)
title("Natural cubic spline fit with 4 df")


# smoothing spline with 4 effective degrees of freedom
fit <- smooth.spline(lidar$range, lidar$logratio, df = 4)
fit$lambda
plot(logratio ~ range, xlim = rangelims, cex = 0.5, col = "darkgrey", data = lidar)
lines(fit, col = "red", lwd = 2)
title("Smoothing Spline")

# natural cubic regression spline with 4 degrees of freedom
mat3 <- ns(lidar$range, df = 4)
dim(mat3)
head(mat3)
fit2 <- lm(logratio ~ ns(range, df = 4), data = lidar)
pred2 <- predict(fit2, newdata = list(range = range.grid), se = T)
plot(logratio ~ range, col = "gray", data = lidar)
lines(range.grid, pred2$fit, col = "red", lwd = 2)
title("Natural cubic spline fit with 4 df")
plot(logratio ~ range, col = "gray", data = lidar)
lines(range.grid, pred2$fit, col = "red", lwd = 2)
lines(range.grid, pred2$fit + 2 * pred2$se, lty = "dashed")
lines(range.grid, pred2$fit - 2 * pred2$se, lty = "dashed")
title("Natural cubic spline fit with 4 df")
