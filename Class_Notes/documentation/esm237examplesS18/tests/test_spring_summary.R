test_that("spring_summary_works" ,
{clim_data = as.data.frame(cbind(month=c(1:4), day=rep(1, times=4), year=rep(1,times=4),
rain=rep(0, times=4), tmax=c(2,2,1,1), tmin=rep(0, times=4)))

expect_that(spring_summary(clim_data, spring_months=4)$mean_springP, equals(0))
expect_that(spring_summary(clim_data, spring_months=4)$mean_springT, equals(0.5))
expect_that(spring_summary(clim_data, spring_months=1)$mean_springT, equals(1))
expect_true(spring_summary(clim_data, spring_months=c(1:4))$coldest_springT > -10)
})
