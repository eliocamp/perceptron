
# ENSO --------------------------------------------------------------------
sst <- ERA5_SST() %>%
  ReadNetCDF(vars = c("t" = "sst")) %>%
  normalise_coords() %>%
  .[season(time) == "SON"] %>%
  na.omit() %>%
  .[, .(t = mean(t)), by = .(lon, lat, time = seasonally(time))] %>%
  .[, t := Anomaly(t), by = .(lon, lat)] %>%
  .[, t := t/sd(t)]

y <- sst[lat %between% c(-5, 5) & lon %between% (360 - c(170, 120)),
         mean(t), by = time] %>%
  .[, sign(V1)]

X <- sst %>%
  na.omit() %>%
  .[, id := interaction(lon, lat)]  %>%
  .[order(as.numeric(id))]



X_matrix <- X %>%
  # .[lon > 180] %>%
  dcast(time ~ id, value.var = "t") %>%
  .[, -1] %>%
  as.matrix()

W <- perceptron_train(X_matrix, y, lambda = 25)

X %>%
  # .[lon > 180] %>%
  .[time == time[1]] %>%
  .[, W := ..W[-1]] %>%
  # .[, w_a := Anomaly(W)] %>%
  ggplot(aes(lon, lat)) +
  geom_contour_fill(aes(z = W)) +
  scale_fill_divergent() +
  geom_qmap(color = "black") +
  annotate("rect", xmin = 360-170, xmax = 360-120, ymin = -5, ymax = 5,
           fill = NA, colour = "black") +
  # scale_y_latitude(limits = c(NA, 0)) +
  labs(title = "Perceptrón",
       subtitle = "El Niño")




# Precipitación -----------------------------------------------------------
y_cont <- ReadNetCDF(CMAP(), vars = c(pp = "precip"),
                     subset = list(time = range(X$time),
                                   lon = sesa$lon,
                                   lat = sesa$lat)) %>%
  normalise_coords() %>%
  .[season(time) == "SON"] %>%
  .[, .(pp = mean(pp)), by = .(time = seasonally(time))] %>%
  .[, pp := Anomaly(pp)]

y <- y_cont[, sign(pp)]


X <- era5 %>%
  .[lev == 200] %>%
  .[lat < 0] %>%
  .[, hgt := Anomaly(hgt), by = .(lon, lat)] %>%
  .[, hgt := hgt/sd(hgt)] %>%
  na.omit() %>%
  .[, id := interaction(lon, lat)]  %>%
  .[order(as.numeric(id))]


X_matrix <- X %>%
  # .[lon > 180] %>%
  dcast(time ~ id, value.var = "hgt") %>%
  .[, -1] %>%
  as.matrix()

W <- perceptron_train(X_matrix, y, lambda = 25)


X %>%
  # .[lon > 180] %>%
  .[time == time[1]] %>%
  .[, W := ..W[-1]] %>%
  # .[, w_a := Anomaly(W)] %>%
  ggplot(aes(lon, lat)) +
  geom_contour_fill(aes(z = W)) +
  scale_fill_divergent() +
  geom_qmap(color = "black") +
  annotate("rect", xmin = min(sesa$lon), xmax = max(sesa$lon),
           ymin = min(sesa$lat), ymax = max(sesa$lat),
           fill = NA, colour = "black") +
  scale_y_latitude(limits = c(NA, 0)) +
  labs(title = "Perceptrón",
       subtitle = "Altura geopotential asociada a precipitación\n en Sudeste de Sudamérica")



X[y_cont, on = "time"] %>%
  .[, .(estimate = cor(pp, hgt)), by = .(lon, lat)] %>%
  # rm_intercept() %>%
  ggplot(aes(lon, lat)) +
  geom_contour_fill(aes(z = estimate)) +
  scale_fill_divergent() +
  geom_qmap(color = "black") +
  scale_y_latitude(limits = c(NA, 0))  +
  labs(title = "Correlación",
       subtitle = "Altura geopotential asociada a precipitación\n en Sudeste de Sudamérica")


X %>%
  # .[time == time[1]] %>%
  .[, W := ..W[-1], by = time] %>%
  .[, sum(W*hgt), by = .(time)] %>%
  .[y_cont, on = "time"] %>%
  ggplot(aes(V1, pp)) +
  geom_hline(yintercept = 0, size = 0.3, colour = "gray50" ) +
  geom_vline(xintercept = 0, size = 0.3, colour = "gray50" ) +
  geom_point() +
  scale_x_continuous("Wt*X") +
  scale_y_continuous("Precipitación en\nsudeste de sudamérica")

