here::i_am("figs/map.R")

site <- magick::image_read(here::here("figs/map.png")) %>% magick::image_ggplot()
site0 <- magick::image_read(here::here("figs/locationWOW.jpg")) %>% magick::image_ggplot()

Osa <- magick::image_read(here::here("figs/map_Osa.jpg")) %>% magick::image_ggplot()
pochote <- magick::image_read(here::here("figs/pochote.jpg")) %>% magick::image_ggplot()

#importRaster?
# source(here::here("data/clean.R"))
# parcela <- ggplot2::ggplot(cleanData, aes(x = W, y = N)) + geom_point()
# ggplot2::geom_raster()


sitePlot <- ggpubr::ggarrange(Osa, site0, site, pochote, #pcarcela,
                              labels = c("a", "b", "c", "d"), align = "hv")
# ggsave(filename = "figs/figmap.png", width = 3, height = 3, units = "in")
