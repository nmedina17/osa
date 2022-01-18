library(
  here
)
i_am(
  "figs/fig3.R"
)
taxRank <- quote(
  mainDispersal
)
#re-run
#taxaResultsTbl1
source(
  here(
    "analysis/stats.R"
  )
)



xAxisLabel3 <- quote(
  "Distance to secondary forest edge (m)"
)
yAxisLabel3 <- quote(
  "Aboveground biomass (kg)"
)
statData3 <- taxaResultsTbl1 %>%
  unnest(
    "varData"
  )



theme_set(
  style
)

fig3 <- statData3 %>%
  # dotGraph()
  ggplot(
    aes(
      x = dist,
      y = {{
        taxMetric
      }},
      color = mainDispersal
    )
  ) +
  geom_quasirandom() +
  labs(
    x = xAxisLabel3,
    y = yAxisLabel3
  ) +
  scale_y_log10() +
  theme(
    legend.position = "top"
  ) +
  # geom_smooth(
  #   method = "lm",
  #   se = F
  # ) +
  stat_summary(
    fun.data = "median_mad",
    position = position_dodge(
      width = 25
    )
  )
fig3
