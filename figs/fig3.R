library(
  here
)
i_am(
  "figs/fig3.R"
)
#userDiff----
taxRank <- quote(
  # mainDispersal
  successionalStage
)
#re-run
#taxaResultsTbl1
source(
  here(
    "analysis/stats.R"
  )
)



#plan----

# - biomass x dist for main disp mode 1
# - biomass x dist for main disp mode 2
# - disp kernel (y x dist) for main taxon 1
# - disp kernel (y x dist) for main taxon 2


#defs----


measure3a <- quote(
  "NA"
)

xAxisLabel3 <- quote(
  "Distance to edge (m)"
)
yAxisLabel3 <- quote(
  "Aboveground biomass (kg)"
)

statData3clean <- taxaResultsTbl1 %>%
  unnest(
    "varData"
  )
statData3 <- taxaResultsTbl1 %>%
  rename(
    #keepConsistent
    "variable" = mainDispersal
  )



theme_set(
  style
)

#draft----

fig3 <- statData3clean %>%
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


#panel----


fig3a <- statData3clean %>%
  filter(
    is.na(
      mainDispersal
    )
  ) %>%

  # dotGraph()
  ggplot(
    aes(
      x = dist,
      y = {{
        taxMetric
      }}
    )
  ) +
  geom_quasirandom(
    data = cleanData,
    color = "gray"
  ) +
  geom_quasirandom(
    shape = 21,
    fill = "white"
  ) +
  labs(
    x = xAxisLabel3,
    y = yAxisLabel3
  ) +
  scale_y_log10() +
  # theme(
  #   legend.position = "top"
  # ) +
  # geom_smooth(
  #   method = "lm",
  #   se = F
  # ) +
  stat_summary(
    fun.data = "median_mad",
    position = position_dodge(
      width = 25
    )
  ) +
  stat_smooth(
    method = "lm",
    formula = y ~
      poly(
        x,
        2
      ),
    color = "black",
    size = 0.5,
    se = F
  ) +
  stat_poly_eq(
    formula = y ~
      poly(
        x,
        2
      ),
    size = 2,
    label.x = "right",
    label.y = 1.1
  ) +
  annotate(
    "text",
    label = glue(
      "P = ",
      taxaResultsTbl12$
        pval[[2 * 2]] %>%
        last() %>%
        round(
          digits = 3
        )
    ),
    x = 1,
    y = 1.1,
    hjust = -0.5,
    size = 2
  )
fig3a


##draft----
# fig3a <- statData3 %>%
#   oir::dotGraph(
#     measure3a,
#     taxForm[[3]],
#     taxForm[[2]],
#     xAxisLabel3,
#     yAxisLabel3
#   )



#save----


ggsave(
  "fig3.pdf",
  fig3,
  path = "figs",
  width = 3,
  height = 3,
  units = "in"
)

ggsave(
  "fig3.png",
  fig3,
  path = "figs",
  width = 3,
  height = 3,
  units = "in"
)
