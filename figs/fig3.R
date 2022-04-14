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
library(
  glue
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
  paste(
    "Biomass (kg ha^-1",
    ")"
  )
)

statData3clean <- taxaResultsTbl1 %>%
  unnest(
    "varData"
  )

statData3 <- taxaResultsTbl1 %>%
  rename(
    #keepConsistent

    # "variable" = mainDispersal
    "variable" = successionalStage
  )



theme_set(
  style
)


#panel----


fig3a <- statData3clean %>%
  filter(
    !is.na(
      # mainDispersal
      successionalStage
    ) &
      successionalStage == "Both"
  ) %>%

  # dotGraph()
  ggplot(
    aes(
      x = dist,
      y = {{
        taxMetric
      }},
      shape = successionalStage
    )
  ) +
  geom_quasirandom(
    data = cleanData %>%
      filter(
        !is.na(
          # mainDispersal
          successionalStage
        ) # &
    #       successionalStage == "Both"
      ),
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
  stat_summary(
    fun.data = "median_mad",
    size = 0.25
    # position = position_dodge(
    #   width = 25
    # )
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
  ) +
  theme(
    legend.position = "top"
  ) +
  labs(
    shape = "Successional stage"
  ) +

  theme(
    #allsmaller
    legend.text = element_text(
      size = 6
    ),
    legend.key.size = unit(
      0.2, "cm"
    ),
    legend.text.align = 0,
    legend.key.width = unit(
      0.2, "cm"
    ),
    legend.title = element_text(
      size = 6
    ),
    legend.margin = margin(
      l = -5,
      t = -5,
      b = -5,
      r = -5
    )
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



#userDiff----


taxRank <- quote(
  mainDispersal
  # successionalStage
)
#re-run
#taxaResultsTbl1
source(
  here(
    "analysis/stats.R"
  )
)


statData3clean <- taxaResultsTbl1 %>%
  unnest(
    "varData"
  )



#panel----


fig3b <- statData3clean %>%
  filter(
    !is.na(
      mainDispersal
      # successionalStage
    )
  ) %>%

  # dotGraph()
  ggplot(
    aes(
      x = dist,
      y = {{
        taxMetric
      }},
      shape = mainDispersal
    )
  ) +
  geom_quasirandom(
    data = cleanData %>%
      filter(
        !is.na(
          mainDispersal
          # successionalStage
        )
      ),
    color = "gray"
    # shape = mainDispersal
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
  theme(
    legend.position = "top",
    #allsmaller
    legend.text = element_text(
      size = 6
    ),
    legend.key.size = unit(
      0.2, "cm"
    ),
    legend.text.align = 0,
    legend.key.width = unit(
      0.2, "cm"
    ),
    legend.title = element_text(
      size = 6
    ),
    legend.margin = margin(
      l = -5,
      t = -5,
      b = -5,
      r = -5
    )
  ) +
  labs(
    shape = "Dispersal"
  ) +
  stat_summary(
    fun.data = "median_mad",
    position = position_dodge(
      width = 25
    ),
    size = 0.25
  )
fig3b



#arrange----

fig3 <- ggarrange(
  fig3a,
  fig3b,
  labels = c(
    "a", "b"
  ),
  ncol = 1
)
fig3


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
