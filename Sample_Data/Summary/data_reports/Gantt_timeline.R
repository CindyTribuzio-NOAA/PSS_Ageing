



# the below can look really nice, but really hard to get the dates right, based on weeks from Jan 1, need to either cheat it, or use ggplot
library(DiagrammeR)
library(plotly)

install.packages("webshot")
webshot::install_phantomjs()

gannt <- mermaid("
gantt
dateFormat  YYYY-MM-DD
title NPRB 2301 Timeline

section Year 1 (Sept 1, 2023 - Aug 31, 2024)
Project Start                 :done,          year1_1,    2023-09-01, 1d
PI Meeting                    :done,          year1_2,    2024-01-31, 2024-2-03
Initial Delaminations (ARC)   :done,          year1_3,    2023-10-01, 2024-05-01
Rebudget/add AK BioMap        :done,          year1_4,    2024-04-30, 2024-07-10
Update Freezer inventory      :active,        year1_5,    2024-06-24, 2024-08-31
Delamination Training         :done,          year1_6,    2024-08-06, 2024-08-09

section Year 2 (Sept 1, 2024 - Aug 31, 2025)
Training samples to WHOI      :done,          year2_1,    2023-09-11, 24h #note years are faked in the following blocks to make things line up
Production Delamination       :active,        year2_2,    2023-10-28, 2024-08-31
14C preliminary evaluation    :done,          year2_3,    2024-01-10, 2024-02-28
Interim Report                :done,          year2_4,    2024-01-31, 1d
PI Meeting                    :crit, active,  year2_5,    2024-02-26, 2024-02-28
Protein separations           :               year2_6,    2024-03-01, 2024-03-14
14C samples to WHOI           :active,        year2_7,    2024-02-06, 2024-08-31
Stable isotope to Stanford    :active         year2_8,    2024-02-10, 2024-08-31
CSIAA samples to UCSC         :               year2_9,    after year2_6, 5d
Manage Research Workspace     :active,        year2_10,   after year2_6, 180d

section Year 3  (Sept 1, 2024 - Jan 31, 2026)
Ship any remaining samples    :               year3_1,    2023-09-01, 30d
Wrap up billing               :crit,          year3_2,    2024-01-01, 30d
Present at AMSS (date TBD)    :crit,          year3_3,    2024-01-15, 3d
Project End                   :crit,          year3_4,    2024-01-31, 1d

section Bonus Time  (Jan 31, 2026 +)
Finalize Research Workspace   :crit,          extras_1,   2024-01-31, 30d
NPRB final report             :crit,          extras_2,   2024-01-31, 30d
Wrap up manuscripts           :crit,          extras_3,   2024-01-31, 90d
")

plotly::export(gannt, file = "NPRB2301_timeline.png") #saves to main working directory


