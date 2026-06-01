# Codex 指令：用 R 计算 ZIP/ZCTA-level travel time 与 market access control

## 0. 项目目标

请在 R 中实现一个可复现 pipeline，用来为 ZIP/ZCTA-level panel data 构造一个 **time-invariant market access control**。研究年份是 2015--2022，但 travel time 只需要算一年的现代路网，因此不需要构造 IHS 之前的路网。

目标输出是一份 ZIP/ZCTA-level 数据：

```text
ZIP | market_access | log_market_access | n_destinations_used | time_source | market_size_year
```

其中：

\[
MA_z = \sum_{k \neq z} market\_size_k \times time_{zk}^{-\theta}
\]

- \(z\)：origin ZIP/ZCTA，即研究样本中的 ZIP/ZCTA。
- \(k\)：destination ZIP/ZCTA，即用于代表市场规模的所有可达 ZIP/ZCTA，最好是 contiguous U.S. 的全部 ZCTA，至少应覆盖主要市场区域。
- \(market\_size_k\)：destination ZIP/ZCTA 的市场规模，例如 payroll、employment、population、GDP proxy 等。
- \(time_{zk}\)：origin 到 destination 的 driving time。
- \(\theta\)：travel-time decay parameter，baseline 用 `1.5`，可做 sensitivity check。

Herzog (2021) 的思路是用地区间 travel time 构造 income/payroll 的 travel-time-discounted sum。这里将其从 county-to-county 改成 ZIP/ZCTA-to-ZIP/ZCTA。

---

## 1. 关键实证设定

### 1.1 只算一年的 travel time

因为这个变量只是作为 market access control，并且研究期是 2015--2022，现代美国道路网络在这段时间的拓扑变化相对有限，所以请计算一次固定的 modern road travel time matrix，并把它作为 time-invariant geography control。

不要构造：

- pre-IHS road network
- 1947 planned IHS network
- year-by-year road network

除非之后另有说明。

### 1.2 不要只用 Interstate Highway System

不要把 travel time 简化为“只沿 IHS 行驶”。应使用现代完整 road network，至少包括 interstate highways、U.S. highways、state highways、major local roads 等。推荐用 OSRM/OpenStreetMap 的 car routing profile。

### 1.3 ZIP 与 ZCTA 的处理

USPS ZIP code 是邮政路线，不是严格地理边界；Census ZCTA 是 ZIP Code Tabulation Area，是实证里常用的 ZIP 地理近似。

请用 ZCTA shapefile 生成 representative point：

- 用 `tigris::zctas()` 获取 ZCTA polygon。
- 用 `sf::st_point_on_surface()` 生成点，而不是直接用 `st_centroid()`。
- 原因：`st_point_on_surface()` 更能保证点落在 polygon 内。

### 1.4 Origin 与 destination 可以不同

如果研究样本只包含部分 ZIP，比如 border ZIP 或 online retailer ZIP，那么：

- origins = 研究样本中的 ZIP/ZCTA。
- destinations = 市场规模数据中所有可用 ZIP/ZCTA，最好是 contiguous U.S. 全部 ZCTA。

不要默认只在样本 ZIP 内互相计算 market access，否则会低估 market access，尤其是样本只覆盖 border ZIP 时。

---

## 2. 输入数据要求

### 2.1 ZIP-level panel data

假设文件路径：

```text
data/raw/zip_panel.csv
```

最低需要字段：

```text
ZIP       # 5-digit ZIP/ZCTA code
YEAR      # panel year, e.g. 2015--2022
```

Codex 应确保 ZIP 被处理为 5 位字符型：

```r
ZIP = stringr::str_pad(as.character(ZIP), width = 5, pad = "0")
```

### 2.2 ZIP-level market size data

假设文件路径：

```text
data/raw/zip_market_size_2017.csv
```

最低需要字段：

```text
ZIP           # 5-digit ZIP/ZCTA code
market_size   # e.g. payroll, employment, population, or GDP proxy
```

建议使用 2015 或 2017 的 pre-period market size，避免用 post-outcome period 的 market size 造成 bad control / endogeneity。若研究 Wayfair 的 post-2018 effect，推荐优先使用 2015 或 2017。

market size 要求：

- 必须非负。
- 缺失值应处理为 0 或删除 destination，具体行为写入 log。
- 如果 market size 为 0，该 destination 对 market access 贡献为 0。

### 2.3 ZCTA shapefile

可用 `tigris::zctas(year = 2020, cb = TRUE)` 下载 2020 cartographic boundary ZCTA。若研究更早年份，也可以使用 2010 ZCTA，但要与 ZIP 数据年份一致性做说明。

---

## 3. R package 需求

请在脚本开头检查并加载以下包：

```r
packages <- c(
  "sf", "dplyr", "data.table", "stringr", "tigris", "osrm", "purrr", "readr"
)
```

若缺失，提示用户安装，而不是在脚本中静默安装。

---

## 4. 推荐文件结构

请生成以下 R scripts：

```text
R/
  00_config.R
  01_make_zcta_points.R
  02_compute_osrm_times.R
  03_compute_market_access.R
  04_merge_to_panel.R
  99_run_all.R

cache/
  zcta_points.rds
  osrm_blocks/

output/
  zip_market_access_2017_theta1p5.csv
  zip_panel_with_market_access.csv
  validation_summary.csv
```

---

## 5. `00_config.R`

请集中管理参数：

```r
# Paths
path_panel        <- "data/raw/zip_panel.csv"
path_market_size  <- "data/raw/zip_market_size_2017.csv"
path_cache_points <- "cache/zcta_points.rds"
path_cache_blocks <- "cache/osrm_blocks"
path_output_ma    <- "output/zip_market_access_2017_theta1p5.csv"
path_output_panel <- "output/zip_panel_with_market_access.csv"

# Geography
zcta_year <- 2020
exclude_state_prefix <- c("02", "15", "60", "66", "69", "72", "78")
# Alaska, Hawaii, American Samoa, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands

# OSRM
osrm_server  <- "http://127.0.0.1:5000/"
osrm_profile <- "car"
block_size_origins <- 25
block_size_destinations <- 500

# Market access
theta <- 1.5
market_size_year <- 2017
self_time_strategy <- "exclude"   # exclude own ZIP from sum
min_time_minutes <- 1              # avoid division by zero if needed
```

---

## 6. `01_make_zcta_points.R`

任务：读取 panel 与 market size，确定 origin/destination ZIP 集合，下载 ZCTA，生成 representative points。

### 6.1 逻辑

1. 读取 `zip_panel.csv`，得到 origin ZIPs。
2. 读取 `zip_market_size_2017.csv`，得到 destination ZIPs。
3. 下载 ZCTA polygons。
4. 保留 contiguous U.S.。
5. 生成 `st_point_on_surface()`。
6. 分别生成 origin points 与 destination points。
7. 保存到 `cache/zcta_points.rds`。

### 6.2 代码骨架

```r
library(sf)
library(dplyr)
library(data.table)
library(stringr)
library(tigris)

source("R/00_config.R")
options(tigris_use_cache = TRUE)

panel <- fread(path_panel) %>%
  mutate(ZIP = str_pad(as.character(ZIP), 5, pad = "0"))

market <- fread(path_market_size) %>%
  mutate(ZIP = str_pad(as.character(ZIP), 5, pad = "0"))

origin_zips <- sort(unique(panel$ZIP))
dest_zips   <- sort(unique(market$ZIP))

zcta <- tigris::zctas(year = zcta_year, cb = TRUE) %>%
  st_transform(4326)

# Handle possible column names across vintages.
zcta_zip_col <- intersect(names(zcta), c("ZCTA5CE20", "ZCTA5CE10", "GEOID20", "GEOID10", "ZCTA5CE"))[1]
if (is.na(zcta_zip_col)) stop("Cannot find ZCTA ZIP code column in ZCTA shapefile.")

zcta <- zcta %>%
  mutate(
    ZIP = str_pad(as.character(.data[[zcta_zip_col]]), 5, pad = "0"),
    state_prefix = substr(ZIP, 1, 2)
  ) %>%
  filter(!state_prefix %in% exclude_state_prefix)

zcta_pts <- zcta %>%
  st_point_on_surface() %>%
  select(ZIP, geometry)

origin_pts <- zcta_pts %>% filter(ZIP %in% origin_zips)
dest_pts   <- zcta_pts %>% filter(ZIP %in% dest_zips)

missing_origins <- setdiff(origin_zips, origin_pts$ZIP)
missing_dests   <- setdiff(dest_zips, dest_pts$ZIP)

message("Missing origin ZIPs from ZCTA shapefile: ", length(missing_origins))
message("Missing destination ZIPs from ZCTA shapefile: ", length(missing_dests))

saveRDS(
  list(
    origin_pts = origin_pts,
    dest_pts = dest_pts,
    missing_origins = missing_origins,
    missing_dests = missing_dests
  ),
  path_cache_points
)
```

---

## 7. `02_compute_osrm_times.R`

任务：用 OSRM 计算 origin ZIP 到 destination ZIP 的 driving time matrix。

### 7.1 OSRM 设置

代码应假设用户已经本地运行 OSRM server。不要依赖公共 demo server 计算大矩阵。

```r
options(osrm.server = osrm_server)
options(osrm.profile = osrm_profile)
```

### 7.2 分块计算

因为 ZIP/ZCTA matrix 可能很大，不要一次性请求全部 origins × destinations。用 block 保存，每个 block 保存成一个 `.rds` 文件，方便断点续算。

每个 block 输出 long format：

```text
origin_zip | dest_zip | duration_min
```

注意：`osrm::osrmTable()` 的返回单位需要检查。R `osrm` 包通常返回分钟，但底层 OSRM API 是秒。请在脚本中加入一个 sanity check 或明确注释。如果用户确认单位为分钟，则字段命名为 `duration_min`。

### 7.3 代码骨架

```r
library(sf)
library(dplyr)
library(data.table)
library(osrm)
library(purrr)

source("R/00_config.R")
options(osrm.server = osrm_server)
options(osrm.profile = osrm_profile)

dir.create(path_cache_blocks, recursive = TRUE, showWarnings = FALSE)
pts <- readRDS(path_cache_points)
origin_pts <- pts$origin_pts
dest_pts   <- pts$dest_pts

make_blocks <- function(n, block_size) {
  split(seq_len(n), ceiling(seq_len(n) / block_size))
}

origin_blocks <- make_blocks(nrow(origin_pts), block_size_origins)
dest_blocks   <- make_blocks(nrow(dest_pts), block_size_destinations)

for (oi in seq_along(origin_blocks)) {
  for (di in seq_along(dest_blocks)) {
    out_file <- file.path(path_cache_blocks, sprintf("time_block_o%04d_d%04d.rds", oi, di))
    if (file.exists(out_file)) {
      message("Skip existing block: ", out_file)
      next
    }

    idx_o <- origin_blocks[[oi]]
    idx_d <- dest_blocks[[di]]

    src <- origin_pts[idx_o, ]
    dst <- dest_pts[idx_d, ]

    message("Computing OSRM block: origins ", min(idx_o), "-", max(idx_o),
            "; destinations ", min(idx_d), "-", max(idx_d))

    tab <- osrm::osrmTable(src = src, dst = dst, measure = "duration")
    dur <- tab$durations

    block_dt <- as.data.table(as.table(dur))
    setnames(block_dt, c("origin_zip", "dest_zip", "duration_min"))

    # Ensure IDs are ZIP strings, not factor levels.
    block_dt[, origin_zip := as.character(origin_zip)]
    block_dt[, dest_zip   := as.character(dest_zip)]
    block_dt[, duration_min := as.numeric(duration_min)]

    saveRDS(block_dt, out_file)
  }
}
```

---

## 8. `03_compute_market_access.R`

任务：读取 OSRM blocks，合并 destination market size，计算 ZIP-level market access。

### 8.1 计算公式

\[
MA_z = \sum_{k \neq z} market\_size_k \times time_{zk}^{-\theta}
\]

其中 `time` 建议用小时：

```r
duration_hour = duration_min / 60
```

如果最后用 `log(MA)`，分钟与小时只是差一个常数比例，但为了可解释性统一用小时。

### 8.2 处理细节

- 如果 `origin_zip == dest_zip`，默认排除。
- 如果 `duration_min <= 0`，设为 missing 或 `min_time_minutes`，但自身 ZIP 已排除。
- 如果 OSRM 返回 NA，说明不可达或失败，删除该 pair，并记录数量。
- market size 缺失时，删除 destination 或设为 0；推荐删除并记录。
- 最后输出 `market_access` 与 `log_market_access`。

### 8.3 代码骨架

```r
library(data.table)
library(dplyr)
library(stringr)

source("R/00_config.R")

market <- fread(path_market_size) %>%
  mutate(
    ZIP = str_pad(as.character(ZIP), 5, pad = "0"),
    market_size = as.numeric(market_size)
  ) %>%
  filter(!is.na(market_size), market_size >= 0) %>%
  select(ZIP, market_size)

market_dt <- as.data.table(market)
setkey(market_dt, ZIP)

block_files <- list.files(path_cache_blocks, pattern = "^time_block_.*\\.rds$", full.names = TRUE)
if (length(block_files) == 0) stop("No OSRM block files found.")

ma_list <- vector("list", length(block_files))

for (b in seq_along(block_files)) {
  message("Processing block ", b, " / ", length(block_files))
  dt <- readRDS(block_files[[b]])
  setDT(dt)

  dt <- dt[!is.na(duration_min)]
  dt <- dt[duration_min > 0]

  if (self_time_strategy == "exclude") {
    dt <- dt[origin_zip != dest_zip]
  }

  dt[, duration_hour := duration_min / 60]

  dt <- merge(dt, market_dt, by.x = "dest_zip", by.y = "ZIP", all.x = FALSE, all.y = FALSE)

  dt[, contribution := market_size * (duration_hour ^ (-theta))]

  ma_list[[b]] <- dt[, .(
    market_access = sum(contribution, na.rm = TRUE),
    n_destinations_used = .N
  ), by = origin_zip]

  rm(dt)
  gc()
}

ma_dt <- rbindlist(ma_list)[, .(
  market_access = sum(market_access, na.rm = TRUE),
  n_destinations_used = sum(n_destinations_used, na.rm = TRUE)
), by = origin_zip]

ma_dt[, log_market_access := log(market_access)]
ma_dt[, theta := theta]
ma_dt[, market_size_year := market_size_year]
ma_dt[, time_source := "OSRM_OpenStreetMap_modern_car"]

setnames(ma_dt, "origin_zip", "ZIP")

fwrite(ma_dt, path_output_ma)
```

---

## 9. `04_merge_to_panel.R`

任务：把 market access merge 回 ZIP-year panel。

```r
library(data.table)
library(dplyr)
library(stringr)

source("R/00_config.R")

panel <- fread(path_panel) %>%
  mutate(ZIP = str_pad(as.character(ZIP), 5, pad = "0"))

ma <- fread(path_output_ma) %>%
  mutate(ZIP = str_pad(as.character(ZIP), 5, pad = "0"))

panel_ma <- panel %>%
  left_join(ma, by = "ZIP")

missing_ma <- sum(is.na(panel_ma$log_market_access))
message("Panel rows missing market access: ", missing_ma)

fwrite(panel_ma, path_output_panel)
```

---

## 10. `99_run_all.R`

任务：顺序运行完整 pipeline。

```r
source("R/00_config.R")
source("R/01_make_zcta_points.R")
source("R/02_compute_osrm_times.R")
source("R/03_compute_market_access.R")
source("R/04_merge_to_panel.R")
```

---

## 11. Validation checks

请生成 `output/validation_summary.csv` 或在 console 打印以下检查：

1. origin ZIP 数量。
2. destination ZIP 数量。
3. missing origin ZIP 数量。
4. missing destination ZIP 数量。
5. OSRM duration missing pair 数量与比例。
6. `market_access` 的 summary statistics。
7. `log_market_access` 的 summary statistics。
8. `n_destinations_used` 的 min/median/max。
9. 抽查几个 ZIP 到大城市 ZIP 的 travel time 是否合理。
10. 检查是否存在 `Inf`、`NaN`、负值。

示例：

```r
summary(ma_dt$market_access)
summary(ma_dt$log_market_access)
summary(ma_dt$n_destinations_used)
stopifnot(!any(is.infinite(ma_dt$log_market_access)))
stopifnot(!any(is.nan(ma_dt$log_market_access)))
```

---

## 12. 性能注意事项

### 12.1 不建议全 ZIP × 全 ZIP 一次性计算

全美国 ZCTA 大约三万多个，完整矩阵约 30,000 × 30,000，接近 9 亿 pairs。若 origins 也是全体 ZCTA，计算与存储都很重。

推荐：

- origins 只用研究样本 ZIP。
- destinations 可以用全部有 market size 的 ZIP。
- 分块请求 OSRM。
- 每个 block 保存为 `.rds`，允许断点续跑。

### 12.2 如果 destinations 过多

如果 local OSRM 无法承受所有 destinations，提供三种 fallback：

1. 只保留 market size 最大的 destinations，比如 payroll top 10,000 ZIP。
2. 按距离截断，比如只考虑 500 miles 或 1000 miles 内的 destinations。
3. 改用 county-level destination market size，把 ZIP origin 到 county destination 算 travel time。

优先级：完整 destinations > 大市场 destinations > 半径截断 > county-level fallback。

### 12.3 local OSRM server

大规模计算必须使用本地 OSRM server。不要对公共 demo server 发大量请求。

Codex 不需要自动搭建 OSRM server，但请在 README 或脚本注释里说明：

```r
options(osrm.server = "http://127.0.0.1:5000/")
options(osrm.profile = "car")
```

---

## 13. Sensitivity checks

请让 `theta` 可以从 config 修改，至少能轻松重跑：

```text
theta = 1.0
theta = 1.5
theta = 2.0
```

输出文件名中要包含 theta，例如：

```text
zip_market_access_2017_theta1p0.csv
zip_market_access_2017_theta1p5.csv
zip_market_access_2017_theta2p0.csv
```

---

## 14. 回归中使用方式

最终在回归里使用：

```r
feols(
  outcome ~ treatment + post + treatment:post + log_market_access + controls |
    ZIP + YEAR,
  data = panel_ma,
  cluster = ~ state
)
```

注意：如果包含 ZIP fixed effects，time-invariant 的 `log_market_access` 会被 ZIP FE 吸收。此时它不能作为单独 control 识别，但可以：

1. 用它和 post 或 year trend 交互：

```r
log_market_access:post
log_market_access:YEAR
```

2. 或在没有 ZIP FE 的规格里作为 geography control。

3. 或用于构造 heterogeneous effects / pre-determined exposure。

如果模型有 ZIP FE + YEAR FE，单独的 `log_market_access` 被自动 dropped 是正常的。

---

## 15. 最终交付物

Codex 最终应生成：

```text
R/00_config.R
R/01_make_zcta_points.R
R/02_compute_osrm_times.R
R/03_compute_market_access.R
R/04_merge_to_panel.R
R/99_run_all.R
output/zip_market_access_2017_theta1p5.csv
output/zip_panel_with_market_access.csv
output/validation_summary.csv
```

并确保代码具备：

- 可重复运行。
- 支持断点续算 OSRM blocks。
- 对 missing ZIP 和 missing durations 有清楚记录。
- 不静默吞掉错误。
- 所有 ZIP 都保持 5 位字符型。
- 所有输出均可直接 merge 到 ZIP-year panel。

---

## 16. 参考依据

- Herzog (2021) 使用地区间 driving time 构造 market access，并将 market access 表示为其他地区 income/payroll 的 travel-time-discounted sum。
- Herzog 的 county-level 实现使用地区 representative points 之间的 fastest driving time，并通过 Dijkstra shortest path 计算 road-network travel time。
- 本项目将该思路改写为 ZIP/ZCTA-level，并使用 OSRM/OpenStreetMap 的现代 road network 计算 driving time。
- R `osrm::osrmTable()` 可用于计算 origins 到 destinations 的 travel time matrix。
- R `tigris::zctas()` 可用于下载 Census ZCTA shapefiles。
