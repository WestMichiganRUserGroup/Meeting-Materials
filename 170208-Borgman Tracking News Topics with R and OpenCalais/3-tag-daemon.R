#!/usr/bin/env Rscript
# andrew borgman
# tagging all the posts that haven't been tagged
library(RSQLite)
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(Matrix)


# helper functions -------------------------------------------------------------

# thanks to; http://stats.stackexchange.com/questions/49453/calculating-jaccard-or-other-association-coefficient-for-binary-data-using-matri
jaccard <- function(m) {
  A = tcrossprod(m)
  im = which(A > 0, arr.ind=TRUE)
  b = rowSums(m)
  Aim = A[im]
  J = sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
  return(J)
}


# hit the calais API
get_tags <- function(submit_text, calais_token) {
  res <- POST(
    url = "https://api.thomsonreuters.com/permid/calais", 
    body = list(data = submit_text), 
    add_headers('X-AG-Access-Token' = calais_token, 'Content-Type' = 'text/raw', 
                'outputformat' = 'application/json')
  )
  if (res$status_code == 200L) {
    return(fromJSON(content(res, 'text')))
  } else {
    message('api call failed')
    return(NULL)
  }
}





# tagging script ---------------------------------------------------------------

# pulling all of our posts out of our django-planet db
drv <- dbDriver("SQLite")
con <- dbConnect(drv=drv, dbname="/mnt/data/django-planet-demo/db.sqlite3")
posts <- dbGetQuery(con, 'select * from planet_post;')
posts$content <- gsub("<.*?>", "", posts$content)
dbDisconnect(con)

# seeing which tags we already have and create archive of data
curr_tags <- readRDS('tagged-data/latest-tag-object.rds')
saveRDS(curr_tags, paste0('tagged-data/' ,Sys.Date(), '-tag-archive.rds'))

f <- function(x) if (!is.null(x$tag_content)) return(x$id)
complete <- unlist(sapply(curr_tags, f))

to_anno <- setdiff(posts$id, complete)

# gotta tag em alllll
tagged <- vector(mode = 'list', length = length(to_anno))
for (i in seq_along(tagged)) {
  if (i %% 100 == 0) message(i, ' posts tagged thus far...')
  Sys.sleep(1)
  tagged[[i]] <- list(
    id = to_anno[i],
    tag_content = get_tags(
      submit_text = paste(posts$title[to_anno[i]], posts$content[to_anno[i]]),
      calais_token = Sys.getenv('OPEN_CALAIS_KEY') # your open calais API key -- i keep mine in an environtment variable
      )
  )
}

new_tags <- c(curr_tags, tagged)
saveRDS(object = new_tags, file = 'tagged-data/latest-tag-object.rds')

new_tags <- readRDS('tagged-data/latest-tag-object.rds')

# then create the appropriate extract for the shiny app
posts <- posts[!duplicated(posts),]
saveRDS(object = tagged, file = 'tagged-posts-batch-3.rds')

all_tags <- lapply(new_tags, function(x)
  unname(unlist(lapply(x$tag_content, function(y) unname(y$`_typeGroup`))))
)

tops_all <- c("topics", "language", "socialTag", "versions", "entities",
              "industry", "relations")

all_social_tags <- data.table::rbindlist(lapply(new_tags, function(x) {
  to_ret <- try({
    do.call(rbind, lapply(x$tag_content, function(y) {
      if (length(y$`_typeGroup`)) {
        if (y$`_typeGroup` == "socialTag") return(y)
      }
    }))
  }, silent = T)

  if (inherits(to_ret, 'try-error')) {
    to_ret <- NA
  }
  cout <- try({cbind.data.frame(
    data.frame(id = x$id),
    to_ret
  )})
  if (inherits(cout, 'try-error')) {
    cout <- NULL
  }
  return(cout)
}))


alldf <- as.data.frame(all_social_tags)
alldf[] <- lapply(alldf, function(x) unlist(x))
mm <- merge(posts, alldf[,-3], by = 'id')

mm$date_modified <- ymd_hms(mm$date_modified)
mm$date_week <- floor_date(mm$date_modified, unit = 'week')
mm$date_day <- floor_date(mm$date_modified, unit = 'day')


# use our *attractor* method to coalesce topics into groups

tk <- table(mm$name)
tk <- names(tk)[tk > 30]

bad_tags <-
  c(
    "American brands",
    "Americas",
    "Business",
    "Clinical medicine",
    "Culture",
    "Fiction",
    "Health",
    "Health care",
    "Healthcare in the United States",
    "Health in the United States",
    "History of the United States 1991–present)",
    "Hurricane Matthew",
    "Joint Commission",
    "Medicine",
    "Presidency of Lyndon B. Johnson",
    "Rodham family",
    "RTT",
    "S&P/TSX Composite Index",
    "Thomson Reuters",
    "United States",
    "WWE Hall of Fame"
  )

tk <- setdiff(tk, bad_tags)

mtk <- mm[mm$name %in% tk, ]

for_upset <- lapply(split(mtk, mtk$name), function(x) x$id)
fp <- UpSetR::fromList(for_upset)

fpm <- data.matrix(fp)
dists <- jaccard(t(fpm))
dists <- as.matrix(dists)

pdf(paste0('heatmaps/', Sys.Date(), '.pdf'), width = 24, height = 24)
borgmisc::heat_misc(dists, z_score = F, rang = c(0,1), axis_scale = .5, mar_padding = c(6, 7, 0, 0))
dev.off()


colnames(dists) <- rownames(dists) <- colnames(fpm)

dist_ord <- dists
hc <- hclust(as.dist(dists))

topics <- read.delim('topics.tsv', stringsAsFactors = F)

top_list <- do.call(rbind, lapply(1:nrow(topics), function(i) {
  magnet <- topics$Topic[i]
  coors <- dists[,magnet]
  coors <- sort(coors[coors > .15], decreasing = T)
  data.frame(
    Topic = topics$Factor[i],
    Tag = names(coors),
    Jaccard = unname(coors),
    stringsAsFactors = F
  )
}))


topic_stories <- do.call(rbind, lapply(split(top_list, top_list$Topic), function(tf) {
  subber <- mm[mm$name %in% tf$Tag, ]
  subber <- subber[c("id", "title", "url", "guid", "content", "date_modified","date_week", "date_day")  ]
  subber <- subber[!duplicated(subber),]  
  subber$Topic <- tf$Topic[1]
  subber
}))

saveRDS(topic_stories, 'tagged-data/for-shiny.rds')

