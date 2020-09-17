load('sample_df.Rda')
df = sample_df


Seed = 1368


###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
# First splitting method

id_based_split = function(data, Seed) {
  samples = lapply(split(data, data$id ), function(x) {
    set.seed(Seed)
    s = sample(seq_len(nrow(x)), size=floor(nrow(x)*0.75000))
    x[s,]
  })
  
  train = do.call(rbind, samples)
  
  samples = lapply(split(data, data$id), function(x) {
    set.seed(Seed)
    s = sample(seq_len(nrow(x)), size=floor(nrow(x)*0.75000))
    s_test = setdiff(1:nrow(x), s)
    x[s_test,]
  })
  
  test = do.call(rbind, samples)
  
  out = list("train" = train,
              "test" = test)
  return(out)
  
}

tt_idSplit_omitNA = id_based_split(df, Seed)

train1 = tt_idSplit_omitNA$train
test1 = tt_idSplit_omitNA$test






###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
# Second splitting method


alt_split = function(data, Seed) {
  set.seed(Seed)
  ids = unique(data$id)
  inds = numeric()
  
  for(i in ids) {
    inds = c(inds, sample(seq_len(nrow(data))[data$id==i], 
                          size = floor(sum(data$id==i)*0.75000)))
  }
  inds
}

inds = alt_split(df, Seed)
train2 = df[inds, ]
test2 = df[-inds, ]


mean(train1$lagHungry1)
mean(train2$lagHungry1)




mean(test1$lagHungry1)
mean(test2$lagHungry1)











