
#' have to load the data.table package to use it
suppressPackageStartupMessages({
    require(data.table)
})

#' this is an approach to having your script receive arguments from the command line,
#' while still having something you can use in Rstudio
#' 
#' using R from the command line is a must in high-performance computing setups
#' which is typical for large scale computational work
.args <- if (interactive()) c(
    "2010_BSA_Carrier_PUF.csv",
    "transformed_data.csv"
) else commandArgs(trailingOnly = TRUE)
infile <- .args[1] #' use the first argument (either from command line arguments or interactive defaults)
otfile <- tail(.args, 1) #' use the last argument

#' this is a placeholder for the tutorial, indicating where *you* need to provide code
`???` <- function(directions, ln, ...) warning(sprintf("on line %i: %s", ln, directions))

#' one of the advantages of data.table is performance, so throughout this tutorial you will
#' `system.time` around expressions, to show how long something takes
system.time(
    df <- read.csv(infile)
)

system.time(
    dt <- fread(infile)
)

#' BASIC DATA REVIEW

#' you will typically want to actually look at datasets; data.table presents differently from data.frame:
print(df)
print(dt)

#' you will also likely want to look at particular columns, e.g.
df[, c("BENE_SEX_IDENT_CD", "BENE_AGE_CAT_CD")]
#' this exact syntax also works in data.table
dt[, c("BENE_SEX_IDENT_CD", "BENE_AGE_CAT_CD")]
#' but there are additional approaches:
dt[, .(BENE_SEX_IDENT_CD, BENE_AGE_CAT_CD)]

#' we might want to know what the unique combinations of some columns are:
system.time(
    unique(df[, c("BENE_SEX_IDENT_CD", "BENE_AGE_CAT_CD")])
)
system.time(
    unique(dt[, .(BENE_SEX_IDENT_CD, BENE_AGE_CAT_CD)])
)

#' or how many entries correspond to those unique combinations:
system.time(
    aggregate(cbind(N=CAR_LINE_ICD9_DGNS_CD) ~ BENE_SEX_IDENT_CD + BENE_AGE_CAT_CD, df, length)
)
#' alternative base R approaches:
#' this one doesn't return a data.frame
bydf <- by(df, df[, c("BENE_SEX_IDENT_CD", "BENE_AGE_CAT_CD")], function(sdf) dim(sdf[2]))
#' though you can combine all the elements, but this doesn't preserve the group labels:
Reduce(rbind, bydf)
#' a `by` object has matrix dimensions, so you could iterate over those names while combining, etc, etc
#' we could also take the uniques we got earlier, iterate over the rows, and use a subsetting count

system.time(
    dt[, .N, .(BENE_SEX_IDENT_CD, BENE_AGE_CAT_CD)]
)

#' WIDE VS LONG DATA

#' say we want to keep our sex / age coding, but then have every other variable in long format
system.time(
    long.df <- reshape(
        df,
        times = setdiff(names(df), c("BENE_SEX_IDENT_CD", "BENE_AGE_CAT_CD")),
        timevar = "measure",
        varying = list(setdiff(names(df),c("BENE_SEX_IDENT_CD", "BENE_AGE_CAT_CD"))),
        direction = "long"
    )
)
#' we could also do it with a loop:
system.time({
    other.long.df <- df[,1:3]
    names(other.long.df)[3] <- "measure"
    for (col in (1:dim(df)[2])[-(1:3)]) {
        add.df <- df[,c(1:2,col)]
        names(add.df) <- names(other.long.df)
        other.long.df <- rbind(other.long.df, add.df, deparse.level = 0)
    }  
})

system.time(
    long.dt <- melt(dt, id.vars = c("BENE_SEX_IDENT_CD", "BENE_AGE_CAT_CD"), variable.name = "measure")
)
#' note: data.table warns about funny business, reshape does not

#' FILTERING

#' we often want to use a subset of data that meets some criteria
system.time(
    sub.df <- subset(df, CAR_LINE_PRVDR_TYPE_CD == 5 & CAR_LINE_ICD9_DGNS_CD != "")
)
system.time(
    sub.dt <- dt[CAR_LINE_PRVDR_TYPE_CD == 5 & CAR_LINE_ICD9_DGNS_CD != ""]
)


#' GROUPING
system.time(
    print(aggregate(cbind(val=CAR_HCPS_PMT_AMT) ~ BENE_SEX_IDENT_CD + BENE_AGE_CAT_CD, df, mean))
)
system.time(
    print(dt[, .(val = mean(CAR_HCPS_PMT_AMT)), by=.(BENE_SEX_IDENT_CD, BENE_AGE_CAT_CD)])
)
#' data.table also provides an easy interface for doing a variety of summary computations per group
system.time(
    print(dt[, .(
        val = mean(CAR_HCPS_PMT_AMT), sd.val = sd(CAR_HCPS_PMT_AMT), .N
    ), by=.(BENE_SEX_IDENT_CD, BENE_AGE_CAT_CD)])
)

#' ORDERING
system.time(
    print(df[do.call(order, df[,c("BENE_SEX_IDENT_CD", "BENE_AGE_CAT_CD", "CAR_HCPS_PMT_AMT")]), ])
)
system.time(
    print(dt[order(BENE_SEX_IDENT_CD, BENE_AGE_CAT_CD, CAR_HCPS_PMT_AMT)])
)
#' data.table supports several other ordering approaches; need to reset dt to be useful comparison
dt <- fread(infile)
system.time(
    print(setorder(dt, BENE_SEX_IDENT_CD, BENE_AGE_CAT_CD, CAR_HCPS_PMT_AMT))
)
dt <- fread(infile)
system.time(
    print(setkey(dt, BENE_SEX_IDENT_CD, BENE_AGE_CAT_CD, CAR_HCPS_PMT_AMT))
)
dt <- fread(infile)
system.time(
    print(dt[,.SD,keyby=.(BENE_SEX_IDENT_CD, BENE_AGE_CAT_CD, CAR_HCPS_PMT_AMT)])
)

#' TRANSFORMING
system.time(
    df$NEWCOL <- df$CAR_HCPS_PMT_AMT / df$CAR_LINE_CNT
)
#' or
system.time(
    df <- within(df, NEWCOL <- CAR_HCPS_PMT_AMT / CAR_LINE_CNT)
)
system.time(
    dt[, NEWCOL := CAR_HCPS_PMT_AMT / CAR_LINE_CNT ]
)

#' COMBINATIONS

#' some data operations may entail all of the above
system.time(
    print({
        sub.df <- subset(df,BENE_SEX_IDENT_CD == 1 & BENE_AGE_CAT_CD > 3)
        ord.df <- sub.df[do.call(order, sub.df[,c("BENE_AGE_CAT_CD", "CAR_HCPS_PMT_AMT")]), ]
        agg.df <- Reduce(rbind, by(ord.df, ord.df$BENE_AGE_CAT_CD, function(sdf) within(
            sdf, CUM_CAR_HCPS_PMT_AMT <- cumsum(CAR_HCPS_PMT_AMT)
        )[, c("BENE_AGE_CAT_CD", "CAR_HCPS_PMT_AMT", "CUM_CAR_HCPS_PMT_AMT")]))
        fin.df <- Reduce(rbind, by(agg.df, agg.df$BENE_AGE_CAT_CD, function(sdf) {
            n <- dim(sdf)[1]
            cross_ind <- which.max(with(sdf, CUM_CAR_HCPS_PMT_AMT/CUM_CAR_HCPS_PMT_AMT[n]) > .8)
            with(sdf, data.frame(
                BENE_AGE_CAT_CD = BENE_AGE_CAT_CD[1],
                N=n, TOT_PMT_AMT = CUM_CAR_HCPS_PMT_AMT[n],
                CROSSN = cross_ind,
                CROSS_PMT_AMT = CAR_HCPS_PMT_AMT[cross_ind],
                CUM_CROSS_PMT_AMT = CUM_CAR_HCPS_PMT_AMT[cross_ind]
            ))
        }))
    })
)
system.time(
    print(fin.dt <- dt[
        BENE_SEX_IDENT_CD == 1 & BENE_AGE_CAT_CD > 3,
        .SD[order(CAR_HCPS_PMT_AMT), .(CAR_HCPS_PMT_AMT, CUM_CAR_HCPS_PMT_AMT = cumsum(CAR_HCPS_PMT_AMT))],
        keyby = .(BENE_AGE_CAT_CD)
    ][,{
        cross_ind <- which.max(CUM_CAR_HCPS_PMT_AMT/CUM_CAR_HCPS_PMT_AMT[.N] > .8)
        .(
            .N,
            TOT_PMT_AMT = CUM_CAR_HCPS_PMT_AMT[.N],
            CROSSN = cross_ind,
            CROSS_PMT_AMT = CAR_HCPS_PMT_AMT[cross_ind],
            CUM_CROSS_PMT_AMT = CUM_CAR_HCPS_PMT_AMT[cross_ind]
        )
    }, keyby = .(BENE_AGE_CAT_CD)
    ])
)

write.csv(fin.df)
fwrite(fin.dt)