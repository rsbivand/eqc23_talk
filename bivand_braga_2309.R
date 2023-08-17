

## ---- size="scriptsize"------------------------------------------------------------------
com <- sf::st_read("data/lot46.gpkg", layer = "commune")


## ---- size="scriptsize"------------------------------------------------------------------
bigcom <- com[com$POPULATION > 2000,]
bigcom <- bigcom[rev(order(bigcom$NOM_COM)),]


## ---- size="scriptsize"------------------------------------------------------------------
(com$POPULATION / (sf::st_area(com) |> units::set_units(km^2))) |>
    as.numeric() -> com$DENS
vars <- c("AGR_H", "AGR_F", "IND_H", "IND_F", "BTP_H", "BTP_F", "TER_H", "TER_F")
com |> subset(select=vars, drop=TRUE) |> apply(1, sum) |>
    unname() -> com$POP_ACT
com$SHARE_ACT <- 100 * com$POP_ACT / com$POPULATION


## ---- size="scriptsize"------------------------------------------------------------------
ccom <- cartogram::cartogram_cont(com, weight="POPULATION", prepare = "none")


## ---- echo=FALSE-------------------------------------------------------------------------
opar <- par(mfrow=c(2,2))
plot(density(com$DENS))
rug(com$DENS, col="grey")
plot(density(com$SHARE_ACT))
rug(com$SHARE_ACT, col="grey")
hist(com$DENS)
hist(com$SHARE_ACT)
par(opar)


## ---- size="scriptsize"------------------------------------------------------------------
c(DENS=moments::agostino.test(com$DENS)$p.value < 0.005,
    SHARE_ACT=moments::agostino.test(com$SHARE_ACT)$p.value < 0.005)


## ---- echo=FALSE-------------------------------------------------------------------------
library(ggplot2)
pal <- hcl.colors(313, palette="Dark Mint")
ggplot(com) + geom_sf(aes(fill=DENS)) + theme_void() + 
    scale_fill_gradientn(colors=rev(pal)) + 
    geom_sf_label(data=bigcom, aes(label=NOM_COM))


## ---- echo=FALSE-------------------------------------------------------------------------
ggplot(com) + geom_sf(aes(fill=SHARE_ACT)) + theme_void() + 
    scale_fill_gradientn(colors=rev(pal)) + 
    geom_sf_label(data=bigcom, aes(label=NOM_COM))


## ---- echo=FALSE-------------------------------------------------------------------------
library(mapsf)
mf_map(com, var="SHARE_ACT", type="choro", breaks="equal", nbreaks=7, pal="Dark Mint", lwd=0.5, leg_title="percent economically active")
mf_label(bigcom, var="NOM_COM", col="wheat4", cex=0.9, overlap=FALSE, halo=TRUE, bg="wheat1", r=0.05)
mf_title("Economically active population share, Lot, 2020")


## ---- echo=FALSE-------------------------------------------------------------------------
mf_map(com, var="SHARE_ACT", type="choro", breaks="pretty", nbreaks=7, pal="Dark Mint", lwd=0.5, leg_title="percent economically active")
mf_label(bigcom, var="NOM_COM", col="wheat4", cex=0.9, overlap=FALSE, halo=TRUE, bg="wheat1", r=0.05)
mf_title("Economically active population share, Lot, 2020")


## ---- echo=FALSE-------------------------------------------------------------------------
mf_map(com, var="SHARE_ACT", type="choro", breaks="sd", nbreaks=6, pal="Dark Mint", lwd=0.5, leg_title="percent economically active")
mf_label(bigcom, var="NOM_COM", col="wheat4", cex=0.9, overlap=FALSE, halo=TRUE, bg="wheat1", r=0.05)
mf_title("Economically active population share, Lot, 2020")


## ---- echo=FALSE-------------------------------------------------------------------------
mf_map(ccom, var="SHARE_ACT", type="choro", breaks="quantile", nbreaks=7, pal="Dark Mint", lwd=0.5, leg_title="inhabitants per km2")
mf_label(ccom[com$POPULATION > 2000,], var="NOM_COM", col="wheat4", cex=0.9, overlap=FALSE, halo=TRUE, bg="wheat1", r=0.05)
mf_title("Economically active population share, Lot, 2020, Dougenik cartogram")


## ---- echo=FALSE-------------------------------------------------------------------------
library(classInt)
sd_SA <- classIntervals(com$SHARE_ACT, n=6, style="sd")
pretty_SA <- classIntervals(com$SHARE_ACT, n=6, style="pretty")
equal_SA <- classIntervals(com$SHARE_ACT, n=6, style="equal")


## ---- echo=FALSE-------------------------------------------------------------------------
library(modelsummary)
datasummary_crosstab(sd ~ pretty, 1 ~ 1 + N, data.frame(sd=findCols(sd_SA), pretty=findCols(pretty_SA)), title="Standard deviation by Pretty", output="markdown")


## ---- echo=FALSE-------------------------------------------------------------------------
opar <- par(mfrow=c(2,3), mar=c(3, 4, 4, 2))
pal <- rev(hcl.colors(length(pretty_SA$brks)-1L, palette="Dark Mint"))
plot(pretty_SA, pal=pal, main="Pretty", xlab="")
pal <- rev(hcl.colors(length(equal_SA$brks)-1L, palette="Dark Mint"))
plot(equal_SA, pal=pal, main="Equal intervals", xlab="")
pal <- rev(hcl.colors(length(sd_SA$brks)-1L, palette="Dark Mint"))
plot(sd_SA, pal=pal, main="Standard deviation", xlab="")
quantile_SA <- classIntervals(com$SHARE_ACT, n=6, style="quantile")
pal <- rev(hcl.colors(length(quantile_SA$brks)-1L, palette="Dark Mint"))
plot(quantile_SA, pal=pal, main="Quantile", xlab="")
fisher_SA <- classIntervals(com$SHARE_ACT, n=6, style="fisher")
pal <- rev(hcl.colors(length(fisher_SA$brks)-1L, palette="Dark Mint"))
plot(fisher_SA, pal=pal, main="Fisher", xlab="")
headtails_SA <- classIntervals(com$SHARE_ACT, n=6, style="headtails")
pal <- rev(hcl.colors(length(headtails_SA$brks)-1L, palette="Dark Mint"))
plot(headtails_SA, pal=pal, main="Headtails", xlab="")
par(opar)


## ---- echo=FALSE-------------------------------------------------------------------------
mf_map(com, var="DENS", type="choro", breaks="quantile", nbreaks=7, pal="Dark Mint", lwd=0.5, leg_title="inhabitants per km2")
mf_label(bigcom, var="NOM_COM", col="wheat4", cex=0.9, overlap=FALSE, halo=TRUE, bg="wheat1", r=0.05)
mf_title("Population density, Lot, 2020")


## ---- echo=FALSE-------------------------------------------------------------------------
mf_map(com, var="DENS", type="choro", breaks="fisher", nbreaks=7, pal="Dark Mint", lwd=0.5, leg_title="inhabitants per km2")
mf_label(bigcom, var="NOM_COM", col="wheat4", cex=0.9, overlap=FALSE, halo=TRUE, bg="wheat1", r=0.05)
mf_title("Population density, Lot, 2020")


## ---- echo=FALSE-------------------------------------------------------------------------
mf_map(ccom, var="DENS", type="choro", breaks="fisher", nbreaks=7, pal="Dark Mint", lwd=0.5, leg_title="inhabitants per km2")
mf_label(ccom[com$POPULATION > 2000,], var="NOM_COM", col="wheat4", cex=0.9, overlap=FALSE, halo=TRUE, bg="wheat1", r=0.05)
mf_title("Population density, Lot, 2020, Dougenik cartogram")


## ---- echo=FALSE-------------------------------------------------------------------------
quantile_D <- classIntervals(com$DENS, n=7, style="quantile")
fisher_D <- classIntervals(com$DENS, n=7, style="fisher")


## ---- echo=FALSE-------------------------------------------------------------------------
datasummary_crosstab(quantile ~ fisher, 1 ~ 1 + N,  data.frame(quantile=findCols(quantile_D), fisher=findCols(fisher_D)), title="Fisher by Quantile", output="markdown")


## ---- echo=FALSE-------------------------------------------------------------------------
mf_map(com, var="DENS", type="choro", breaks="geom", nbreaks=7, pal="Dark Mint", lwd=0.5, leg_title="inhabitants per km2")
mf_label(bigcom, var="NOM_COM", col="wheat4", cex=0.9, overlap=FALSE, halo=TRUE, bg="wheat1", r=0.05)
mf_title("Population density, Lot, 2020")


## ---- echo=FALSE-------------------------------------------------------------------------
gbrks <- mapsf::mf_get_breaks(com$DENS, breaks="geom", k=7)
geom_D <- classIntervals(com$DENS, style="fixed", fixedBreaks=gbrks)
datasummary_crosstab(fisher ~ geom, 1 ~ 1 + N, data.frame(geom=findCols(geom_D), fisher=findCols(fisher_D)), title="Fisher by Geometric progression", output="markdown")


## ---- echo=FALSE, results="hide"---------------------------------------------------------
opar <- par(mfrow=c(2,3), mar=c(3, 4, 4, 2))
pal <- rev(hcl.colors(length(quantile_D$brks)-1L, palette="Dark Mint"))
plot(quantile_D, pal=pal, main="Quantile", xlab="")
pal <- rev(hcl.colors(length(fisher_D$brks)-1L, palette="Dark Mint"))
plot(fisher_D, pal=pal, main="Fisher", xlab="")
pal <- rev(hcl.colors(length(geom_D$brks)-1L, palette="Dark Mint"))
plot(geom_D, pal=pal, main="Geometric progression", xlab="")
set.seed(1)
bclust_D <- classIntervals(com$DENS, n=7, style="bclust")
pal <- rev(hcl.colors(length(bclust_D$brks)-1L, palette="Dark Mint"))
plot(bclust_D, pal=pal, main="Bagged Clustering", xlab="")
set.seed(1)
kmeans_D <- classIntervals(com$DENS, n=7, style="kmeans")
pal <- rev(hcl.colors(length(kmeans_D$brks)-1L, palette="Dark Mint"))
plot(kmeans_D, pal=pal, main="K-means", xlab="")
headtails_D <- classIntervals(com$DENS, n=7, style="headtails")
pal <- rev(hcl.colors(length(headtails_D$brks)-1L, palette="Dark Mint"))
plot(headtails_D, pal=pal, main="Headtails", xlab="")
par(opar)


## ---- echo=FALSE-------------------------------------------------------------------------
datasummary_crosstab(fisher ~ kmeans, 1 ~ 1 + N, data.frame(kmeans=findCols(kmeans_D), fisher=findCols(fisher_D)), title="Fisher by K-means", output="markdown")


## ---- echo=FALSE, results="hide"---------------------------------------------------------
com_sv <- terra::vect(com)
opar <- par(mfrow=c(1,2))
terra::plot(com_sv, "DENS", type="continuous", col=rev(hcl.colors(312, palette="Dark Mint")),
    lwd=0.5, main="Population density", axes=FALSE)
terra::plot(com_sv, "SHARE_ACT", type="continuous", col=rev(hcl.colors(312, palette="Dark Mint")),
    lwd=0.5, main="Economically active", axes=FALSE)
par(opar)


## ---- echo=FALSE-------------------------------------------------------------------------
opar <- par(mfrow=c(1,2))
terra::plot(com_sv, "DENS", type="interval", col=rev(hcl.colors(7, palette="Dark Mint")),
    breaks=7, breakby="cases", lwd=0.5, main="Population density", axes=FALSE)
terra::plot(com_sv, "SHARE_ACT", type="interval", col=rev(hcl.colors(7, palette="Dark Mint")),
    breaks=7, breakby="cases", lwd=0.5, main="Economically active", axes=FALSE)
par(opar)


## ---- echo=FALSE-------------------------------------------------------------------------
opar <- par(mfrow=c(1,2))
terra::plot(com_sv, "DENS", type="interval", col=rev(hcl.colors(7, palette="Dark Mint")),
    breaks=classInt::classIntervals(com_sv$DENS, n=7, style="fisher")$brks, 
    lwd=0.5, main="Population density", axes=FALSE)
terra::plot(com_sv, "SHARE_ACT", type="interval", col=rev(hcl.colors(7, palette="Dark Mint")),
    breaks=classInt::classIntervals(com_sv$SHARE_ACT, n=7, style="fisher")$brks, 
    lwd=0.5, main="Economically active", axes=FALSE)
par(opar)


## ---- echo=FALSE-------------------------------------------------------------------------
gbrks <- mapsf::mf_get_breaks(com_sv$DENS, breaks="geom", k=7)
terra::plot(com_sv, "DENS", type="interval", breaks=gbrks, main="Population density",
    col=rev(hcl.colors(length(gbrks)-1, palette="Dark Mint")), lwd=0.5, axes=FALSE)

