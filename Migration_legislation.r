#This file summarises the Australian bird species list, by migration type and cross-national connectivity.
#Associated with PhD chapter "Migration Legislation" 
#Date: 06/05/2016

options(stringsAsFactors=TRUE) # turn off automatic factor coersion
options(scipen=9999)            # turn off plotting axis lables in scientific notation
options(help_type="html")       # help as html

library(plyr)

inpDir <- "C:/Claire/PhD/7 Migration Legislation/species listings/Australian Bird Data Version_for_R_160504.csv"
outFile <- "C:/Claire/PhD/7 Migration Legislation/species listings/Australia_bird_migration_summary.csv"

birds <- read.csv(inpDir)

#select only full species
birds <- subset(birds, birds$Species==1)

#select only core taxa
birds <- subset(birds, birds$Core.taxa==1)
numBirds <- nrow(birds)
print(numBirds)

#select only migratory birds
allmigrants <- droplevels(subset(birds, birds$Movement.within.Aus %in% c("certain", "probable") | birds$Nature.of.international.movements %in% c("annual inward migration - non-marine", "annual outward migration - non-marine", "marine inward", "marine outward", "probable annual outward migration"))) #CLARIFY THIS


#Select migrants that only move within Australia, where any cross-national movement is irregular or unconfirmed
withinAusMig <- droplevels(subset(birds, birds$Movement.within.Aus %in% c("certain", "probable") & Nature.of.international.movements %in% c("irregular outward dispersal", "local dispersal", "none", "possible annual outward migration", "possible irregular outward dispersal", "possible irregular transboundary dispersal", "possible local dispersal", "probable irregular outward dispersal", "probable local dispersal", "vagrancy")))

#select international migrants, where cross-national movement in confirmed
internationalMigrants <- droplevels(subset(birds, birds$Nature.of.international.movements %in% c("annual inward migration - non-marine", "annual outward migration - non-marine", "marine inward", "marine outward", "probable annual outward migration")))

regions <- c("Connectivity.with.Papua.New.Guinea", "Connectivity.with.Indonesia", "Connectivity.with.New.Zealand", "Connectivity.with.Palearctic", "Oceanic")

summaryStats <- ldply(regions, function(x){
	currCol <- which(names(internationalMigrants) == x)
	return(data.frame(	Region = x, 
						Number.of.species = length(which(internationalMigrants[,currCol] %in% c("certain", "probable"))),
						EPBC.Migratory = length(which(internationalMigrants[,currCol] %in% c("certain", "probable") & internationalMigrants$Migratory==1)),
						EPBC.Threatened = length(which(internationalMigrants[,currCol] %in% c("certain", "probable") & internationalMigrants$EPBC.Status.May.2016 %in% c("CR", "EN", "VU"))),
						EPBC.Migratory.and.Threatened = length(which(internationalMigrants[,currCol] %in% c("certain", "probable") & internationalMigrants$EPBC.Status.May.2016 %in% c("CR", "EN", "VU") & internationalMigrants$Migratory==1))
						))
})
#add row for withinAusMig
summaryStats <- rbind(summaryStats, data.frame(
						Region = "Migrate.solely.within.Australia",
						Number.of.species = nrow(withinAusMig),
						EPBC.Migratory = length(which(withinAusMig$Migratory==1)),
						EPBC.Threatened = length(which(withinAusMig$EPBC.Status.May.2016 %in% c("CR", "EN", "VU"))),
						EPBC.Migratory.and.Threatened = length(which(withinAusMig$EPBC.Status.May.2016 %in% c("CR", "EN", "VU") & withinAusMig$Migratory==1))
						))

#add row for overall stats

summaryStats <- rbind(summaryStats, data.frame(
						Region = "Overall",
						Number.of.species = nrow(allmigrants),
						EPBC.Migratory = length(which(allmigrants$Migratory==1)),
						EPBC.Threatened = length(which(allmigrants$EPBC.Status.May.2016 %in% c("CR", "EN", "VU"))),
						EPBC.Migratory.and.Threatened = length(which(allmigrants$EPBC.Status.May.2016 %in% c("CR", "EN", "VU") & allmigrants$Migratory==1))
						))

#calculate percentages
#Overall number as proportion of overall number of mig & non mig birds in Aus, all others as proportion of migratory birds in Aus
summaryStats <- data.frame(summaryStats, 
						round(summaryStats$Number.of.species/c(rep(summaryStats[summaryStats$Region=="Overall", "Number.of.species"], nrow(summaryStats)-1), numBirds)*100, 1),
						round(summaryStats[,c("EPBC.Migratory", "EPBC.Threatened", "EPBC.Migratory.and.Threatened")]/summaryStats[,"Number.of.species"]*100,1)
						)
names(summaryStats)[6:9] <- paste0(names(summaryStats)[2:5], ".percent")


#Save file
write.csv(summaryStats, outFile, row.names=FALSE)

#make a list of png-Aus migrants
pngmigs <- internationalMigrants[internationalMigrants$Connectivity.with.Papua.New.Guinea %in% c("certain", "probable"), c("Taxon.common.name", "Genus.name", "Species.name", "EPBC.Status.May.2016", "Migratory")]
pngmigs$Scientific.name <- paste(pngmigs$Genus.name, pngmigs$Species.name, sep=" ")

write.csv(pngmigs, paste0(dirname(outFile), "/PNG_migrants.csv"), row.names=FALSE)

#make a list of within-Aus migrants
ausmigs <- withinAusMig[withinAusMig$Movement.within.Aus %in% c("certain", "probable"),c("Taxon.common.name", "Genus.name", "Species.name", "EPBC.Status.May.2016", "Migratory")]
ausmigs$Scientific.name <- paste(ausmigs$Genus.name, ausmigs$Species.name, sep=" ")
write.csv(ausmigs, paste0(dirname(outFile), "/WithinAus_migrants.csv"), row.names=FALSE)

#c("annual inward migration - non-marine", "annual outward migration - non-marine", "irregular outward dispersal", "local dispersal", "marine inward", "marine outward", "none", "possible annual outward migration","possible irregular outward dispersal", "possible irregular transboundary dispersal", "possible local dispersal", "probable annual outward migration", "probable irregular outward dispersal", "probable local dispersal", "vagrancy")

#c("Taxon.sort", "Taxon.common.name", "X", "Genus.name", "Species.name", "Subspecies.name", "Species", "Subspecies", "Ultrataxon", "Population.description", "Core.taxa", "Non.breeding.populations.of.core.taxa", "Extinct", "Introduced", "Vagrant", "Supplementary", "Endemic..entirely.in.Australia.", "Endemic..breeding.only.", "Non.breeding.only", "X..in.Australia..breeding.", "X..in.Australia..non.breeding.", "Global.IUCN.status.2014", "Global.IUCN.criteria.2014", "Australian.status.2015", "Australian.status.criteria.2015", "EPBC.Status.May.2016", "EPBC.year.listed", "Bonn.Convention", "CAMBA", "JAMBA", "ROKAMBA", "Migratory", "EPBC.Marine", "ACT.status.2015", "NSW.status.2015", "NT.status.2015", "QLD.status.2015", "SA.status.2015", "TAS.status.2015", "VIC.status.2015", "WA.status.2015", "Also.in.Papua.New.Guinea", "Connectivity.with.Papua.New.Guinea", "Also.in.Indonesia", "Connectivity.with.Indonesia", "Also.in.Timor.Leste", "Connectivity.with.Timor.Leste", "Also.in.New.Zealand","Connectivity.with.New.Zealand", "Also.in.New.Caledonia", "Connectivity.with.New.Caledonia", "Nature.of.international.movements", "Mixing.beyond.Australia", "Level.of.international.genetic.exchange..estimate.", "Level.of.international.genetic.exchange..reliability.", "Migration.within.Australia..Griffieon.2002...Regarded.as.wet.season.migrant.to.C..York.Pen.from.NG..Sednetray.in.s..part.of.range.", "Resident.within.Australia..Chan.2001.", "Migrant.within.Australia..Chan.2001.", "Partial.migrant.within.Australia..Chan.2001.", "Migrant.to.TransFly..from.WWF.report.", "Migrant.to.PNG..from.WWF.report.", "HANZAB", "Birdlife.datazone", "Movement.within.Aus", "Within.Aus.migration.type", "X193..National.movement...local.dispersal..13.", "X194..National.movement...partial.migrant..13.", "X195..National.movement...total.migrant..13.", "X196..National.movement...nomadic...opportunistic..13.", "X197..National.movement...irruptive..13.", "X198..Nature.of.international.movements..13.")
