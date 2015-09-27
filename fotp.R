require(reshape2)
require(ggplot2)
FILE_NAME = "FOTP scores 1993-2014.csv"

LoadData = function(file_name) {
	fotp_wide = read.csv(file_name, stringsAsFactors = TRUE, sep = ",", quote = "", header = TRUE)
	fotp = melt(data = fotp_wide, id.vars = c("Year"), variable.name = "Country", value.name = "Value")
	fotp$Type = factor(rep("Score", nrow(fotp)))
	fotp_rank_wide = as.data.frame(do.call(rbind, by(fotp_wide, fotp_wide$Year, rank, ties.method = "min")))
	fotp_rank_wide$Year = fotp_wide$Year
	fotp_rank = melt(data = fotp_rank_wide, id.vars = c("Year"), variable.name = "Country", value.name = "Value")
	fotp_rank$Type = factor(rep("Rank", nrow(fotp_rank)))
	rbind(fotp, fotp_rank)
}

PlotData = function(fotp_s, ylabel) {
	g = ggplot(data = fotp_s, aes(x = Year, y = Value, group = Country, colour = Country, shape = Country)) +
		geom_line() +
		geom_point() +
		ylab(ylabel) +
		scale_y_reverse()
	g
}

fotp = LoadData(FILE_NAME)
quartz()
PlotData(subset(fotp, Country %in% c("Japan", "United.States", "Finland") & Type == "Score"), "Freedom of the Press Score")
quartz()
PlotData(subset(fotp, Country %in% c("Japan", "United.States", "Finland") & Type == "Rank"), "Freedom of the Press Rank")
