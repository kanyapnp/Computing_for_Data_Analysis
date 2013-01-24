csv <- read.csv('hw1_data.csv');
clean <- complete.cases(csv);
o <- csv[clean,][,'Ozone'];
csv1 = csv[clean,];
o31 = (csv1[,'Ozone'] > 31);
csv3 = csv1[o31,];
csv3;
t90 = (csv3[,'Temp'] > 90);
csv4 = csv3[t90,];
csv4
csv5 = csv4[,'Solar.R']
summary(csv5)
str(csv)

temp6 = csv[(csv[,'Month'] == 6),]
temp7 = temp6[complete.cases(temp6),]
summary(temp6)

summary(csv[!is.na(csv[,'Ozone']),'Ozone'])