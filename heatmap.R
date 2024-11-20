library(openxlsx)
library(ComplexHeatmap)
library(plyr)
library(dplyr)
library(stringr)
library(circlize)

#import data
db1 = openxlsx::read.xlsx("D:/R project/GAL/Database 1 - top journals 20240805.xlsx",startRow = 1)
db2 = openxlsx::read.xlsx("D:/R project/GAL/Database 2 - whole journals 20240805.xlsx",startRow = 1)
hm1_data = db1%>%dplyr::select(Title,SJR,Region,
                    `A-Provision`:`A-I8`,
                    `R-Provision`:`R-I3`,
                    `COPE`:`WAME`,`Sum-specificity`)
hm1_data <- hm1_data[hm1_data$"A-Provision" == 1|hm1_data$"R-Provision" == 1,]

hm2_data = db2%>%dplyr::select(Title,SJR,Region,
                               `A-Provision`:`A-I8`,
                               `R-Provision`:`R-I3`,
                               `COPE`:`WAME`,`Sum-specificity`)
hm2_data <- hm2_data[hm2_data$"A-Provision" == 1|hm2_data$"R-Provision" == 1,]

#database 1
mat = hm1_data %>%
  select(-c(1:5)) %>%
  mutate_at(vars('A-I1':'A-I8','R-I1':'WAME'),
            ~ ifelse(str_trim(.) %in% c('Y', 'y', 'Yes'), "1",
                     ifelse(str_trim(.) %in% c('No', 'NO', 'N'), "2", "3")))

mat = mat%>%mutate_at(vars(`COPE`,
                     `ICMJE`,
                     `WAME`),
                function(x){ifelse(x==2,3,x)})

mat$Title <- hm1_data$Title
mat <- mat[, !names(mat) %in% "R-specificity"]
mat <- mat[, !names(mat) %in% "Sum-specificity"]
mat <- mat[, !names(mat) %in% "R-Provision"]
colnames(mat) = c('Usage permission','Language editing','Manuscript writing',
                  'Data analysis and interpretation','Image generating',
                  'Fact-checking','Usage documentation',
                  'Authorship eligibility','Usage  permission','Language  editing','Usage  documentation',
                  'COPE','ICMJE','WAME','Title')

annotation_values <- mat$Title
mat <- as.matrix(mat[, -which(names(mat) == "Title")]) 

top_annotation <- columnAnnotation(
  Title = anno_text(annotation_values, rot = 45, just = "right"),
  annotation_name_side = "left",
  annotation_name_gp = gpar(fontsize = 1),
  show_annotation_name = TRUE
)
#heatmap
hm = Heatmap(
  t(mat),
  name = 'Top Journals',
  show_row_names = TRUE,
  show_column_names = TRUE,
  column_labels = annotation_values, 
  col = c('1' = '#2c7fb8', '2' = '#e31a1c', '3' = '#d9d9d9'),
  column_names_side = 'top',
  column_names_gp = gpar(fontsize = 7,rot = 90),
  column_names_rot = 90,
  column_title = 'Journal name',
  column_title_gp = gpar(fontsize = 7),
  row_names_side = 'left',
  row_names_rot = 0,
  row_names_gp = gpar(fontsize = 7), # row names (8+3)
  show_heatmap_legend = FALSE,
  heatmap_legend_param = list(
    labels = c("Yes", "No", "NR"),
    nrow = 3,
    title_position = 'leftcenter'
  ),
  rect_gp = gpar(col = "white", lwd = 0.3),
  row_title_gp = gpar(fontsize = 7),
  row_split = factor(
    c(rep('Author\nguidelines', 8), rep('Reviewer\nguidelines', 3), rep('Reference to\nexternal\nguidelines', 3)),
    levels = c('Author\nguidelines', 'Reviewer\nguidelines', 'Reference to\nexternal\nguidelines')
  ),
  row_gap = unit(c(1, 4), 'mm'),
  row_title_rot = 0,
  border = TRUE
) 
print(hm)
#legend
lgd3 = Legend(labels = c('Yes','No'), legend_gp = gpar(fill =  c('#2c7fb8','#e31a1c')),
              title = "Item instruction",direction='horizontal',title_position='lefttop',nrow=1)
hm <- draw(hm,heatmap_legend_side='bottom',heatmap_legend_list=lgd3)
#extract plot
png('D:/R project/GAL/heatmap1.png',width =  9500,height =5000, res = 600)
draw(hm,heatmap_legend_side='bottom')
dev.off()

#database2
mat2 = hm2_data %>%
  select(-c(1:5)) %>%
  mutate_at(vars('A-I1':'A-I8','R-I1':'WAME'), 
            ~ ifelse(str_trim(.) %in% c('Y', 'y', 'Yes'), "1",
                     ifelse(str_trim(.) %in% c('No', 'NO', 'N'), "2", "3")))

mat2 = mat2%>%mutate_at(vars(`COPE`,
                           `ICMJE`,
                           `WAME`),
                      function(x){ifelse(x==2,3,x)})

mat2$Title <- hm2_data$Title
mat2 <- mat2[, !names(mat2) %in% "R-specificity"]
mat2 <- mat2[, !names(mat2) %in% "Sum-specificity"]
mat2 <- mat2[, !names(mat2) %in% "R-Provision"]
colnames(mat2) = c('Usage permission','Language editing','Manuscript writing',
                  'Data analysis and interpretation','Image generating',
                  'Fact-checking','Usage documentation',
                  'Authorship eligibility','Usage  permission','Language  editing','Usage  documentation',
                  'COPE','ICMJE','WAME','Title')

annotation_values <- mat2$Title
mat_s2 <- mat2
mat2 <- as.matrix(mat2[, -which(names(mat2) == "Title")]) 

top_annotation <- columnAnnotation(
  Title = anno_text(annotation_values, rot = 45, just = "right"),
  annotation_name_side = "left",
  annotation_name_gp = gpar(fontsize = 1),
  show_annotation_name = TRUE
)

#heatmap
hm2 = Heatmap(
  t(mat2),
  name = 'Whole-Spectrum Journals',
  show_row_names = TRUE,
  show_column_names = TRUE,
  column_labels = annotation_values, 
  col = c('1' = '#2c7fb8', '2' = '#e31a1c', '3' = '#d9d9d9'),
  column_names_side = 'top',
  column_names_gp = gpar(fontsize = 7,rot = 90),
  column_names_rot = 90,
  column_title = 'Journal name',
  column_title_gp = gpar(fontsize = 7),
  row_names_side = 'left',
  row_names_rot = 0,
  row_names_gp = gpar(fontsize = 7),
  show_heatmap_legend = FALSE,
  heatmap_legend_param = list(
    labels = c("Yes", "No", "NR"),
    nrow = 3,
    title_position = 'leftcenter'
  ),
  rect_gp = gpar(col = "white", lwd = 0.3),
  row_title_gp = gpar(fontsize = 7),
  row_split = factor(
    c(rep('Author\nguidelines', 8), rep('Reviewer\nguidelines', 3), rep('Reference to\nexternal\nguidelines', 3)),
    levels = c('Author\nguidelines', 'Reviewer\nguidelines', 'Reference to\nexternal\nguidelines')
  ),
  row_gap = unit(c(1, 4), 'mm'),
  row_title_rot = 0,
  border = TRUE
)
print(hm2)
#legend setting
lgd3 = Legend(labels = c('Yes','No'), legend_gp = gpar(fill =  c('#2c7fb8','#e31a1c')),
              title = "Item instruction",direction='horizontal',title_position='lefttop',nrow=1)
hm2 <- draw(hm2,heatmap_legend_side='bottom',heatmap_legend_list=lgd3)

##extract plot
png('D:/R project/GAL/heatmap2.png',width = 5500,height =5000, res = 600)
draw(hm2,heatmap_legend_side='bottom')
dev.off()
