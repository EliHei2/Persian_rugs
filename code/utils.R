# Utilities 

## Libraries
library('tidyverse')
library('magrittr')
library('xlsx')
library('data.table')
library('stringr')
library('patchwork')
library('tidyHeatmap')
library('viridis')
library('knitr')
library('car')

## Pie chart for one feature 
pie_chart <- function(feature){
    rugs_data %>%
    .[, .(pct = dim(.SD)[1]/dim(.)[1]), keyby=feature] %>%
    .[, lab.pose:= 1-(cumsum(pct)-.5*pct)] %>%
    ggplot +
    aes_string(x=1, y='pct', fill=feature) +
    geom_bar(stat='identity', color='white') +
    coord_polar("y", start=0) +
    scale_fill_viridis_d() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5), legend.position='') +
    ggtitle(features[feature, text]) + 
    geom_label(aes(y=lab.pose, label=paste(as.integer(pct*100), '%')), fill='white')
}

## Density plots
dens_plot <- function(feature){
    rugs_data %>% ggplot + 
    aes_string(y='group', x='score', fill=feature) +
    geom_density_ridges(jittered_points = TRUE, scale =.8, rel_min_height = .01,
    point_size = 0.5, size = 0.25, alpha=0.5, point_alpha=0.75,
    position=position_points_jitter(height=.2, width=.2)) +
    theme_bw() +
    theme(legend.position='') +
    facet_grid(as.formula(paste0(feature, '~ .')))
    # , labeller=labeller(degree=labs)) 
}

## bar plots
bar_plot <- function(feature){
    rugs_sum = rugs_data[, .(mean_score = mean(score), 
        ci_score = sd(score)/sqrt(length(score)) * qt((1-0.05)/2 + .5, length(score)-1)),
        by=c('group', feature)] %>%
        .[, ci_score := ifelse(ci_score > mean_score, 0, ci_score)]

    bar1 = rugs_sum %>% ggplot + 
    aes_string(x=feature, y='mean_score', fill='group') +
    geom_bar(stat='identity', position=position_dodge(), color='black')  +
    geom_errorbar(aes(ymin=mean_score-ci_score, ymax=mean_score+ci_score), width=0.1,
                 position=position_dodge(.9)) +
    theme_bw() +
    labs(fill='Experimental Group')

    bar2 = rugs_sum %>% ggplot + 
    aes_string(x='group', y='mean_score', fill=feature) +
    geom_bar(stat='identity', position=position_dodge(), color='black')  +
    geom_errorbar(aes(ymin=mean_score-ci_score, ymax=mean_score+ci_score), width=0.1,
                 position=position_dodge(.9)) +
    scale_fill_viridis_d() +
    theme_bw() +
    labs(fill=features[feature,text])

    list(bar1=bar1, bar2=bar2)
}

# compute p-value with kruskal.test
p_value <- function(feature){
    print(feature)
    res1 = kruskal.test(as.formula(paste0('score ~ ', feature)), data = rugs_data[group==1])$p.value
    res2 = kruskal.test(as.formula(paste0('score ~ ', feature)), data = rugs_data[group==2])$p.value
    res3 = kruskal.test(as.formula(paste0('score ~ ', feature)), data = rugs_data[group==3])$p.value
    res = kruskal.test(as.formula(paste0('score ~ ', feature)), data = rugs_data)$p.value
    p_vals = c(res1, res2, res3, res) %>% round(., digits = 4)
    p_vals[which(p_vals < 0.005)] = paste(p_vals[which(p_vals < 0.005)], '***')
    p_vals[which(as.numeric(p_vals) < 0.01)] = paste(p_vals[which(p_vals < 0.01)], '**')
    p_vals[which(as.numeric(p_vals) < 0.05)] = paste(p_vals[which(p_vals < 0.05)], '*')
    p_vals
}

