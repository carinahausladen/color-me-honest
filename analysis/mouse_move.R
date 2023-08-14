'script plots mouse movements; run after executing first junk of main.qmd.'

palette <- colorRampPalette(c('yellow', 'red'))
dcolors <- c("deepskyblue", "forestgreen", "coral4", "deeppink2", "orange", "gray")
dvalues <- c(49:54)

raw<- raw_nov
raw<- raw_jun

#resort the dataset for easier visual inspection
sorted <- with(raw, order(player.treat_color, player.treat_pressure, player.sex, player.cblind))
raw <- raw[c(sorted),]

#exclude the subjects below:
raw %>%  filter(!player.report == "R") ->raw

#
raw$treatment<- ifelse(raw$player.treat_color==1 & raw$player.treat_pressure==1, "CP", 
                      ifelse(raw$player.treat_color==1 & raw$player.treat_pressure==0, "CNP", 
                             ifelse(raw$player.treat_color==0 & raw$player.treat_pressure==1, "NCP",
                                    ifelse(raw$player.treat_color==0 & raw$player.treat_pressure==0, "NCNP", "NA"))))

raw %>%
left_join(read_csv("/Users/carinah/Documents/GitHub/color-me-honest/analysis/totaldist-annotated.csv",
                   show_col_types = FALSE)%>% select(participant.code, total_distance, type), 
          by="participant.code") %>%
  mutate(participant.code = fct_reorder(factor(participant.code),total_distance, .na_rm = TRUE)) ->raw


#plt_mouse <- function(treatment_in,type_in) {
plt_mouse <- function(treatment_in) {
  
  raw %>% 
    filter(treatment == treatment_in) %>%
    #filter(type == type_in) %>%
    arrange(total_distance) ->raw_temp
  
  for (i in 1:length(unique(raw_temp$participant.code))) {
    
    x <- as.numeric(unlist(strsplit(as.character(raw_temp$player.mouse_x[i]), split = ",")))
    y <- as.numeric(unlist(strsplit(as.character(raw_temp$player.mouse_y[i]), split = ",")))
    t <- as.numeric(unlist(strsplit(as.character(raw_temp$player.mouse_t[i]), split = ",")))
    
    color <- palette(100)[as.numeric(cut(t, breaks = 100))]
    with(raw_temp[i,], plot(y~x, pch = ".", 
                       xlim = c(player.butt_fl, player.butt_cr), 
                       ylim = c(player.butt_at, player.butt_eb), 
                       col = color, 
                       asp = 1, 
                       axes = FALSE, ann = FALSE, frame.plot = TRUE))

    dice <- data.frame()
    for (j in 1:6) {
      dice[j,1] <- as.integer(substr(raw_temp$player.faces[i], 2 + (j-1)*3, 2 + (j-1)*3))
      dice[j,2] <- ifelse(raw_temp$player.treat_color[i] == 1, dcolors[dice[j,1]], "gray")
      dice[j,3] <- as.integer(substr(raw_temp$player.values[i], 2 + (j-1)*3, 2 + (j-1)*3))
      dice[j,4] <- dvalues[dice[j,3]]
    }
    
    #plot overlay and legends
    overlay_x <- with(raw_temp[i,], c(player.butt_al, player.butt_ar, player.butt_bl, player.butt_br, player.butt_cl, player.butt_cr, player.butt_dl, player.butt_dr, player.butt_el, player.butt_er, player.butt_fl, player.butt_fr))
    overlay_y <- with(raw_temp[i,], c(player.butt_at, player.butt_ab, player.butt_bt, player.butt_bb, player.butt_ct, player.butt_cb, player.butt_dt, player.butt_db, player.butt_et, player.butt_eb, player.butt_ft, player.butt_fb))
    for (j in 1:6) {
      rect(overlay_x[1 + (j-1)*2], 
           overlay_y[2 + (j-1)*2], 
           overlay_x[2 + (j-1)*2], 
           overlay_y[1 + (j-1)*2], 
           border = dice[j,2], lwd = .5)
      
      points(mean(c(overlay_x[1 + (j-1)*2], 
                    overlay_x[2 + (j-1)*2])), 
             mean(c(overlay_y[1 + (j-1)*2], 
                    overlay_y[2 + (j-1)*2])), 
             col = dice[j,2], pch = dice[j,4], cex = .5)
    }
    with(raw_temp[i,], legend("topright", paste('#', participant.code, 
                                           '|P=', player.treat_pressure, 
                                           '|C=', player.treat_color, 
                                           '|R=', player.report_value, 
                                         #  '|s=', player.sex, 
                                        #   '|b=', player.cblind, 
                                        #   '|t=', round(max(t), digits = 1), 
                                           '|?=', player.checkmore, sep = ''), cex = 0.3, box.lwd = .1))
    with(raw_temp[i,], legend("bottomright", strwrap(player.checkwhy, width = 60), cex = 0.3, box.lwd = .1))
  }
}


pdf("/Users/carinah/Documents/GitHub/color-me-honest/analysis/trajectories.pdf")
par(mfrow = c(9, 7), 
  #  mar = c(2, 2, 2, 2), 
    mai = c(.1, 0.1, 0.1, 0.1))
plt_mouse("CP")
#plt_mouse("CNP")
#plt_mouse("NCP", type_in = "A")
#plt_mouse("CNP", type_in = "A<O")
dev.off()


# demonstration types
#A
#34tj2pql
#A<O
#unll52d9
#A=O
#9x4wp3bz
#A>O
#4no3ml9t


#----------------------------------------------------------- PLOT ALL
library(dplyr)
library(readr)
library(forcats)
library(ggplot2)

# Define colors and values
palette <- colorRampPalette(c('yellow', 'red'))
dcolors <- c("deepskyblue", "forestgreen", "coral4", "deeppink2", "orange", "gray")
dvalues <- c(49:54)

raw <- df

total_dist_data <- read_csv("/Users/carinah/Documents/GitHub/color-me-honest/analysis/totaldist-annotated.csv", show_col_types = FALSE) %>%
  select(participant.code, total_distance, type)

raw <- raw %>%
  left_join(total_dist_data, by = "participant.code") %>%
  arrange(participant.code)


plt_mouse <- function() {
  raw_temp <- raw  # Use the combined and sorted dataset
  pdf("/Users/carinah/Documents/GitHub/color-me-honest/analysis/trajectories.pdf")
  par(mfrow = c(9, 7), mai = c(.1, .1, .1, .1))
  for (i in 1:nrow(raw_temp)) {
      
      x <- as.numeric(unlist(strsplit(as.character(raw_temp$player.mouse_x[i]), split = ",")))
      y <- as.numeric(unlist(strsplit(as.character(raw_temp$player.mouse_y[i]), split = ",")))
      t <- as.numeric(unlist(strsplit(as.character(raw_temp$player.mouse_t[i]), split = ",")))
      
      color <- palette(100)[as.numeric(cut(t, breaks = 100))]
      with(raw_temp[i,], plot(y~x, pch = ".", 
                              xlim = c(player.butt_fl, player.butt_cr), 
                              ylim = c(player.butt_at, player.butt_eb), 
                              col = color, 
                              asp = 1, 
                              axes = FALSE, ann = FALSE, frame.plot = TRUE))
      
      dice <- data.frame()
      for (j in 1:6) {
        dice[j,1] <- as.integer(substr(raw_temp$player.faces[i], 2 + (j-1)*3, 2 + (j-1)*3))
        dice[j,2] <- ifelse(raw_temp$player.treat_color[i] == 1, dcolors[dice[j,1]], "gray")
        dice[j,3] <- as.integer(substr(raw_temp$player.values[i], 2 + (j-1)*3, 2 + (j-1)*3))
        dice[j,4] <- dvalues[dice[j,3]]
      }
      
      #plot overlay and legends
      overlay_x <- with(raw_temp[i,], c(player.butt_al, player.butt_ar, player.butt_bl, player.butt_br, player.butt_cl, player.butt_cr, player.butt_dl, player.butt_dr, player.butt_el, player.butt_er, player.butt_fl, player.butt_fr))
      overlay_y <- with(raw_temp[i,], c(player.butt_at, player.butt_ab, player.butt_bt, player.butt_bb, player.butt_ct, player.butt_cb, player.butt_dt, player.butt_db, player.butt_et, player.butt_eb, player.butt_ft, player.butt_fb))
      for (j in 1:6) {
        rect(overlay_x[1 + (j-1)*2], 
             overlay_y[2 + (j-1)*2], 
             overlay_x[2 + (j-1)*2], 
             overlay_y[1 + (j-1)*2], 
             border = dice[j,2], lwd = .5)
        
        points(mean(c(overlay_x[1 + (j-1)*2], 
                      overlay_x[2 + (j-1)*2])), 
               mean(c(overlay_y[1 + (j-1)*2], 
                      overlay_y[2 + (j-1)*2])), 
               col = dice[j,2], pch = dice[j,4], cex = .5)
      }
      with(raw_temp[i,], legend("topright", paste('#', participant.code, 
                                                  '|P=', player.treat_pressure, 
                                                  '|C=', player.treat_color, 
                                                  '|R=', player.report_value, 
                                                  #  '|s=', player.sex, 
                                                  #   '|b=', player.cblind, 
                                                  #   '|t=', round(max(t), digits = 1), 
                                                  '|?=', player.checkmore, sep = ''), cex = 0.3, box.lwd = .1))
      with(raw_temp[i,], legend("bottomright", strwrap(player.checkwhy, width = 60), cex = 0.3, box.lwd = .1))
    }
  dev.off()
}

plt_mouse()

