library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(scales)
library(tibble)
library(fmsb)
library(rsconnect)

server <- function(input, output){

df_1640 <- read.csv(file = "event_data/Bensalem2019.csv", header = TRUE)

## 1640 uses TRUE and FAlSE BOOL statements instead of ones and zeros, switches them to something that we can work with
df_1640[df_1640 == TRUE] <- 1
df_1640[df_1640 == FALSE] <- 0

# Total Columns in starting df 107
# Formatting the data from Sandstorm into values that we can formatt and would be easier to select
# Changing the data frame to match something that we had


  #######################
## SANDSTORM DATA TOTALS ##
  #######################

df_1640$SS_total_rocket_hatch <- df_1640$SS_LR_HATCH_LH + df_1640$SS_LR_HATCH_LM + df_1640$SS_LR_HATCH_LL +
                                 df_1640$SS_RR_HATCH_LH + df_1640$SS_RR_HATCH_LM + df_1640$SS_RR_HATCH_LL +
                                 df_1640$SS_LR_HATCH_RH + df_1640$SS_LR_HATCH_RM + df_1640$SS_LR_HATCH_RL +
                                 df_1640$SS_RR_HATCH_RH + df_1640$SS_RR_HATCH_RM + df_1640$SS_RR_HATCH_RL

df_1640$SS_total_rocket_cargo <- df_1640$SS_LR_CARGO_H1 + df_1640$SS_LR_CARGO_H2 + df_1640$SS_LR_CARGO_M1 + df_1640$SS_LR_CARGO_M2 + df_1640$SS_LR_CARGO_L1 + df_1640$SS_LR_CARGO_L2 +
                                 df_1640$SS_RR_CARGO_H1 + df_1640$SS_RR_CARGO_H2 + df_1640$SS_RR_CARGO_M1 + df_1640$SS_RR_CARGO_M2 + df_1640$SS_RR_CARGO_L1 + df_1640$SS_RR_CARGO_L2


df_1640$SS_total_CS_cargo <- df_1640$SS_CS_CARGO_L3 + df_1640$SS_CS_CARGO_L2 + df_1640$SS_CS_CARGO_L1 + df_1640$SS_CS_CARGO_FL +
                             df_1640$SS_CS_CARGO_R3 + df_1640$SS_CS_CARGO_R2 + df_1640$SS_CS_CARGO_R1 + df_1640$SS_CS_CARGO_FR


df_1640$SS_total_CS_hatch <- df_1640$SS_CS_HATCH_L3 + df_1640$SS_CS_HATCH_L2 + df_1640$SS_CS_HATCH_L1 + df_1640$SS_CS_HATCH_FL +
                             df_1640$SS_CS_HATCH_R3 + df_1640$SS_CS_HATCH_R2 + df_1640$SS_CS_HATCH_R1 + df_1640$SS_CS_HATCH_FR



                          ##############
  ############################################################
## NEW DF BASED ON 1640s DATA BUT FORMATTED AND DULLED FOR APP ##
  ############################################################


  ################
## SANDSTORM DATA ##
  ################

df_1640$SS_rocket_Lev_3_hatch <- df_1640$SS_RR_HATCH_RH + df_1640$SS_RR_HATCH_LH + df_1640$SS_LR_HATCH_RH + df_1640$SS_LR_HATCH_LH
df_1640$SS_rocket_Lev_2_hatch <- df_1640$SS_RR_HATCH_RM + df_1640$SS_RR_HATCH_LM + df_1640$SS_LR_HATCH_RM + df_1640$SS_LR_HATCH_LM
df_1640$SS_rocket_Lev_1_hatch <- df_1640$SS_RR_HATCH_RL + df_1640$SS_RR_HATCH_LL + df_1640$SS_LR_HATCH_RL + df_1640$SS_LR_HATCH_LL

df_1640$SS_rocket_Lev_3_cargo <- df_1640$SS_RR_CARGO_H1 + df_1640$SS_RR_CARGO_H2 + df_1640$SS_LR_CARGO_H1 + df_1640$SS_LR_CARGO_H2
df_1640$SS_rocket_Lev_2_cargo <- df_1640$SS_RR_CARGO_M1 + df_1640$SS_RR_CARGO_M2 + df_1640$SS_LR_CARGO_M1 + df_1640$SS_LR_CARGO_M2
df_1640$SS_rocket_Lev_1_cargo <- df_1640$SS_RR_CARGO_L1 + df_1640$SS_RR_CARGO_L2 + df_1640$SS_LR_CARGO_L1 + df_1640$SS_LR_CARGO_L2



  ##############
## TELE-OP DATA ##
  #############

df_1640$total_rocket_hatch <- df_1640$LR_HATCH_LH + df_1640$LR_HATCH_LM + df_1640$LR_HATCH_LL + df_1640$RR_HATCH_LH + df_1640$RR_HATCH_LM + df_1640$RR_HATCH_LL + df_1640$LR_HATCH_RH + df_1640$LR_HATCH_RM + df_1640$LR_HATCH_RL + df_1640$RR_HATCH_RH + df_1640$RR_HATCH_RM + df_1640$RR_HATCH_RL

df_1640$total_rocket_cargo <- df_1640$LR_CARGO_H1 + df_1640$LR_CARGO_H2 + df_1640$LR_CARGO_M1 + df_1640$LR_CARGO_M2 + df_1640$LR_CARGO_L1 + df_1640$LR_CARGO_L2 + df_1640$RR_CARGO_H1 + df_1640$RR_CARGO_H2 + df_1640$RR_CARGO_M1 + df_1640$RR_CARGO_M2 + df_1640$RR_CARGO_L1 + df_1640$RR_CARGO_L2

df_1640$total_CS_hatch <- df_1640$CS_HATCH_L3 + df_1640$CS_HATCH_L2 + df_1640$CS_HATCH_L1 + df_1640$CS_HATCH_FL + df_1640$CS_HATCH_R3 + df_1640$CS_HATCH_R2 + df_1640$CS_HATCH_R1 + df_1640$CS_HATCH_FR

df_1640$total_CS_cargo <- df_1640$CS_CARGO_L3 + df_1640$CS_CARGO_L2 + df_1640$CS_CARGO_L1 + df_1640$CS_CARGO_FL +
                          df_1640$CS_CARGO_R3 + df_1640$CS_CARGO_R2 + df_1640$CS_CARGO_R1 + df_1640$CS_CARGO_FR


df_1640$rocket_Lev_3_hatch <-  df_1640$RR_HATCH_RH + df_1640$RR_HATCH_LH + df_1640$LR_HATCH_RH + df_1640$LR_HATCH_LH
df_1640$rocket_Lev_2_hatch <-  df_1640$RR_HATCH_RM + df_1640$RR_HATCH_LM + df_1640$LR_HATCH_RM + df_1640$LR_HATCH_LM
df_1640$rocket_Lev_1_hatch <-  df_1640$RR_HATCH_RL + df_1640$RR_HATCH_LL + df_1640$LR_HATCH_RL + df_1640$LR_HATCH_LL

df_1640$rocket_Lev_3_cargo <-  df_1640$RR_CARGO_H1 + df_1640$RR_CARGO_H2 + df_1640$LR_CARGO_H1 + df_1640$LR_CARGO_H2
df_1640$rocket_Lev_2_cargo <-  df_1640$RR_CARGO_M1 + df_1640$RR_CARGO_M2 + df_1640$LR_CARGO_M1 + df_1640$LR_CARGO_M2
df_1640$rocket_Lev_1_cargo <-  df_1640$RR_CARGO_L1 + df_1640$RR_CARGO_L2 + df_1640$LR_CARGO_L1 + df_1640$LR_CARGO_L2


df_1640$Hatch_total <- df_1640$total_CS_hatch + df_1640$rocket_Lev_3_hatch + df_1640$rocket_Lev_2_hatch + df_1640$rocket_Lev_1_hatch
df_1640$Cargo_total <- df_1640$total_CS_cargo + df_1640$rocket_Lev_3_cargo + df_1640$rocket_Lev_2_cargo + df_1640$rocket_Lev_1_cargo




df <- subset(df_1640, select = c("Match", "Scout_Initials", "Team", "Starting_Location","Piece_Holding", "SS_rocket_Lev_3_cargo", "SS_rocket_Lev_2_cargo", "SS_rocket_Lev_1_cargo", "SS_rocket_Lev_3_hatch", "SS_rocket_Lev_2_hatch", "SS_rocket_Lev_1_hatch", "SS_total_rocket_hatch", "SS_total_rocket_cargo", "SS_total_CS_cargo", "SS_total_CS_hatch",
                                 "rocket_Lev_3_hatch", "rocket_Lev_2_hatch", "rocket_Lev_1_hatch", "rocket_Lev_3_cargo", "rocket_Lev_2_cargo", "rocket_Lev_1_cargo",
                                 "total_rocket_hatch", "total_rocket_cargo", "total_CS_hatch", "total_CS_cargo", "Hatch_total", "Cargo_total",
                                 "FEEDER_L", "FEEDER_R", "Level_Ended", "General_Success", "Defensive_Success", "Success_v_Defense", "Efficient_Placing", "Dropped_Pieces"
))


  ####################
## MAKING DATAFRAMES ##
  ###################

  #Taking Means of Total Cargo
 summary_df <- round(aggregate(cbind(df$Hatch_total, df$Cargo_total, df$Efficient_Placing , df$Level_Ended), by=list(Category=df$Team), FUN=mean), digits = 3)

  #Making summary_df for EVENT TAB AND TEAM TAB
 names(summary_df) <- c("Team", "Hatch_Avg", "Cargo_Avg", "Efficient_Placing", "Level_Ended")


## Team Summary More Df ##


  team_summary_df <- aggregate(cbind(df$Hatch_total, df$Cargo_total, df$rocket_Lev_3_hatch, df$rocket_Lev_3_cargo, df$rocket_Lev_2_hatch, df$rocket_Lev_2_cargo), by=list(Category=df$Team), FUN=sum)

  names(team_summary_df) <- c("Team", "Hatch_Sum", "Cargo_Sum", "Hatch_High", "Cargo_High", "Hatch_Med", "Cargo_Med")




 ################################
## MISC. INFORMATION ABOUT ROBOT ##
  ###############################

##Server-side for STARTING ITEM :: STARTING ITEM
  output$startingitem_Text <- renderPrint({
    newdf <- df[grep(input$robot_numSearch, df$Team),c(1,5)]
    newdf$Piece_Holding[newdf$Piece_Holding == 0] <- "H"
    newdf$Piece_Holding[newdf$Piece_Holding == 1] <- "C"
    newdf
  })

##Server-side for DEFENCE RANK AND SCORE
  output$defence_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Team),c(1,34)]
  })
##Server-side for ENDING POINT
  output$endpoint_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Team),c(1,30)]
  })



#Server-Side for Finding who scouted the robot and at what match number

  output$scoutName <- renderPrint({
    df[grep(input$robot_numSearch, df$Team),c(1,2)]
  })

#Server-Side for Finding starting points and match number
  output$teamMatches <- renderPrint({
    df[grep(input$robot_numSearch, df$Team),c(1,4)]
  })


  ####################################
## SANDSTORM INFORMATION ABOUT ROBOT ##
 ####################################

#Server-Side for Sandstorm #Cargo# on CargoShip w/ match Number
  output$A_cargoShip_Cargo_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Team),c(1,14)]
  })
        #PLOT
        output$A_cargoShip_Cargo_Line <- renderPlot({
          plotdata <- df[grep(input$robot_numSearch, df$Team),c(1,14)]
          plotdata$Match <- as.character(plotdata$Match)
          ggplot(data = plotdata, aes(x=Match, y=SS_total_CS_cargo)) + geom_bar(stat="identity") + ylim(0,3)
        })

##Server-side for Sandstrom #Hatches on CargoShip w/ match Number
  output$A_cargoShip_Hatch_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Team),c(1,15)]
  })
        #PLOT
        output$A_cargoShip_Hatch_Line <- renderPlot({
          plotdata <- df[grep(input$robot_numSearch, df$Team),c(1,15)]
          plotdata$Match <- as.character(plotdata$Match)
          ggplot(data = plotdata, aes(x=Match, y=SS_total_CS_hatch)) + geom_bar(stat="identity")  + ylim(0,3)
        })

##TOTAL ROCKET HATCH
  output$A_rocket_Hatch_Text <- renderPrint({
    df[grep(input$robot_numSearch, df$Team), c(1,22)]
  })

  ##################################
## TELE-OP INFORMATION ABOUT ROBOT ##
  #################################

##Server-side for TOTAL ROCKET DATA w/ match Number
  #CARGO
  output$rocket_Cargo_Text_total <- renderPrint({
   df[grep(input$robot_numSearch, df$Team),c(1,19,20,21,23)]

  })
        #PLOT
        output$rocket_Cargo_Plot_total <- renderPlot({
          plotdata <- df[grep(input$robot_numSearch, df$Team),c(1,23)]
          plotdata$Match <- as.character(plotdata$Match)
          ggplot(data = plotdata, aes(x=Match, y=total_rocket_cargo)) + geom_bar(stat="identity") + ylim(0,6)
        })

  #HATCH
  output$rocket_Hatch_Text_total <- renderPrint({
    hatch_df <- df[grep(input$robot_numSearch, df$Team),c(1,16,17,18,22)]
    hatch_df
  })
        #PLOT
        output$rocket_Hatch_Plot_total <- renderPlot({
          plotdata <- df[grep(input$robot_numSearch, df$Team),c(1,22)]
          plotdata$Match <- as.character(plotdata$Match)
          ggplot(data = plotdata, aes(x=Match, y=total_rocket_hatch)) + geom_bar(stat="identity") + ylim(0,6)
        })

##Server-side for TOTAL CARGO SHIP DATA w/ match Number
  #HATCH 22
  output$cargoship_Hatch_Text_total <- renderPrint({
    df[grep(input$robot_numSearch, df$Team),c(1,24)]
  })
      #PLOT
      output$cargoship_Hatch_Plot_total <- renderPlot({
        plotdata <- df[grep(input$robot_numSearch, df$Team),c(1,24)]
        plotdata$Match <- as.character(plotdata$Match)
        ggplot(data = plotdata, aes(x=Match, y=total_CS_hatch)) + geom_bar(stat="identity") + ylim(0,6)
      })
  #CARGO 21
  output$cargoship_Cargo_Text_total <- renderPrint({
    df[grep(input$robot_numSearch, df$Team),c(1,25)]
  })
      #PLOT
      output$cargoship_Cargo_Plot_total <- renderPlot({
        plotdata <- df[grep(input$robot_numSearch, df$Team),c(1,25)]
        plotdata$Match <- as.character(plotdata$Match)
        ggplot(data = plotdata, aes(x=Match, y=total_CS_cargo)) + geom_bar(stat="identity") + ylim(0,6)
      })

  #################
## Frequencies TAB ###
  #################

#Starting location Freq
  output$Start_Location <- renderPrint ({
    tblofstrt <- df[grep(input$robot_numSearch, df$Team), c(3,4)]
    r1 <- length(which(tblofstrt == "1R"))
    c1 <- length(which(tblofstrt == "1C"))
    l1 <- length(which(tblofstrt == "1L"))
    l2 <- length(which(tblofstrt == "2L"))
    r2 <- length(which(tblofstrt == "2R"))
    matchesLen <- r1 + c1 + l1 + l2 + r2
    paste(c("L1: "), c(round((l1/matchesLen)*100, digits = 2)) , c("% || C1: ") , c(round((c1/matchesLen)*100, digits = 2)) , c("% || R1: ") , c(round((r1/matchesLen)*100, digits = 2)), c("% || L2: ") , c(round((l2/matchesLen)*100, digits = 2)) , c("% || R2: "), c(round((r2/matchesLen)*100, digits = 2)), c("%."), sep = "")
  })

#endlocaltion Freq
  output$End_Location <- renderPrint ({
    endloc_df <- df[grep(input$robot_numSearch, df$Team), c(3,30)]
    lv_0 <- length(which(endloc_df == 0))
    lv_1 <- length(which(endloc_df == 1))
    lv_2 <- length(which(endloc_df == 2))
    lv_3 <- length(which(endloc_df == 3))
    matchesLen <- lv_0 + lv_1 +lv_2 + lv_3

    paste(c("Level 0: "), c(round((lv_0/matchesLen)*100, digits = 2)) , c("% || Level 1: ") , c(round((lv_1/matchesLen)*100, digits = 2)) , c("% || Level 2: ") , c(round((lv_2/matchesLen)*100, digits = 2)), c("% || Level 3: ") , c(round((lv_3/matchesLen)*100, digits = 2)), c("%."), sep = "")
})




  ###################
## ROBOT SUMMARY TAB ##
  ###################

  output$robot_num <- renderText({
   if (!is.element(input$robot_numSearch, df$Team)) {
      "Overall"
    } else {
      paste("Team", input$robot_numSearch)
    }
  })
  #THE RADAR CHARTS OF GODS
  output$robot_skills_radar <- renderPlot({
    newdf <- summary_df[grep(input$robot_numSearch, summary_df$Team),]
    newdf$Team <- NULL
    newdf <- rbind(c(0,0,0,0), newdf)
    newdf <- rbind(c(8,8,5,3), newdf)
    radarchart(newdf, axistype = 2,
               pcol='brown3', pfcol='brown3', plwd = 3,
               cglcol="grey", cglty=1, axislabcol="grey", cglwd=2)

  })

  #Quick total Numbers
    #Total CARGO in Event
    output$robot_total_cargo_summary <- renderText({
      newdf <- df[grep(input$robot_numSearch, df$Team),]
    })


#OUTPUT FOR SHOWING THE ROBOT Category
  output$robot_category <- renderText ({
    newdf <- df[grep(input$robot_numSearch, df$Team),c(15,16,17,18,19,20,21,22,30)]
    ##TOTAL HATCHES 32
    H_total <- newdf$H_on_3_Lev + newdf$H_on_2_Lev + newdf$H_on_1_Lev + newdf$CS_H
    #TOTAL CARGO 33
    C_total <- newdf$C_on_3_Lev + newdf$C_on_2_Lev + newdf$C_on_1_Lev + newdf$CS_C
    hatches <- sum(H_total)
    cargo <- sum(C_total)
    defense <- sum(newdf$Defense) / 2

    if (!is.element(input$robot_numSearch, df$Team)) {
      ""
    } else if (defense > cargo && defense > hatches) {
      "Defensive Robot"
    }else if (cargo > defense && cargo > hatches) {
        "Cargo-Style Robot"
    } else if (hatches > cargo && hatches > defense) {
      "Hatch-Style Robot"
    }

  })

  ####################
## TEAM LEVLES OUTPUTs ##
  ####################

  output$robot_cargo_high <- renderText({
    newdf <- team_summary_df[grep(input$robot_numSearch, team_summary_df$Team),]
    newdf$Cargo_High
  })

  output$robot_cargo_med <- renderText({
    newdf <- team_summary_df[grep(input$robot_numSearch, team_summary_df$Team),]
    newdf$Cargo_Med
  })
  
  output$robot_hatch_high <- renderText({
    newdf <- team_summary_df[grep(input$robot_numSearch, team_summary_df$Team),]
    newdf$Hatch_High
  })

  output$robot_hatch_med <- renderText({
    newdf <- team_summary_df[grep(input$robot_numSearch, team_summary_df$Team),]
    newdf$Hatch_Med
  })

    output$robot_cargo_sum <- renderText({
    newdf <- team_summary_df[grep(input$robot_numSearch, team_summary_df$Team),]
    newdf$Cargo_Sum
  })
  
  output$robot_hatch_sum <- renderText({
    newdf <- team_summary_df[grep(input$robot_numSearch, team_summary_df$Team),]
    newdf$Hatch_Sum
  })

  ####################
## EVENT SUMMARY TAB ##
  ###################
  # Plain output of the summary_df that was made at the top of the code
  output$event_skill_summary <- renderDT({
    summary_df
  })

  #Scatter plot based on summary_df
  output$event_skill_summary_plot <- renderPlot({
    ggplot(summary_df, aes(x=Cargo_Avg, y=Hatch_Avg, label=Team, color=Level_Ended)) + geom_point(aes(size=Level_Ended)) +geom_text(color ="darkgreen", aes(label=Team),hjust=0, vjust=0) + scale_color_gradient(low="red", high="lightgreen")

  })

    ################
  ## PREDICTION TAB ##
    ################

  #SHOWING PERCENT WIN with inputs on UI.R
  output$total_w_l_corr <- renderPrint ({
    mylogit <- glm(W_L ~ C_on_3_Lev + C_on_2_Lev + C_on_1_Lev + CS_C + CS_H + A_H_on_1_Lev + A_CS_H + H_on_3_Lev + H_on_2_Lev + H_on_1_Lev + Defense, data = df, family = "binomial")
    c3 <- as.integer(input$cargo_3lvl_in)
    c2 <- as.integer(input$cargo_2lvl_in)
    c1 <- as.integer(input$cargo_1lvl_in)

    x <- data.frame(C_on_3_Lev = c3, C_on_2_Lev = c2, C_on_1_Lev = c1, CS_C = as.integer(input$cargo_cs_in), CS_H = as.integer(input$hatch_cs_in), A_H_on_1_Lev = as.integer(input$ahatch_1lvl_in), A_CS_H = as.integer(input$ahatch_cs_in), H_on_3_Lev = as.integer(input$hatch_3lvl_in), H_on_2_Lev = as.integer(input$hatch_2lvl_in), H_on_1_Lev = as.integer(input$hatch_1lvl_in), Defense = as.integer(input$defense_) )
    p<- predict(mylogit,x)
    paste(round(p*100, digits = 2), "% chance of winning", sep = "")
       # c(input$cargo_3lvl_in, input$cargo_2lvl_in, input$cargo_1lvl_in, input$cargo_cs_in, input$hatch_cs_in, input$ahatch_1lvl_in, input$ahatch_cs_in, input$hatch_3lvl_in, input$hatch_2lvl_in, input$hatch_1lvl_in, input$defense_)
  })
  #SUMMARY FOR LINEAR REGRESSION
  output$lin_reg_summ <- renderPrint({
    mylogit <- glm(W_L ~ C_on_3_Lev + C_on_2_Lev + C_on_1_Lev + CS_C + CS_H + A_H_on_1_Lev + A_CS_H + H_on_3_Lev + H_on_2_Lev + H_on_1_Lev + Defense, data = df, family = "binomial")
    summary(mylogit)
  })

}

