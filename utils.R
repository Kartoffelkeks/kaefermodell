get_orientation = function(my_quadrant, xcor, ycor){
  if((xcor > ycor && xcor > abs(ycor))|| xcor == ycor) {
    if(my_quadrant == 1) {
      orientation = "in"
    }
    else {
      if(my_quadrant == 3) {
        orientation = "out"
      }
      else {
        orientation = "circle"
      }
    }
  }
  else{
    if(xcor > ycor && xcor <= abs(ycor)){
      if(my_quadrant == 2) {
        orientation = "in"
      }
      else {
        if(my_quadrant == 4) {
          orientation = "out"
        }
        else {
          orientation = "circle"
        }
      }
    }
    else{
      if(xcor <= ycor && xcor < abs(ycor)){
        if(my_quadrant == 3) {
          orientation = "in"
        }
        else {
          if(my_quadrant == 1) {
            orientation = "out"
          }
          else {
            orientation = "circle"
          }
        }
      }
      else{
        if(xcor < ycor && xcor >= abs(ycor)){
          if(my_quadrant == 4) {
            orientation = "in"
          }
          else {
            if(my_quadrant == 2) {
              orientation = "out"
            }
            else {
              orientation = "circle"
            }
          }
        }
        else{
          orientation = "error"
        }
      }
    }
  }
  return(orientation)
}

get_trap_row = function(xcor, ycor){
  outer_radius = 10
  if(sqrt(xcor*xcor + ycor*ycor) > outer_radius){
    trap_row = "2ndRow"
  } 
  else{
    trap_row = "1stRow"
  }
  return(trap_row)
}

get_gamma_with_error = function(alpha_1, lambda_1, e_m, e_v){
  alpha_2 = ((alpha_1 + lambda_1 * e_m) ^2)/(alpha_1 + lambda_1^2 * e_v)
  lambda_2 = (alpha_1 * lambda_1 + lambda_1^2 * e_m)/(alpha_1 + lambda_1^2 * e_v)
  return(c(alpha_2, lambda_2))
}

get_alternative_walllength = function(distance1, length1, distance2){
  # for explanation of formula see Modelling Notebook entrance 25
  constant = length1^2/(distance1^2-sqrt(2)*distance1*length1+length1^2) 
  p = -sqrt(2)*constant*distance2/(constant-1)
  q = constant*distance2^2/(constant-1)
  # use p-q-formula
  length2 = -p/2 + sqrt((p/2)^2-q)
  return(length2)
}

get_rotation = function(xcor, ycor){
  # calc angle and convert it to degree
  rotation = atan(xcor/ycor)/(pi)*180
  rotation = (rotation - 45) %%90
  return(rotation)
}
# get_rotation(2,13.5)
# get_alternative_walllength(6.5, 1, sqrt(13.5^2 + 2^4))

get_trapping_pattern = function(params, constants, repetitions){
  memory_size = 1024 * 7
  # nl <- nl(nlversion = "6.1.1",
  nl <- nl(nlversion = "6.2.0",
           nlpath = path_netlogo,
           modelpath = path_model,
           jvmmem = memory_size)
  
  constants_local = append(constants, list("step_size_mean" = params[1], 
                                     "step_size_variance" = params[2], 
                                     "turning_angle_sd" = params[3], 
                                     "bias" = params[4]))
  
  # define the behavior space
  nl@experiment <- experiment(expname = "walk",
                              outpath = path_output,
                              repetition = repetitions,
                              tickmetrics = "false",
                              idsetup = "setup",
                              idgo = "go",
                              runtime = constants$max_ticks,
                              evalticks = constants$max_ticks,
                              metrics.turtles=list("pitfall_traps" = c("xcor",
                                                                       "ycor",
                                                                       "my_quadrant",
                                                                       "n_trapped_beetles")),
                              constants = constants_local)
  
  # setup the full factorial sampling
  nl@simdesign <- simdesign_simple(nl = nl, nseeds = 1)
  
  # run the experiment
  results <- run_nl_all(nl)
  
  results.unnested = results %>%
            unnest(cols = c(metrics.pitfall_traps))
  results.mutated = results.unnested %>%
    mutate(orientation = mapply(get_orientation,my_quadrant, xcor, ycor)) %>%
    mutate(trap_row = mapply(get_trap_row,xcor, ycor))
  
  results.renamed = results.mutated %>% rename_with(~ gsub(" ", "_", .x, fixed = TRUE))%>% 
    rename_with(~ gsub("[", "", .x, fixed = TRUE))%>% 
    rename_with(~ gsub("]", "", .x, fixed = TRUE))
  
  #check, if results are right
  summary = results.renamed %>%
    group_by(orientation, trap_row, run_number) %>%
    summarise(n_trapped_beetles = sum(n_trapped_beetles))
  
  summary.united = summary %>%
    unite(position, c(orientation, trap_row)) %>%
    spread(position, n_trapped_beetles) %>%
    select(-run_number)
  
  return(summary.united)
}

read_csv = function(path, colnames = c("run_number", "max_ticks", "worldsize", "n_beetles",
                                       "include_traps?", "trap_scenario", "init_beetles", "init_sd",
                                       "seed", "video_name", 
                                       "step_size_mean", "step_size_variance",
                                       "turning_angle_sd", "bias", 
                                       "step", "pitfall_list")) {
  library(tidyverse)
  
  table = read.csv(file.path(path_data, path), header = F,
                   col.names = colnames)
  
  
  run_number = as.integer(table[8:dim(table)[1], "run_number"])
  step_size_mean = as.double(table[8:dim(table)[1], "step_size_mean"])
  step_size_variance = as.double(table[8:dim(table)[1], "step_size_variance"])
  turning_angle_sd = as.integer(table[8:dim(table)[1], "turning_angle_sd"])
  bias = as.double(table[8:dim(table)[1], "bias"])
  init_beetles = as.factor(table[8:dim(table)[1], "init_beetles"])
  init_sd = as.integer(table[8:dim(table)[1], "init_sd"])
  trap_scenario = as.factor(table[8:dim(table)[1], "trap_scenario"])
  pitfall_list = table[8:dim(table)[1], "pitfall_list"]
  
  data_all = data.frame(run_number, step_size_mean, step_size_variance, turning_angle_sd, bias, init_beetles, init_sd, trap_scenario, pitfall_list)
  data_all$pitfall_list = strsplit(data_all$pitfall_list, as.character("]"))
  data_all$pitfall_list = lapply(data_all$pitfall_list, function(x) gsub(as.character(" ["), "", x, fixed = T))
  data_all$pitfall_list = lapply(data_all$pitfall_list, function(x) gsub(as.character("[["), "", x, fixed = T))
  
  get_pitfall_data = function(x){
    xcor = vector()
    ycor = vector()
    quadrant =  vector()
    n_trapped =  vector()
    for (liste in x) {
      if(liste != ""){
        value = strsplit(liste, as.character(" "))
        xcor = append(xcor, as.double(value[[1]][1]))
        ycor = append(ycor, as.double(value[[1]][2]))
        quadrant = append(quadrant, as.integer(value[[1]][3]))
        n_trapped = append(n_trapped, as.integer(value[[1]][4]))
      }
    }
    return(data.frame(xcor, ycor,quadrant, n_trapped))
  }
  
  data_all$pitfall_list = lapply(data_all$pitfall_list, get_pitfall_data)
  
  data_unnested = data_all %>% unnest(pitfall_list)
  
  any(is.na(data_unnested))
  
  
  results.mutated = data_unnested %>%
    mutate(orientation = mapply(get_orientation, quadrant, xcor, ycor)) %>%
    mutate(trap_row = mapply(get_trap_row,xcor, ycor))
  
  
  results.renamed = results.mutated %>% rename_with(~ gsub(" ", "_", .x, fixed = TRUE))%>% 
    rename_with(~ gsub("[", "", .x, fixed = TRUE))%>% 
    rename_with(~ gsub("]", "", .x, fixed = TRUE))
  
  #check if all traps are assigned correctly
  summary(as.factor(results.renamed$orientation))
  summary(as.factor(results.renamed$trap_row))
  
  #sum results of traps with same row and position
  summary = results.renamed %>%
    group_by(run_number, orientation, trap_row) %>%
    dplyr::summarise(n_trapped = sum(n_trapped))
  
  summary.united = summary %>%
    unite(position, c(orientation, trap_row)) %>%
    spread(position, n_trapped) 
  
  data.joined =inner_join(summary.united, data_all, by = "run_number")%>%
    select(-pitfall_list)
  
  return(data.joined)
}

my_ggbiplot = function (pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
            obs.scale = 1 - scale, var.scale = scale, groups = NULL, 
            ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 3, 
            alpha = 1, var.axes = TRUE, circle = FALSE, circle.prob = 0.69, 
            varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE,
            var.color = "darkred", shape = NULL, xlim = NULL, ylim = NULL, arrow_size = 3, ...) 
  {
    library(ggplot2)
    library(plyr)
    library(dplyr)
    library(scales)
    library(grid)
    stopifnot(length(choices) == 2)
    if (inherits(pcobj, "prcomp")) {
      nobs.factor <- sqrt(nrow(pcobj$x) - 1)
      d <- pcobj$sdev
      u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
      v <- pcobj$rotation
    }
    else if (inherits(pcobj, "princomp")) {
      nobs.factor <- sqrt(pcobj$n.obs)
      d <- pcobj$sdev
      u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
      v <- pcobj$loadings
    }
    else if (inherits(pcobj, "PCA")) {
      nobs.factor <- sqrt(nrow(pcobj$call$X))
      d <- unlist(sqrt(pcobj$eig)[1])
      u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor), FUN = "*")
      v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord), 
                                                    1]), FUN = "/")
    }
    else if (inherits(pcobj, "lda")) {
      nobs.factor <- sqrt(pcobj$N)
      d <- pcobj$svd
      u <- predict(pcobj)$x/nobs.factor
      v <- pcobj$scaling
      d.total <- sum(d^2)
    }
    else {
      stop("Expected a object of class prcomp, princomp, PCA, or lda")
    }
    choices <- pmin(choices, ncol(u))
    df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, 
                                FUN = "*"))
    v <- sweep(v, 2, d^var.scale, FUN = "*")
    df.v <- as.data.frame(v[, choices])
    names(df.u) <- c("xvar", "yvar")
    names(df.v) <- names(df.u)
    if (pc.biplot) {
      df.u <- df.u * nobs.factor
    }
    r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
    v.scale <- rowSums(v^2)
    df.v <- r * df.v/sqrt(max(v.scale))
    if (obs.scale == 0) {
      u.axis.labs <- paste("standardized PC", choices, sep = "")
    }
    else {
      u.axis.labs <- paste("PC", choices, sep = "")
    }
    u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", 
                                              100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
    if (!is.null(labels)) {
      df.u$labels <- labels
    }
    if (!is.null(groups)) {
      df.u$groups <- groups
    }
    if (varname.abbrev) {
      df.v$varname <- abbreviate(rownames(v))
    }
    else {
      df.v$varname <- rownames(v)
    }
    df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
    df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)
    g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) +
      theme_classic()+ 
      ylab(u.axis.labs[2]) + coord_equal(xlim = xlim, ylim = ylim)
    if (var.axes) {
      if (circle) {
        theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, 
                                                  length = 50))
        circle <- data.frame(xvar = r * cos(theta), yvar = r * 
                               sin(theta))
        g <- g + geom_path(data = circle, color = muted("white"), 
                           size = 1/2, alpha = 1/3)
      }
      g <- g + geom_segment(data = df.v, aes(x = 0, y = 0, 
                                             xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2, 
                                                                                                    "picas")), color = var.color, size = arrow_size)
    }
    if (!is.null(df.u$labels)) {
      if (!is.null(df.u$groups)) {
        g <- g + geom_text(aes(label = labels, color = groups), 
                           size = labels.size)
      }
      else {
        g <- g + geom_text(aes(label = labels), size = labels.size)
      }
    }
    else {
      if (!is.null(df.u$groups)) {
        g <- g + geom_point(aes(color = groups, alpha = alpha, shape = shape))
      }
      else {
        g <- g + geom_point(alpha = alpha)
      }
    }
    if (!is.null(df.u$groups) && ellipse) {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- cbind(cos(theta), sin(theta))
      ell <- ddply(df.u, "groups", function(x) {
        if (nrow(x) <= 2) {
          return(NULL)
        }
        sigma <- var(cbind(x$xvar, x$yvar))
        mu <- c(mean(x$xvar), mean(x$yvar))
        ed <- sqrt(qchisq(ellipse.prob, df = 2))
        data.frame(sweep(circle %*% chol(sigma) * ed, 2, 
                         mu, FUN = "+"), groups = x$groups[1])
      })
      names(ell)[1:2] <- c("xvar", "yvar")
      g <- g + geom_path(data = ell, aes(color = groups, group = groups))
    }
    if (var.axes) {
      g <- g + geom_text(data = df.v, aes(label = varname, 
                                          x = xvar, y = yvar, angle = angle, hjust = hjust), 
                         color = var.color, size = varname.size)
    }
    return(g)
}

get_type = function(bias, ssmean, turning){
  if(ssmean == 0.5 & turning == 5){ # fast and small turns
    if(bias == 0){ # random
      type = "fast random"
    } 
    else{
      if(bias == -0.5){ # biased to walk out
        type = "fast out"
      } 
      else{
        if(bias == 0.5){ # biased to walk in
          type = "fast in"
        } 
        else{
          type = NA
        }
      }
    }
  } 
  else{
    if(ssmean == 0.1 & turning == 45){ # slow and big turns
      if(bias == 0){ # random
        type = "slow random"
      } 
      else{
        if(bias == -0.5){ # biased to walk out
          type = "slow out"
        } 
        else{
          if(bias == 0.5){ # biased to walk in
            type = "slow in"
          } 
          else{
            type = NA
          }
        }
      }
    } 
    else{
      type = NA
    }
  }
  return(type)
}



my_plot2 = function(data, xlim, ylim, groups, labs){
  this_plot = data%>%
    dplyr::select(`circle_1stRow_rel`:`out_2ndRow_rel`) %>%
    prcomp(scale. = TRUE) %>%
    my_ggbiplot(var.axes = FALSE, xlim = xlim, ylim = ylim, groups = groups, varname.size = 5, var.color = muted("green", l = 50, c = 70))+
    colorspace::scale_color_discrete_diverging(palette = "Berlin", na.value = "black")+
    scale_alpha_identity()+
    xlab("")+
    ylab("")+ 
    theme(legend.position = "none")
  # labs(color = labs)+
  # theme( legend.position="top")+
  # guides(color = guide_legend(label.position = "bottom", nrow = 1))
  
  return(this_plot)
}
my_plot2_abs = function(data, xlim, ylim, groups, labs){
  this_plot = data%>%
    dplyr::select(`circle_1stRow`:`out_2ndRow`) %>%
    prcomp(scale. = TRUE) %>%
    my_ggbiplot(var.axes = FALSE, xlim = xlim, ylim = ylim, groups = groups, varname.size = 5, var.color = muted("green", l = 50, c = 70))+
    colorspace::scale_color_discrete_diverging(palette = "Berlin", na.value = "black")+
    scale_alpha_identity()+
    xlab("")+
    ylab("")+ 
    theme(legend.position = "none")
  # labs(color = labs)+
  # theme( legend.position="top")+
  # guides(color = guide_legend(label.position = "bottom", nrow = 1))
  
  return(this_plot)
}
