
#-----------------------------------------------------------------------------#
#           e-BayNeRD - Enhanced BAYesian NEtworks for Raster Data            #
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#                                Implemented by                               #
#                     Alexsandro Candido de Oliveira Silva                    #
#                           alexsandro.silva@inpe.br                          #
#            National Institute for Space Research (INPE) - Brazil            #
#                                                                             #
#                                                                             #
#           The e-BayNeRD is an enhanced version of the BayNeRD model,        #
#                 which was developed by Mello et al (2013).                  #
#-----------------------------------------------------------------------------#


FUNCTION_write.the.heading.of.the.program <- function() {
  # Writes the heading of the program.
  cat("\n
      ________________________________________________________________

      e-BayNeRD - ENHANCED BAYESIAN NETWORKS FOR RASTER DATA
      Implemented by
      Alexsandro Candido de Oliveira Silva - alexsandro.silva@inpe.br
      National Institute for Space Research (INPE) - Brazil

      -----

      The e-BayNeRD is an enhenced version of the BayNeRD model,
      which was developed by Mello et al (2013):
      https://doi.org/10.3390/rs5115999.
      ________________________________________________________________\n\n")
}


FUNCTION_how.to.cite.the.program <- function() {
  # Shows how to cite this program
  cat("\n\n
      ___________________________________________________________________
      Please, cite the follow bibliography:

      SILVA, A. C. O.; FONSECA, L. M. G.; KORTING, T. S..
      Bayesian network model to predict areas for sugarcane expansion
      in Brazilian Cerrado. Brazilian Journal of Cartography (2017),
      N. 69-5, Special Issue GEOINFO 2017, page 857-867. Brazilian Society
      of Cartography, Geodesy, Photgrammetry and Remote Sense.
      ISSN: 1808-0936.
      
      -----
      
      SILVA, A. C. O.; MELLO, M. P.; FONSECA, L. M. G.. Enhanced Bayesian 
      Network for Raster Data (e-BayNeRD). In: Brazilian Symposium on 
      Geoinformatics - GEOINFO, 2014, Campos do Jordão, SP, Brazil. 
      Proceedings of the Brazilian Symposium on Geoinformatics - GEOINFO. 
      São José dos Campos - SP: National Institute for Space Research (INPE), 
      2014. v. 15. p. 73-83.
      ___________________________________________________________________
      \n\n")
}

#-----------------------------------------------------------------------------#
#                     e-BayNeRD menu in RGui for Windows                      #
#-----------------------------------------------------------------------------#

FUNCTION_eBayNeRD.R.menu.names <- function() {
  # Builds an window menu in R for Windows Operational System.
  #
  # Returns:
  #   A list with the menu and submenus names and their respective functions.
  main.menu.name <- "e-BayNeRD"  # Defining main menu name.
  if(Sys.info()[[1]]=="Windows")
    winMenuAdd(menuname=main.menu.name)

  # Defining names and type for the menu (they should be changed only here)
  # A is for the main menu
  #   A0 if there are no submenus in A
  #   A1 if there are submenus in A
  # B is for the first submenu
  #   B0 if there are no submenus in B
  #   B1 if there are submenus in B
  menu.type <- c("A0","A0", "A0",
                 "A0","A1","B0","B0",
                 "A0","A0","A0","A0","A0","A0", "A0")
  menu.disable.until.step <- c(0,0,0,1,2,2,2,3,4,5,5,6,7,0)
  menu.name <- c("About",
                 "How to cite e-BayNeRD",
                 "Before beginning...",
                 "Reading target variable",
                 "Reading context variables",
                 "In a specific subfolder",
                 "Type file names manually",
                 "Building the DAG",
                 "Computing the CPTs",
                 "Computing the influence KL score",
                 "Generating the Probability(ies) Band(s) (PB)",
                 #"Generating the classified image",
                 "Reading target variable (for testing)",
                 #"Accuracy assessment",
                 "Finding the best-TPV",
                 "Instructions...")
 menu.action <- c("FUNCTION_write.the.heading.of.the.program()",
                   "FUNCTION_how.to.cite.the.program()",
                   "FUNCTION_before.beginning()",
                   "FUNCTION_read.target.variable()",
                   "",
                   "FUNCTION_read.context.variables(subfolder=T)",
                   "FUNCTION_read.context.variables(subfolder=F)",
                   "FUNCTION_graphical.model()",
                   "FUNCTION_probability.table()",
                   "FUNCTION_running.influence.KLscore()",
                   "FUNCTION_probability.bands()",
                   #"FUNCTION_classification()",
                   "FUNCTION_read.target.variable.for.testing()",
                   #"FUNCTION_assessment()",
                   "FUNCTION_find.bestTPV()",
                   "FUNCTION_instructions()")

  # Mounting the data.frame that contains the menu information
  menu.table <- data.frame(menu.type=menu.type,
                           disable.until.step=menu.disable.until.step,
                           menu.name=menu.name, action=menu.action)
  menu.table$menu.name <- as.character(menu.table$menu.name)
  menu.table$action <- as.character(menu.table$action)
  return(list(main.menu.name=main.menu.name,menu.table=menu.table))
}


FUNCTION_eBayNeRD.R.menu.activation <- function(
  menu.elements = FUNCTION_BayNeRD.R.menu.names(), exist.eBayNeRDinfo=F) {
  # Activates or Desactivates a menu option based on
  # what the user has done before in e-BayNeRD.
  # This function is used in R for Windows.
  if(Sys.info()[[1]]=="Windows") {
    main.menu.name <- menu.elements[[1]]
    menu.table <- menu.elements[[2]]

    # Writing the entire menu
    if (exist.eBayNeRDinfo==F) {
      winMenuAdd(main.menu.name)
      for (i in 1:length(menu.table[,1])) {
        if (menu.table$menu.type[i] == "A0") {
          winMenuAddItem(main.menu.name,
                         menu.table$menu.name[i], menu.table$action[i])
        } else {
          if (menu.table$menu.type[i] == "A1") {
            winMenuAdd(paste(main.menu.name, "/", menu.table$menu.name[i],
                             sep=""))
          } else {
            winMenuAddItem(paste(main.menu.name, "/",
                                 menu.table$menu.name[max(
                                   which(menu.table$menu.type[1:i] == "A1"))],
                                 sep=""),
                           menu.table$menu.name[i], menu.table$action[i])
          }
        }
      }
      current.step <- 0
    } else {
      current.step <- max(eBayNeRDinfo$step.completed)
    }

    for (i in 1:length(menu.table[,1])) {
      if (menu.table$disable.until.step[i] > current.step) {
        temp <- "disable"
      } else {
        temp <- "enable"
      }
      if (menu.table$menu.type[i] == "A0") {
        winMenuAddItem(main.menu.name, menu.table$menu.name[i], temp)
      } else {
        if (menu.table$menu.type[i]=="B0") {
          winMenuAddItem(paste(main.menu.name, "/",
                               menu.table$menu.name[max(
                                 which(menu.table$menu.type[1:i] == "A1"))],
                               sep=""), menu.table$menu.name[i], temp)
        }
        rm(temp)
      }
    }
  }
}


#-----------------------------------------------------------------------------#
#                                  FUNCTIONS                                  #
#-----------------------------------------------------------------------------#

FUNCTION_read.target.variable <- function() {
  # Reads the target variable data (reference data for training)
  # and save the informations in the eBayNeRD.RData file.
  cat("\n\n\nInstructions:\n
      - Target variable data should be in RASTER format (i.e. it is an image);
      - It must be a GeoTiff file (e.g. 'tif' file extension);
      - It should be geographically referenced (i.e., georeferenced);
      - It must have only one band (not staked layers);
      - If the target variable is discrete it must have at least two labels
      (corresponding to the 'target' and 'no-target' thematic classes);

      Example of entry: D:/project/target.tif\n\n")

  ok <- F
  while (ok == F) {
    name.of.target.file <- readline(
      paste("\nEnter the file name (with path) of the target data image\n",
            "(including the extension): ", sep=""))
    if (file.exists(name.of.target.file) == T) {
      ok <- T
    } else {
      cat("\n\nThis file (path) does not exist!\n")
    }
  }
  rm(ok)

  # Entering the name of the TARGET variable.
  name.of.target <- readline(
    paste("\nEnter the name of the target variable (e.g. TARGET or just T): ",
          sep=""))

  # Loading the target variable.
  temp.data.raster <- raster(name.of.target.file)

  if (nbands(temp.data.raster) > 1) {  # Checking the requirements
    stop("\nThe entered target data image has more than one band (layer).",
         call.=F)
  }

  # List of labels corresponding to pixels of target variable.
  temp <- list(class.outside   = "-999999",
               class.NA.inside = "-888888",
               class.target    = "-777777")

  flush.console()
  cat("\n\n\nReading... (please wait)\n\n")

  classes <- raster::unique(temp.data.raster)
  n.classes <- length(classes)

  # Testing if the target data image has more than 1 class.
  if (n.classes < 2) {
    stop ("\nThe entered target data image has only one label.
          It must have at least two.\n")
  }

  cat("\n
      Enter the corresponding requested labels as follow.
      The entered target data image has", n.classes, "labels: ",
      paste(classes, collapse=", "),"\n\n")

  # Reading the label corresponding to pixels outside the study area (mask).
  ok <- F
  while(ok==F){
    temp1 <- ""
    temp1 <- readline(
      paste("\nEnter the label corresponding to pixels outside\nthe study",
            " area (left blank if there is not): ", sep=""))
    if (temp1 == "") {
      ok <- T
    } else {
      if (is.element(temp1, classes)) {
        ok <- T;
        temp$class.outside <- temp1
      } else {
        rm(temp1);
        cat("\n\nThis label does not exist!\n")
      }
    }
  }
  rm(ok)

  # Reading the label corresponding to pixels
  # with no data inside the study area.
  ok <- F
  while (ok == F) {
    temp2 <- ""
    temp2 <- readline(
      paste("\nEnter the label corresponding to pixels with no-data\n",
            "inside the study area (left blank if there is not): ", sep=""))
    if (temp2 == "") {
      ok <- T
    } else {
      if (temp2 == temp1) {
        rm(temp2)
        cat("\n\nLabel already used!\n")
      }
      if (is.element(temp2, classes)) {
        ok <- T
        temp$class.NA.inside <- temp2
      } else {
        rm(temp2)
        cat("\n\nThis label does not exist!\n")
      }
    }
  }
  rm(ok)

  # Reading the labels corresponding to pixels of the class "target".
  ok1 <- F
  while (ok1 == F) {
    cat("\n\nHow many classes have the 'target'?\n")
    temp3 <- as.numeric(readline("Answer: "))
    if ((is.na(temp3)) | (temp3 == 0) | (temp3 == "") |
        ((temp3 - floor(temp3)) != 0)) {
      cat("\n\nInvalid value!\n")
    } else {
      temp4 <- NULL
      for (i in 1:temp3) {
        ok2 <- F
        while (ok2 == F) {
          temp5 <- readline(
            paste("\nEnter the ", i, "th label",
                  " to pixels of the 'target' class: ", sep=""))
          if (temp5 == "") {
            cat("\n\nYou must enter a valid label!\n")
          }
          if ((temp5 == temp1) | (temp5 == temp2) |
              (is.element(temp5, temp4))) {
            rm(temp5)
            cat("\n\nLabel already used!\n")
          }
          if (is.element(temp5, classes)) {
            ok2 <- T
            temp4 <- c(temp4,temp5)
          } else {
            rm(temp5)
            cat("\n\nThis label does not exist!\n")
          }
        }
      }
    }
    ok1 <- T
  }

  temp$class.target <- temp4;  # Labels of 'target' class

  # Checking class number consistence
  # Corresponding to, at least, the classes "target" and "no-target"
  shoud.have.at.least.X.classes <- temp3+1
  if (temp$class.outside != "-999999") {
    shoud.have.at.least.X.classes <- shoud.have.at.least.X.classes+1
  }
  if (temp$class.NA.inside != "-888888") {
    shoud.have.at.least.X.classes <- shoud.have.at.least.X.classes+1
  }
  if (n.classes < shoud.have.at.least.X.classes) {
    stop(paste("\nAccording to your entries
               the target data image should have at least",
               shoud.have.at.least.X.classes, "labels.
               However it has only", n.classes, "labels."))
  }

  flush.console()
  cat("\n\n Recording entered information... (please wait)")

  temp.data.raster[temp.data.raster == as.numeric(temp$class.outside) |
                     temp.data.raster == as.numeric(temp$class.NA.inside)] <- NA


  if (!(file.exists("./e-BayNeRD Outcomes"))) dir.create("./e-BayNeRD Outcomes")

  mask.file <- paste("./e-BayNeRD Outcomes/",
                     strsplit(name.of.target.file, ".tif")[[1]],
                     "_to_mask.tif", sep="")
  writeRaster(temp.data.raster, mask.file, format = 'GTiff', overwrite = T)

  # Step 2 corresponds to entered and set up the TARGET variable
  # as well as the study area information.
  eBayNeRDinfo$step.completed <- 0:2
  # Setting target information and study area dimensions.
  eBayNeRDinfo$target.variable <- list(name     = name.of.target,
                                       file     = name.of.target.file,
                                       mask     = mask.file,
                                       classes  = temp)
  # Save the eBayNeRDinfo file inside the current working folder.
  save(eBayNeRDinfo, file="eBayNeRDinfo.RData")
  load("eBayNeRDinfo.RData")  # Loading eBayNeRDinfo file.
  eBayNeRDinfo<<-eBayNeRDinfo

  if (Sys.info()[[1]] == 'Windows') {
    cat('\n\n === Done! ===\n\n')
    winDialog(type="ok",
              "Now the menus to read the context variables have been unlocked.")
    FUNCTION_eBayNeRD.R.menu.activation(menu.elements=eBayNeRDinfo$menu,
                                        exist.eBayNeRDinfo=T)
  } else {
    cat('\n\n Done!\n In the next step you should use the function\n',
        'FUNCTION_read.context.variables(subfolder = T or F).\n\n')
  }

  invisible(gc())
  invisible(rm(list=ls()))
  }


FUNCTION_changing.projection <- function(context.raster, proj.target) {

  # Changes the coordinate reference system (CRS) of a raster.
  #
  # Args:
  #   context.raster: a raster object of the context variable.
  #   proj.target:    an object of class CRS used to set the output CRS.
  #
  # Return:
  #   a raster object with changed CRS.

  # rasterOptions(progress='window')
  return(projectRaster(
    context.raster, crs=proj.target, method="ngb", progress='text'))
}


FUNCTION_changing.pixel.size <- function(context.raster, target.raster) {
  # Changes the pixel size (resolution) of a raster.
  #
  # Args:
  #   context.raster: a raster object of the context variable.
  #   target.raster:  a raster object with parameters that
  #                   context.raster should be resampled to.
  #
  # Return:
  #   a resampled raster object

  fact.res <- NULL  # Aggregation or disaggregation factor (as number of cels).
  res.context <- res(context.raster)
  res.target <- res(target.raster)
  aux <- sum(res.target > res.context)

  # x and y resolutions of the target variable
  # are less than resolutions of the context one.
  if (aux == 0) {
    fact.res <- res.context/res.target
    if (sum(fact.res < 2) == 2) {  # (dis)aggregation factors < 2.
      return(resample(
        context.raster, target.raster, method='ngb', progress='text'))
    } else {
      # Both (dis)aggregation factors are integers.
      if (sum((fact.res - floor(fact.res)) == 0) == 2) {
        return(disaggregate(context.raster, fact.res, progress='text'))
      } else {
        context.raster <- disaggregate(
          context.raster, fact=floor(fact.res), progress='text')
        return(resample(
          context.raster, target.raster, method='ngb',progress='text'))
      }
    }
  }

  # x or y resolutions of the target variable
  # are less (or bigger) than resolutions of the context one.
  if (aux == 1) {
    return(resample(
      context.raster, target.raster, method='ngb', progress='text'))
  }

  # x and y resolutions of the target variable
  # are bigger than resolutions of the context one.
  if (aux == 2) {
    fact.res <- res.target/res.context
    if (sum(fact.res < 2) == 2) {  # (dis)aggregation factors < 2.
      return(resample(
        context.raster, target.raster, method='ngb', progress='text'))
    } else {
      # Both (dis)aggregation factors are integers.
      if (sum((fact.res - floor(fact.res)) == 0) == 2) {
        return(aggregate(context.raster, fact.res, progress='text'))
      } else {
        context.raster <- aggregate(
          context.raster, fact = floor(fact.res), progress='text')
        return(resample(
          context.raster, target.raster, method='ngb', progress='text'))
      }
    }
  }
}


FUNCTION_changing.bounding.box <- function(context.raster, target) {
  # Changes the bounding box of a raster
  #
  # Args:
  #   context.raster: a raster object of the context variable.
  #   ext.target:     a extent object used to set output bounding box raster
  #
  # Return:
  #   a raster object with changed bounding box

  context.raster <- resample(context.raster, target, method="ngb",
                             progress='text')
  return(context.raster)
}


FUNCTION_read.context.variables <- function(subfolder = T) {
  # Reads all the context variables
  # and save the informations in the eBayNeRD.RData file.
  cat("\n\n\nInstructions:\n
      - Context variables data should be in RASTER format;
      - They must be GeoTiff files (with the 'tif' file extension);
      - They must have only one band each (i.e., not stacked layers);
      - They may be geographically referenced with
      different coordinate reference system. The raster data will be
      transformed according to the TARGET image coordinate system;
      - They may have different spatial resolution (pixel size).
      The raster data will be resampled matching to the TARGET image;
      - They may contain a different extent.
      The raster data will be intersected according to the
      TARGET image bounding box. Outside values of study area will be
      ignored and the news cells will be assigned with NA values;
      - They may contain missing values.
      The corresponding label for that will be defined later.\n\n")

  # Target variable informations
  target.var <- eBayNeRDinfo$target.variable

  if (subfolder == T) {
    cat("\nAll the context variable files must be",
        "in a specific subfolder inside the working folder.\n")
    ok <- F
    while (ok == F) {
      subfolder.name <- readline("\nEnter the name of the subfolder containing the 'tif' files: ")
      if ((sum(dir(all.files = T) == subfolder.name) > 0)
          && (sum(dir(paste("./", subfolder.name, "/", sep=""),
                      pattern=".tif") != "") > 0)) {
        ok <- T
      } else {
        cat("\n\n
            The entered name is invalid.
            This subfolder doesn't exist or doesn't contain any 'tif' file!\n")
      }
      }
    rm(ok)

    Tiff.files.list <- paste("./", subfolder.name, "/", dir(
      paste("./", subfolder.name, "/", sep=""), pattern=".tif"), sep="")

    context.variables.list <- as.character(strsplit(dir(
      paste("./", subfolder.name, "/", sep=""), pattern=".tif"), ".tif"))

    n.context <- length(context.variables.list)
    cat("\n\n",
        n.context, "'tif' files have been found:\n",
        paste0(" - ",context.variables.list, collapse = ";\n"), ".\n")
      } else {
        cat("\n\n
            - Each context variables file name (and path, if necessary)
            must be entered manually.

            Example of entry: D:/MyProject/context/var1.tif\n\n\n")

        n.context <- readline("How many context variables will you enter: ")
        Tiff.files.list <- rep("xxx",n.context)
        for (i in 1:n.context) {
          ok <- F
          while (ok == F) {
            cat("\n\n")
            Tiff.files.list[i] <- readline(
              paste("Enter the name (path and extension) of the ", i, "-th file: ",
                    sep=""))
            if (file.exists(Tiff.files.list[i]) == T) {
              ok <- T
            } else {
              cat("\n\nThis file (path) does not exist!\n")
            }
          }
        }
        rm(ok)
        context.variables.list <- sapply(lapply(
          strsplit((sub("/.*", "", (sapply(lapply(
            strsplit((paste(strsplit(Tiff.files.list, ".tif"))), NULL), rev),
            paste, collapse="")))), NULL), rev), paste, collapse="")
        cat("\n\n",
            n.context, "context variables have been entered:\n",
            paste(" ",context.variables.list,collapse = ";\n"), ".\n")
      }

  flush.console()
  cat("\n\nReading and checking the requirements... (please wait)\n")

  # Loading the target variable.
  if(!file.exists(target.var$file)) {
    stop("\nThe TARGET image data is not in the current work-folder!", call.=F)
  } else {
    if (!file.exists(target.var$mask)) {
      stop(paste("\nThe masked TARGET image data is not in the\n",
                 "'./e-BayNeRD Outcomes' folder!"), call.=F)
    } else temp.target <- raster(target.var$mask)
  }

  list.context <- vector('list',n.context)

  # This loop checks if all the context variables
  # has the compatible parameters with target variable.
  for (i in 1:n.context) {
    temp.data <- raster(Tiff.files.list[i])

    # Checking the number of bands.
    if(nbands(temp.data)>1) {
      stop(paste("\nThe entered image data '", context.variables.list[i], "'
                 has more than one band (layer)!\n"))
    }

    # Checking the coordinate reference systems.
    if (!(compareRaster(temp.data, temp.target, extent=F, rowcol=F, crs=T,
                        res=F, orig=F, rotation=F, values=F,
                        stopiffalse=F, showwarning=F))) {
      print(cat("\n
                Based on the TARGET image previously entered,
                the entered image data '", context.variables.list[i], "'
                has no compatible coordinate reference systems.\n
                Its CRS will be changed. This process can take some minutes.\n", sep=""))

      proj.target <- projection(temp.target)
      temp.data <- FUNCTION_changing.projection(temp.data, proj.target)
    }

    # Checking the pixel size.
    if (!(compareRaster(temp.data, temp.target, extent=F, rowcol=F, crs=F,
                        res=T, orig=F, rotation=F, values=F,
                        stopiffalse=F, showwarning=F))) {
      cat("\n
          Based on the TARGET image previously entered,
          the entered image data '", context.variables.list[i], "'
          has no compatible pixel size.\n
          It will be resampled. This process can take some minutes.\n", sep="")

      res.target <- res(temp.target)
      temp.data <- FUNCTION_changing.pixel.size(temp.data, temp.target)
    }

    # Checking the bounding box.
    if (!(compareRaster(temp.data, temp.target, extent=T, rowcol=F, crs=F,
                        res=F, orig=F, rotation=F, values=F,
                        stopiffalse=F, showwarning=F))) {
      cat("\n
          Based on the TARGET data previously entered,
          the entered image data '", context.variables.list[i], "'
          has no same bounding box.\n
          Its bounding box will be changed.\n", sep="")
      #ext.target <- extent(temp.target)
      temp.data <- FUNCTION_changing.bounding.box(temp.data, temp.target)
    }

    list.context[[i]] <- temp.data
    rm(temp.data)
    }

  # Entering the context variables names.
  names.of.context.variables <- rep("",n.context)
  for (i in 1:n.context) {
    ok <- F
    while (ok == F) {
      cat("\n\n")
      temp <- ""
      temp <- readline(
        paste("Enter the name of the context variable ",
              context.variables.list[i],
              "\n(e.g., '",unlist(strsplit(
                context.variables.list[i],split=1,fixed=T)),
              "' or just '", toupper(unlist(strsplit(
                context.variables.list[i], split=NULL, fixed=T))[1]), "'): ",
              sep=""))

      if (temp=="") {
        names.of.context.variables[i] <- context.variables.list[i]
        ok <- T
      } else {
        if ((temp == target.var$name) |
            (sum(names.of.context.variables == temp) > 0)) {
          cat("\n\n
              Invalid name!
              The entered name already exist or
              is the same entered for the TARGET variable.\n\n")
        } else {
          if (!is.na(as.numeric(temp))) {
            cat("\n\nInvalid name! Numbers are not valid names.\n\n")
          } else {
            names.of.context.variables[i] <- temp
            ok <- T
          }
        }
    }
      rm(temp)
  }
    rm(ok)
  }

  # Asking about missing values in the context variables
  NA.label <- rep(NA ,n.context)
  ok1 <- F
  is.there.missing.data <- F
  while (ok1 == F) {
    cat("\n\n")
    is.it.ok <- readline(
      paste0("\nIs there any missing value in any context variable?\n",
            "(y-Yes ; n-No): "))
    if ((is.it.ok == "y") | (is.it.ok == "Y")) {
      ok1 <- T
      is.there.missing.data <- T
    } else {
      if ((is.it.ok == "n") | (is.it.ok == "N")) {
        ok1 <- T
      } else {
        cat("\n\nInvalid option!\n\n")
      }
    }
  }

  if (is.there.missing.data == T) {
    for (i in 1:n.context) {
      ok2 <- F
      while (ok2 == F) {
        NA.read <- ""
        cat("\n\n")
        NA.read <- readline(
          paste("Enter the label corresponding to missing values\n",
                "in the variable '", names.of.context.variables[i],
                "' (left blank if there is not): ", sep=""))
        if (NA.read != "") {
          cat("\n\nChecking... (please wait)\n\n")

          if (is.element(as.numeric(NA.read), getValues(list.context[[i]]))) {
            NA.label[i] <- NA.read
            ok2 <- T
          } else {
            cat(paste("\n\n
                      Invalid label!
                      The entered label does not exist in the context variable '",
                      names.of.context.variables[i], "'.\n", sep=""))
          }
        } else {
          ok2 <- T
        }
        rm(NA.read)
        }
      }
  }

  diff.NA <- which(!is.na(NA.label))
  if (length(diff.NA > 0)) {
    cat("\n\n\nMasking the missing values in the variables... (please wait)\n")
    eval(parse(text=(
      paste("list.context[[", diff.NA, "]][list.context[[", diff.NA, "]] ==
            as.numeric(NA.label[", diff.NA, "])] <- NA;", sep=""))))
  }

  cat("\n\nRecording entered information... (please wait)\n")

  flush.console()
  cat("\nWriting a copy of each context variable raster in\n'./e-BayNeRD Outcomes' folder.\n")

  if (!(file.exists("./e-BayNeRD Outcomes"))) {
    stop("\nThere is no 'e-BayNeRD Outcomes' folder!", call.=F)
  }

  context.files <- paste("./e-BayNeRD Outcomes/", context.variables.list,
                         ".tif", sep = "")
  stack.masked.files <-
    paste("./e-BayNeRD Outcomes/stack_masked_context_variables.tif")

  # Gets the number of cores to be used in the cluster
  use.cores <- detectCores() - 1

  # Creates a set of copies of R running in parallel
  cl <- makeCluster(use.cores)

  # To register the SNOW parrallel backend
  registerDoSNOW(cl)

  # Initializes the workers to be the same as the master
  invisible(clusterCall(cl, function(x) .libPaths(x), .libPaths()))

  # Assign the values of each listed global variables to
  # the same ones (same names) of each worker.
  invisible(clusterExport(cl, c('temp.target'), envir=environment()))

  # Number of iterations to set progress bar
  iterations <- length(list.context)

  # Creats a progress bar
  invisible(pb <- txtProgressBar(max = iterations, style = 3))

  # Sets the progress bar
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  result <-
    foreach(context = iter(list.context), File = iter(context.files),
            .packages = c('rgdal','raster'),
            .options.snow = opts) %dopar% {

              writeRaster(context, File, format = 'GTiff', overwrite = T )

              # Masking the context variables
              return(mask(context, temp.target))
            }

  close(pb)
  stopCluster(cl)

  flush.console()
  cat("\nWriting a RasterStack with context variables masked\n",
      "according to reference data in TARGET image.\n")

  # Writes a stack of the masked context variables
  writeRaster(stack(result), stack.masked.files, format = 'GTiff',
              overwrite = T)

  #Step 3 corresponds to the entry of the context variables
  eBayNeRDinfo$step.completed <- 0:3
  eBayNeRDinfo$context.variables <- list(n        = n.context,
                                         name     = names.of.context.variables,
                                         NA.label = NA.label,
                                         mask     = stack.masked.files,
                                         files    = context.files)
  # Save the eBayNeRDinfo file inside the current working folder
  save(eBayNeRDinfo,file="eBayNeRDinfo.RData")
  load("eBayNeRDinfo.RData") #loading eBayNeRDinfo file
  eBayNeRDinfo<<-eBayNeRDinfo

  if (Sys.info()[[1]] == 'Windows') {
    cat('\n\n === Done! ===\n\n')
    winDialog(type="ok",
              paste("Now the menu to define the\n",
                    "Bayesian Network graphical model has been unlocked."))
    FUNCTION_eBayNeRD.R.menu.activation(
      menu.elements=eBayNeRDinfo$menu, exist.eBayNeRDinfo=T)
  } else {
    cat('\n\n Done!\n In the next step you should use the function\n',
        'FUNCTION_graphical.model().\n\n')
  }

  invisible(gc())
  invisible(rm(list=ls()))
  }


FUNCTION_create.node <- function(idx, parents, name=paste(idx), position=c(0,0)) {
  ## This function is based on 'node' function from deal package.
  ## idx:      The unique index of the node
  ## name:     The plotted name
  ## parents:  Vector with indices of parents
  ## position: (x,y) coordinates

  nd          <- list()
  nd$idx      <- idx
  nd$name     <- name
  nd$parents  <- parents
  nd$position <- position

  return(nd)
}


FUNCTION_create.network <- function(df, yr = c(0,350), xr = c(0,350) )  {
  ## This function is based on 'network' function from deal package.

  if (length(dim(df)) < 1)
    stop("It can't handle with networks with one node.\n")

  nw <- list()
  n  <- ncol(df)

  nw <- list()
  unit <- 2 * pi / n
  xc <- mean(xr)
  yc <- mean(yr)

  for (i in 1:n) {
    pos <- c(cos(unit * i + pi/4), sin(unit * i + pi/4)) * xc * .9 + c(xc, yc)

    nw[[i]] <- FUNCTION_create.node(idx = i, parents = c(),
                                    name = names(df)[i], position = pos)
  }
  names(nw) <- names(df)

  return(nw)
}


AddArrow <- function(nw, j, i) {
  ## This function is based on 'insert' function from deal package.

  n <- length(nw)
  if (i > n | i == j){
    print(cat("\n\nThis arrow can't be created!!\n"))
    return(nw = NULL)
  }

  # Checking if j is parent of i.
  else if (!is.na(match(j, nw[[i]]$parents))) {
    print(cat("\n\nThis arrow already exist!!\n"))
    return(nw = NULL)
  } # Checking if i is parent of j.
  else if (!is.na(match(i, nw[[j]]$parents))) {
    print(cat("\n\nThis arrow can't be created!!\n"))
    return(nw = NULL)
  }

  # Adding j as parent of i.
  nw[[i]]$parents <- sort(c(nw[[i]]$parents, j))
  return(nw)
}


RemoveArrow <- function(nw, j, i) {
  ## This function is based on 'remover' function from deal package.
  n <- length(nw)
  if (i > n | i == j){
    return(nw = NULL)
  }

  parents <- nw[[i]]$parents

  if (!length(intersect(parents, j)) > 0) {
    print(cat("\n\nThere's no arrow there!\n"))
    return(nw = NULL)
  } else {
    nw[[i]]$parents <- setdiff(nw[[i]]$parents, j)
  }

  return(nw)
}


FindLeaf <- function(nw){
  ## This function is based on 'findleaf' function from deal package.
  n <- length(nw)
  jump <- FALSE
  for (i in 1:n) {
    for (j in 1:n) {
      res <- match(nw[[i]]$idx, nw[[j]]$parents)
      if (!is.na(res)) {
        jump <- TRUE
        break
      }
    }
    if (!jump)
      return(i)
    jump <- FALSE
  }
  res <- 0
  return(res)
}


CycleTest <- function(nw) {
  ## This function is based on 'cycletest' function from deal package.
  n <- length(nw)
  if (n == 1) {
    return(FALSE)
  }
  else {
    res <- FindLeaf(nw)
    if (res == 0) {
      return(TRUE)
    }
    else {
      nw <- nw[-res]
      CycleTest(nw)
    }
  }
}


FUNCTION_plot.network <- function(nw) {
  par(mar = c(2, 2, 2, 1))
  plot(0, 0, xlim = c(0, 400), ylim = c(0, 400), type = "n", axes = F,
       xlab = "", ylab = "", main = "Define the variables relationship", adj = 0)

  n <- length(nw)
  for (i in 1:n) {
    # Plotting nodes
    points(nw[[i]]$position[1], nw[[i]]$position[2],
           cex = 6, pch = 19, col = "deepskyblue")
    text(nw[[i]]$position[1], nw[[i]]$position[2],
         nw[[i]]$name, col = "black", cex = 1.5)

    # Ploting arrowns
    nodej <- nw[[i]]
    np <- length(nodej$parents)
    if (np > 0){
      for (l in 1:np){
        pos.nj <- nodej$position # node coordinates
        pa.id  <- nodej$parents[l] # index of the l'th parent
        pos.pa <- nw[[pa.id]]$position # parent coordinates

        # Unit vector from pos.pa to pos.nj
        unit <- (pos.nj - pos.pa) / sqrt(sum( (pos.nj - pos.pa)^2 ))

        pos.nj <- pos.nj - unit * 15
        pos.pa <- pos.pa + unit * 15

        arrows( pos.pa[1], pos.pa[2], pos.nj[1], pos.nj[2],
                length=.2, col="grey60", lwd = 2)
      }
    }
  }
}


FUNCTION_draw.network <- function(nw, yr = c(0,350), xr = c(0,350)) {
  ## This function is based on 'drawnetwork' function from deal package.

  FUNCTION_plot.network(nw)

  xc <- mean(xr)
  yc <- mean(yr)
  n  <- length(nw)

  newnet <- nw

  where <- t(matrix(unlist(lapply(newnet, function(x)x$position)), nrow=2))

  buttonx <- 30
  buttony <- 30

  # Coordinates of the ADD, REMOVE and STOP buttons.
  where <- rbind(where, c(2 * xc + buttonx, 2 * yc + buttony))
  where <- rbind(where, c(2 * xc + buttonx, 2 * yc))
  where <- rbind(where, c(2 * xc + buttonx, 2 * yc - buttony))

  mode <- "Add"
  quit   <- FALSE

  while(!quit) {

    # Setting the buttons aspect
    if (mode == "Add") {
      bgadd  <- "blue4"
      fgadd  <- "white"

      bgrem  <- "gray90"
      fgrem  <- "black"

      bgstop <- "gray90"
      fgstop <- "black"
    }

    if (mode == "Remove") {
      bgadd  <- "gray90"
      fgadd  <- "black"

      bgrem  <- "blue4"
      fgrem  <- "white"

      bgstop <- "gray90"
      fgstop <- "black"
    }

    # Ploting the ADD, REMOVE and STOP buttons.
    symbols(2 * xc + buttonx, 2 * yc + buttony,
            rectangles = matrix(c(3,1),1), add =T, bg = bgadd)
    text(2*xc + buttonx, 2 * yc + buttony, "Add", col = fgadd)

    symbols(2 * xc + buttonx, 2 * yc,
            rectangles = matrix(c(3,1),1), add = T, bg = bgrem)
    text(2 * xc + buttonx, 2 * yc, "Remove", col = fgrem)

    symbols(2 * xc + buttonx, 2 * yc - buttony,
            rectangles = matrix(c(3,1),1), add = T, bg = bgstop)
    text(2 * xc + buttonx, 2 * yc - buttony, "Stop", col = fgstop)


    # Reading the position of the pointer when the mouse is pressed.
    # It then searches the closest coordinates given in x and y.
    from <- identify(where[,1], where[,2], rep("", n + 3), n = 1)

    # If user presses the ADD button.
    if (from == (n + 1)) {
      mode <- "Add"
      next
    }

    # If user presses the REMOVE button.
    if (from == (n + 2)) {
      mode <- "Remove"
      next
    }

    # If user presses the STOP button.
    if (from == (n + 3)) break

    # Reading the position of the pointer when the mouse is pressed.
    # It then searches the closest coordinates given in x and y.
    to <- identify(where[,1], where[,2], rep("", n + 3), n = 1)

    # If user presses the ADD button.
    if (to == (n + 1)) {
      mode <- "Add"
      next
    }

    # If user presses the REMOVE button.
    if (to == (n + 2)) {
      mode <- "Remove"
      next
    }

    # If user presses the STOP button.
    if (to == (n + 3)) break


    if (mode == "Add") {
      tempnet <- AddArrow(newnet, from , to)
    } else if (mode == "Remove") {
      tempnet <- RemoveArrow(newnet, from , to)
    }

    if (length(tempnet) > 0) {
      if (!CycleTest(tempnet)) {
        newnet <- tempnet
      } else {
        print(cat("\n\nYou created a cycle. Try again!\n"))
      }
    }

    FUNCTION_plot.network(newnet)
  }

  FUNCTION_plot.network(newnet)

  return(newnet)
}


ModelString <- function(nw){
  ## This function is based on 'modelstring' function from deal package.
  res <- ""
  n   <- length(nw)
  g <- function(x) x$name
  for (j in 1:n) {
    nd <- nw[[j]]
    res <- paste(res, "[", nd$name, sep = "")
    if (length(nd$parents) > 0) {
      res <- paste(res, "|", paste(unlist(lapply(nw[nd$parents],
                                                 g)), collapse = ":"), sep = "")
    }
    res <- paste(res, "]", sep = "")
  }
  res
}


FUNCTION_graphical.model <- function() {
  # Creates a Bayesian Network graphical model
  # and save the informations in the eBayNeRD.RData file.
  cat("\n\n\nInstructions:\n
      - A empty graph will be drawn. You will specify a Bayesian Network
      through a point and click interface;
      - To insert an arc from the node 'A' to the node 'B', first check if the
      'ADD' button is activated. Then click on node 'A' and after click on node 'B';
      - To remove this arc, first click on 'REMOVE' button in the topright box,
      now you may remove the arc clicking on node 'A' and then on node 'B';
      - The program will not draw an arc if you create a cycle;
      - When your graph is done, check if it is correct and click 'STOP' button;
      - Please, always click ON the letters;\n\n")

  target.var <- eBayNeRDinfo$target.variable$name
  context.var <- eBayNeRDinfo$context.variables$name
  ok <- F
  while (ok == F) {
    is.it.ok <- readline(" Continue? (y-Yes ; n-No): ")
    if ((is.it.ok == "y") | (is.it.ok == "Y")) {
      ok <- T
    } else {
      if ((is.it.ok == "n") | (is.it.ok == "N")) {
        stop("\nAborted by the user!\n\n", call.=F)
      } else {
        cat("\n\nInvalid option!\n\n")
      }
    }
  }

  # The network function (below) has a data.frame as parameter
  # whose columns specify the names of network nodes.
  # all.variables is a empty data.frame.
  # Each column is a variable (target/context)
  all.variables <- NULL
  names.var <- c(target.var,context.var)
  eval(parse(text=paste("all.variables <- data.frame(",
                        paste(names.var," = 0", sep="", collapse=", "), ")",
                        sep="")))

  ok <- F
  while (ok == F) {
    net <- FUNCTION_create.network(all.variables)
    cat("\n Please, specify the network graph model... \n\n")

    # Building a empty network. The user must to draw the arcs
    net <- FUNCTION_draw.network(net)

    is.it.ok <- readline(cat("\n\n\nDo you want to use this DAG?\n",
                             "(y-Yes; n-No): "))
    if ((is.it.ok == "y") | (is.it.ok == "Y")) {
      ok <- T
    } else {
      if ((is.it.ok == "n") | (is.it.ok == "N")) {
        ok <- F
        graphics.off()
        rm(net)
      } else {
        cat("\n\nInvalid option!\n\n")
      }
    }
  }

  flush.console()
  cat("\n\nRecording entered information... (please wait)\n")

  # bnlearn-deal package integration
  net.str <- bnlearn::model2network(ModelString(net))

  # Step 4 corresponds to the entered BN graphical model
  eBayNeRDinfo$step.completed <- 0:4
  # Saving the Bayesian Network model
  eBayNeRDinfo$graph.model <- net.str
  # Save the eBayNeRDinfo file inside the current working folder
  save(eBayNeRDinfo,file="eBayNeRDinfo.RData")
  # Loading eBayNeRDinfo file
  load("eBayNeRDinfo.RData")
  eBayNeRDinfo<<-eBayNeRDinfo

  if (Sys.info()[[1]] == 'Windows') {
    cat('\n\n === Done! ===\n\n')
    winDialog(type="ok",
              paste("Now the menu to define the probability functions",
                    " has been unlocked."))
    FUNCTION_eBayNeRD.R.menu.activation(
      menu.elements=eBayNeRDinfo$menu, exist.eBayNeRDinfo=T)
  } else {
    cat('\n Done! In the next step you should use the function\n',
        'FUNCTION_probability.table().\n\n')
  }

  graphics.off();
  invisible(gc())
  invisible(rm(list=ls()))
}


FUNCTION_discretization <- function(var.name, cont.data, method) {
  # Discretizes the data by a choosed method.
  # The methods are: Equidistant intervals; Frequency;
  #                  Clustering and Manually (defines by the user).
  #
  # Args:
  #   var.name:  a character with the currente variable name.
  #   cont.data: a vector which will be discretized.
  #   method:    a string with the choosed method.
  #
  # Return:
  #   a vector of factors representing the variables categories.
  ok1 <- F
  while (ok1 == F) {
    n.intervals <- as.numeric(readline("How many intervals? "))
    if ((is.na(n.intervals)) | (n.intervals < 2)
        | ((n.intervals - floor(n.intervals)) != 0)) {
      cat("\n\nInvalid value!\n\n")
    } else {
      ok1 <- T
      if (method == "1") {  # Equal interval width.
        cat(paste("\n\nConverting the variable '",var.name,"'\n",
                  "into a categorical variable... (please wait)
                  \n\n", sep=""))
        return(arules::discretize(cont.data, method="interval",
                                  categories=n.intervals))
      }
      if (method == "2") {  # Equal frequency.
        cat(paste("\n\nConverting the variable '",var.name,"'\n",
                  "into a categorical variable... (please wait)
                  \n\n", sep=""))
        return(arules::discretize(cont.data, method="frequency",
                                  categories=n.intervals))
      }
      if (method == "3") {  # K-means clustering.
        cat(paste("\n\nConverting the variable '",var.name,"'\n",
                  "into a categorical variable... (please wait)
                  \n\n", sep=""))
        return(arules::discretize(cont.data, method="cluster",
                                  categories=n.intervals))
      }
      if (method == "4") {  # Categories specifies interval boundaries.
        data.range <- c(min(cont.data, na.rm = T), max(cont.data, na.rm = T))

        cat(paste("\n\nThe range of observed values\nof the variable '",
                  var.name, "' is: ", paste("[", paste(
                    round(data.range,4), collapse=", "),"]", sep=""), sep=""))

        limits <- NULL
        limits[c(1,n.intervals+1)] <- round(data.range,5)

        # Defining the limits.
        for (i in 1:(n.intervals - 1)) {
          cat(paste("\n\nThe lower limit of the ", i, "th interval is: ",
                    round(limits[i],4), sep=""))
          ok2 <- F
          while (ok2 == F) {
            temp <- as.numeric(readline(paste(
              "\nEnter the upper limit of this interval: ", sep="")))
            if ((is.na(temp)) | (temp <= limits[i])
                | (temp >= limits[n.intervals+1])) {
              cat("\n\nInvalid value!\n")
            } else {
              ok2 <- T
              limits[i+1] <- temp
            }
          }
        }

        cat(paste("\n\nConverting the variable '",var.name,"'\n",
                  "into a categorical variable... (please wait)
                  \n\n", sep=""))

        aux <- NULL
        aux.levels <- NULL

        if (n.intervals > 2) {

          aux[cont.data < limits[2]] <-
            paste("[",limits[1], ", ", limits[2], ")", sep="")
          aux.levels[1] <- paste("[",limits[1], ", ", limits[2], ")", sep="")

          for (i in 2:(n.intervals - 1)){
            aux[cont.data >= limits[i] & cont.data < limits[i+1]] <-
              paste("[",limits[i], ", ", limits[i+1], ")", sep="")
            aux.levels[i] <- paste("[",limits[i], ", ", limits[i+1], ")",
                                   sep="")
          }

          aux[cont.data >= limits[n.intervals]] <-
            paste("[",limits[n.intervals], ", ", limits[n.intervals+1], "]",
                  sep="")
          aux.levels[n.intervals] <-
            paste("[",limits[n.intervals], ", ", limits[n.intervals+1], "]",
                  sep="")
        } else {

          aux[cont.data < limits[2]] <-
            paste("[",limits[1], ", ", limits[2], ")", sep="")
          aux.levels[1] <- paste("[",limits[1], ", ", limits[2], ")", sep="")

          aux[cont.data >= limits[2]] <-
            paste("[",limits[2], ", ", limits[3], "]", sep="")
          aux.levels[2] <- paste("[",limits[2], ", ", limits[3], "]", sep="")
        }
        return(factor(aux, levels=aux.levels))
      }
    }
  }
      }


FUNCTION_categorize.raster <- function(file.raster, limits) {
  # To categorize the context variable's raster according to
  # the interval limits defined by user.
  #
  # Args:
  #   file.raster : a string with the path to load the raster data.
  #   limits      : a vector with the limits of each interval
  #
  # Return:
  #   a categorized raster data.

  # Loading the original raster data
  aux.raster  <- raster(file.raster)

  # Categorized raster data
  disc.raster <- raster(aux.raster)

  n <- length(limits)
  categories  <- seq(1:(n+1))

  if (length(limits) > 1){ # if there are more then two intervals

    disc.raster[ aux.raster < limits[1]] <- categories[1]

    for (i in 1:(n-1)){
      disc.raster[ aux.raster >= limits[i] &
                     aux.raster <  limits[i+1] ] <- categories[i+1]
    }

    disc.raster[ aux.raster >= limits[n] ] <- categories[n+1]
    return(disc.raster)

  } else { # if there are two intervals

    disc.raster[ aux.raster <  limits[1] ] <- categories[1]
    disc.raster[ aux.raster >= limits[1] ] <- categories[n+1]

    return(disc.raster)
  }
}


FUNCTION_limits.break <- function (x){
  as.numeric(strsplit(strsplit(x,",")[[1]][2],")")[[1]])
}


FUNCTION_interval.fit <- function (x) length(x) == 2


FUNCTION_probability.table <- function(){

  flush.console()
  cat("\n\n
      Reading and preparing all the data... (please wait)\n\n\n")

  # Gets the context variables data
  context.var    <- eBayNeRDinfo$context.variables
  # Gets the target variable data
  target.var     <- eBayNeRDinfo$target.variable
  nclasses <- length(target.var$classes$class.target)
  # Names of the variables given by user
  names.data     <- c(target.var$name,context.var$name)

  # Bayesian Network model
  net             <- eBayNeRDinfo$graph.model
  # Gets the order to process the model's variables
  order.process   <- node.ordering(net)
  # Gets the parents and children of the target variable
  target.parents  <- bnlearn::parents(net,target.var$name)
  target.children <- bnlearn::children(net, target.var$name)

  # Loading the target variable
  if (!file.exists(target.var$mask)) {
    stop(paste("\nThe masked TARGET image data is not in the\n",
               "'./e-BayNeRD Outcomes' folder!"), call.=F)
  } else target.raster <- raster(target.var$mask)

  # Loading the stack of masked context variables
  context.raster <- stack(context.var$mask)

  # The zero value was chosen to represent the TARGET absence
  # Zero is assigned to all pixels with different values of TARGET CLASSES
  str <- paste("(target.raster != ", target.var$classes$class.target,")",
               sep = "", collapse = " & ")
  eval(parse(text=paste("target.raster[",str,"] <- 0", sep="")))

  # Gets the indices of pixels that are different of 'NA' value
  diff.NA <- which(!is.na(getValues(target.raster)))

  # Stack all raster data (target and context variables)
  all.data <- stack(target.raster, context.raster)
  names(all.data) <- names.data

  # 'disc.vars' will store all variables discretized
  disc.vars <- vector('list',length(order.process))
  names(disc.vars) <- order.process

  # Stores just the pixels with reference data
  eval(parse(text=(paste("disc.vars$", target.var$name, " <-
                         raster::subset(all.data, '", target.var$name,"')[diff.NA]", sep=""))))

  # Stores all the probability tables
  prob.tab.all.data <- vector('list',length(order.process))
  names(prob.tab.all.data) <- order.process

  # Stores the limits of the intervals
  intervals <- vector('list',length(order.process))
  names(intervals) <- order.process

  #-----#
  # Beginning the discretization process
  for (now in order.process) {

    # Which variable will be processed.
    flush.console()
    cat("\n--------------------\n")
    cat("\nStarting the process for the variable '", now, "'.\n", sep="")

    data.of.now <- raster::subset(all.data, now)[diff.NA]


    # Are you sure to use this intervals?
    ok0 <- F
    while (ok0 == F){

      if (now != target.var$name) {
        dev.new()
        hist(data.of.now, xlab='Values', main=now,
             col='gray80', border = 'white')
      }

      if (now == target.var$name) disc.data <- as.factor(data.of.now)
      else {
        ok1 <- F
        while (ok1 == F) {
          cat(paste("\nChoose an option:\n\n",
                    "(1)  Equidistant criterion\n",
                    "(2)  Quantile criterion\n",
                    "(3)  Clustering criterion\n",
                    "(4)  Entering the limits of the intervals manually\n\n"))
          resp <- readline("Option: ")

          if ((resp == "1") | (resp == "2") | (resp == "3") | (resp == "4")) {
            ok1 <- T
            disc.data <- FUNCTION_discretization(now, data.of.now, resp)
          } else {
            cat("\n\nUnknown method!\nTry again...\n\n")
          }
        }
      }

      if (now != target.var$name) {
        breaks <- c(-Inf,
                    head(unname(sapply(levels(disc.data),
                                       FUNCTION_limits.break)), -1),
                    Inf)
        abline(v=tail(head(breaks, -1), -1), col='red', lwd=2, lty=2)
      }

      # Used to drop unused levels from 'disc.data' variable
      temp1.levels <- levels(disc.data)
      disc.data <- droplevels(disc.data)
      temp2.levels <- levels(disc.data)
      temp.ind <- which(temp1.levels %in% temp2.levels)

      # if (length(temp.ind) > 0) {
      #   cat("\n",
      #       paste(now,"'s levels that were dropped:", sep = ""), "\n",
      #       temp1.levels[-temp.ind], "\n\n\n")
      # }


      eval(parse(text=(
        paste("intervals$", now, " <- levels(disc.data)", sep=""))))
      eval(parse(text=(
        paste("n.intervals <- length(intervals$", now, ")", sep=""))))

      flush.console()
      cat("\n Building the Probability Table of the variable '",
          now, "'.\n", sep="")

      eval(parse(text=(paste("disc.vars$", now, " <- disc.data", sep=""))))
      rm(disc.data)
      invisible(gc())

      # Gets the fathers of 'now'
      fathers <- bnlearn::parents(net, now)
      n.fathers <- length(fathers)

      # Checks if 'now' has fathers and build the CPT
      if (n.fathers == 0) {
        eval(parse(text=(paste("tab <- table(disc.vars$", now,
                               ", dnn = now, useNA = 'no')", sep=""))))
        CPT <- prop.table(tab)
      } else {
        # Building a table with 'now' and its fathers.
        eval(parse(text=(
          paste("prop.tab <- prop.table(table(", now,"=disc.vars$", now,", ",
                paste(fathers, "=disc.vars$", fathers, sep="", collapse=", "),
                ", useNA = 'no'))", sep=""))))

        # Checks if there are some intersections with no one value.
        # The proportions table receive a small value (10^-8)
        # to guarantee the CPT sums 1.
        ind1 <- which(prop.tab == 0)
        if (length(ind1) > 0){
          prop.tab[ind1] <- 10^-8
          CPT <- prop.table(prop.tab,c(2:(n.fathers+1)))
        } else {
          CPT <- prop.table(prop.tab,c(2:(n.fathers+1)))
        }
      }


      if (n.fathers == 0) {
        str <- paste("P(", now, ")", sep="")
        cat("\n * Probability Table - ", str, "\n", sep="")
        print(round(CPT,4))
      } else {
        if (now != target.var$name) {
          str <- paste("P(", now, "|",
                       paste(fathers, sep="", collapse="."), ")", sep="")
          cat("\n * Conditional Probability Table - ", str, "\n", sep="")
          print(ftable(round(CPT,4)))
        }
      }

      is.father   <- is.element(now,target.parents)
      is.child    <- is.element(now,target.children)
      if (now != target.var$name & (is.father | is.child)) {
        eval(parse(text=(paste(
          "temp.table <- table(disc.vars$", target.var$name,
          ", disc.vars$", now, ")",sep = ""))))

        cond.prob <- prop.table(temp.table, 2)

        if (is.father)
          cat("\n\n ", now, " is ", target.var$name, "'s father\n", sep="")

        if (is.child)
          cat("\n\n ", now, " is ", target.var$name, "'s child\n", sep="")

        str <- paste("P(", target.var$name, " | ", now, ")", sep="")
        cat("\n\ * Conditional Probability Table - ", str,"\n",sep="");
        print(round(cond.prob,4), na.print = ".")

        ind <- which(is.nan(cond.prob))
        if (length(ind) >0 ){
          cond.prob[ind] <- 0
          dev.new()
          plot(t(cond.prob), main = str, col = terrain.colors(nclasses+1),
               border="black")
        } else {
          dev.new()
          plot(t(cond.prob), main = str, col = terrain.colors(nclasses+1),
               border="black")
        }
      }


      # To check if the interval labels fit to the format '[x , y)'
      eval(parse(text=(paste("temp <- intervals$", now, sep=""))))
      interval.ok <- all(sapply(sapply(temp, strsplit, ","),
                                FUNCTION_interval.fit))


      # Do you want to use these intervals
      ok5 <- F
      if (now != target.var$name) {

        if (interval.ok & length(temp) >= 2){

          while (ok5 == F) {
            is.it.ok <- readline(
              cat("\n\n\nDo you want to use these intervals?\n",
                  "(y-Yes ; n-No): "))
            if ((is.it.ok == "y") | (is.it.ok == "Y")) {
              ok0 <- T
              ok5 <- T
              eval(parse(text=(
                paste("prob.tab.all.data$", now, " <- CPT", sep = ""))))
              graphics.off()
              cat("\n\n\n")
            } else {
              if ((is.it.ok == "n") | (is.it.ok == "N")) {
                ok5 <- T
                graphics.off()
              } else {
                cat("\n\nInvalid option!\n\n")
              }
            }
          }

        } else {
          cat(paste("\n\n\n*** WARNING ***\n",
                    "Please, choose a smaller number of intervals or\n",
                    "another method for discretization.\n\n",
                    "There must be at least two intervals and\n",
                    "All of them must to fit to the format '[x, y)'",
                    "\n\n"))
          graphics.off()
        }

      } else { # TARGET variable only
        ok0 <- T
        eval(parse(text=(
          paste("prob.tab.all.data$", now, " <- CPT", sep = ""))))
        graphics.off()
      }
    } # Are you sure to use this intervals?
  }
  #-----#

  # Gets the context variables' intervals only
  intervals <- intervals[-which(names(intervals) == target.var$name)]

  # Gets the order to process the context variables (remove target one)
  order.process <- order.process[- which(order.process == target.var$name)]

  # Gets the number of intervals of each variable
  n.intervals <- sapply(intervals, length)

  # Stores the interval limits
  interval.limits <- vector('list', length(order.process))
  names(interval.limits) <- order.process

  aux <- NULL
  for (n in order.process) {
    eval(parse(text=(
      paste("aux <- c(",paste("strsplit(intervals$", n, ", ',')
                              [[",1:(n.intervals[n] - 1),"]][2]",
                              sep = "", collapse = ", "),")"))))
    # Gets the limits of each interval of each variable
    eval(parse(text=(
      paste("interval.limits$", n, " <- sapply(strsplit(aux, ')'), as.numeric)",
            sep=""))))
  }

  # Gets the file paths in the order of the processing
  files <- context.var$files[match(order.process, context.var$name)]

  flush.console()
  cat("\n\n Converting the context variables to categorical data\n",
      "according to the defined intervals.\n",
      "This process may take some time... (please wait)\n")

  # Gets the number of cores to be used in the cluster
  use.cores <- detectCores() - 1

  # Creates a set of copies of R running in parallel
  cl <- makeCluster(use.cores)

  # To register the SNOW parrallel backend
  registerDoSNOW(cl)

  # Initializes the workers to be the same as the master
  invisible(clusterCall(cl, function(x) .libPaths(x), .libPaths()))

  # Assign the values of each listed global variables to
  # the same ones (same names) of each worker.
  invisible(clusterExport(cl, c('files', 'interval.limits'),
                          envir=environment()))

  # Number of iterations to set the progress bar
  iterations <- length(files)

  # Creats a progress bar
  invisible(pb <- txtProgressBar(max = iterations, style = 3))

  # Sets the progress bar
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  result <-
    foreach(i = 1:length(files), .export = c("FUNCTION_categorize.raster"),
            .packages = c('rgdal','raster'), .options.snow = opts) %dopar%
            {
              # To categorize the raster data according to the limits defined by user
              return(FUNCTION_categorize.raster(files[i], interval.limits[[i]]))
            }

  close(pb)
  stopCluster(cl)

  flush.console()
  cat("\n\n\nRecording entered information... (please wait)\n")

  if (!(file.exists("./e-BayNeRD Outcomes"))) {
    dir.create("./e-BayNeRD Outcomes")
  }

  stack.file <- "./e-BayNeRD Outcomes/stack_categorized_variables.tif"

  flush.console()
  cat("\n\n Writing a RasterStack with all categorized context variables.\n",
      "One layer for each variable:",
      paste(order.process, collapse = ", "), " (order of processing).\n\n")

  eval(parse(text=(
    paste("writeRaster(stack(result), filename = '", stack.file, "',
          format = 'GTiff', overwrite = T)", sep = ""))))

  # Takes the user-specified distributions to set the Bayesian Network model
  net.fit <- custom.fit(net,dist=prob.tab.all.data)


  # Step 5 corresponds to entered interval limits,
  # computed probability functions and that the BN model has been built
  eBayNeRDinfo$step.completed <- 0:5
  # Saving the fitted BN model
  eBayNeRDinfo$BNmodel <- net.fit;
  # Saving the auxiliary data
  eBayNeRDinfo$aux.data <- list(stack.file = stack.file,
                                intervals  = intervals)

  # Save the eBayNeRDinfo file inside the current working folder
  save(eBayNeRDinfo, file = "eBayNeRDinfo.RData")
  # Loading eBayNeRDinfo file
  load("eBayNeRDinfo.RData") #loading eBayNeRDinfo file
  eBayNeRDinfo <<- eBayNeRDinfo


  if(Sys.info()[[1]]=='Windows')
  {
    cat('\n\n === Done! ===\n\n')
    winDialog(type="ok",
              paste("Now the menu to generate\nthe Probability Bands (PB)",
                    " has been unlocked."))
    FUNCTION_eBayNeRD.R.menu.activation(
      menu.elements=eBayNeRDinfo$menu,exist.eBayNeRDinfo=T)
  } else {
    cat('\n Done!\n In the next step you should use the function\n',
        'FUNCTION_probability.bands().\n\n',
        'You can also see the influence of each context variable\n',
        'in the calculated probability for the TARGET one by using the function\n',
        'FUNCTION_running.influence.KLscore().\n\n')
  }
  invisible(gc())
  invisible(rm(list=ls()))
  }


FUNCTION_influence.KLscore <- function(obj,target,context) {
  # To compute the Kullback-Leibler distance, which is used to quantify
  # the influence of a variable on another in a Bayesian Network model.
  # (created by Dr Nicolay Balov and adapted by Dr Marcio Pupin Mello)
  #
  # Args:
  #   obj  : a gRain object (BN model)
  #   target : a character with the name of the target variable
  #   context : a character with the name of the context variable
  #
  # Return:
  #   a numeric KL-score


  # The KL computed the distance between conditional and marginal probabilities
  pmarg <- querygrain(obj, nodes=target)[[1]]
  pcond <- t(querygrain(obj, nodes=c(target,context), type="conditional"))
  pmarg <- rep(pmarg,length(pcond)/length(pmarg))
  
  pcond <- pcond/sum(pcond)
  pmarg <- pmarg/sum(pmarg)
  
  # Gets the KL-score
  klscore  <-  sum(pcond * log(pcond / pmarg))
  return(klscore)
}


FUNCTION_running.influence.KLscore <- function() {

  target.name <- eBayNeRDinfo$target.variable$name

  # Bayesian Network model
  net <- eBayNeRDinfo$BNmodel

  # Gets the processing order of the variables
  order.process <- node.ordering(net)
  order.process <- order.process[-(which(order.process==target.name))];

  # Creats a gRain object for queries
  gRain.obj <- as.grain(net)

  temp.result <- data.frame(KL.distance = rep(0,length(order.process)))
  
  for (i in seq_along(order.process))
    temp.result[i, 1] <- round(
      FUNCTION_influence.KLscore(obj = gRain.obj, target = target.name,
                                 context = order.process[i]), 4)
  
  temp.result$Rank <- rank(-temp.result$KL.distance)  # decreasing rank
  temp.result$Percentage <- round(temp.result$KL.distance /
                                    sum(temp.result$KL.distance) * 100, 2)
  id <- order(temp.result$Rank, sort(temp.result$Rank))
  temp.result <- temp.result[id, ]
  temp.result <- temp.result[, c(2,3,1)]
  rownames(temp.result) <- order.process[id]
  
  return(temp.result)
}


FUNCTION_shannon.entropy <- function() {
  nclass <- eBayNeRDinfo$target.variable$classes$class.target

  eval(parse(text = paste(
    "pb", nclass, " <- raster(eBayNeRDinfo$prob.bands[",1:length(nclass),"])", sep="")))
  eval(parse(text = paste(
    "pb0 <- 1 - sum(",paste("pb", nclass, sep="", collapse = ", "),")", sep="")))
  eval(parse(text = paste(
    "pbs.stack <- stack(pb0, ", paste("pb", nclass, sep="", collapse = ", "), ")", sep="")))

  fun <- function(x) {
    return( - sum(x*log2(x)) )
  }
  entropy <- raster::calc(pbs.stack, fun)

  dev.new()
  plot(entropy, colNA='black', col = cm.colors(10), main = "Shannon Entropy")
  writeRaster(entropy, filename = "./e-BayNeRD Outcomes/Entropy.tif", format="GTiff", overwrite=T)

}


FUNCTION_probability.bands <- function() {

  cat("\nLoading data to compute the Probability Band(s)... (please wait)\n")
  cat("\nThis process may take a some time... Go take a coffee! :)\n\n")
  flush.console()

  # Gets Bayesian Network model as a 'grain' object
  net <- as.grain(eBayNeRDinfo$BNmodel)

  # Gets the intervals/Categories of each variable
  intervals <- eBayNeRDinfo$aux.data$intervals

  # Gets data from target variable
  target.data <- eBayNeRDinfo$target.variable
  target.name <- target.data$name

  # Gets the names of the context variables (in order to process)
  var.names <- names(intervals)

  # Creates a list with categories (pixel values)
  # of each discretized context variable
  raster.categories <- lapply(sapply(intervals, length),
                              function(x) return(seq(1:x)))

  # Adds 'NA' value as one category
  raster.categories <- lapply(raster.categories, c, NA)
  names(raster.categories) <- names(intervals)

  # The combination of the context variables' categories
  # creates all possible scenarios to be found
  scenarios <- expand.grid(raster.categories)
  n <- ncol(scenarios)


  #--1--#
  # This will be used to create the probability bands
  # with the same parameters of target variable raster data
  aux.raster <- raster(target.data$file)

  # Gets pixels from study area
  study.area <- which(getValues(aux.raster) !=
                        as.numeric(target.data$classes$class.outside))

  # Path to the stack of categorized context variables
  path.stack <- eBayNeRDinfo$aux.data$stack.file

  # Gets the unstack context variables
  unstack.rasters <- unstack(stack(path.stack))

  # Gets the number of cores to be used in the cluster
  use.cores <- detectCores() - 1
  # Creates a set of copies of R running in parallel
  cl <- makeCluster(use.cores)
  # To register the SNOW parrallel backend
  registerDoSNOW(cl)

  # Extracts pixels of the study area from the context variables rasters.
  # Rows: number of cells (pixels) inside the study area;
  # Cols: number of context variables.
  # To a faster processing, it is necessary to load all of them in memory.
  temp.data <- parSapply(cl, unstack.rasters, extract, y = study.area)

  stopCluster(cl)
  #--1--#

  #--2--#
  # Converts the 'temp.data' variable to 'big.matrix' object, which is allocated
  # to shared memory and used by R workers in the parallel foreach loop.
  options(bigmemory.typecast.warning = F)
  all.data <- as.big.matrix(x = temp.data, type = "integer", separated = F,
                            backingfile    = "eBayNeRD_File.bin",
                            descriptorfile = "eBayNeRD_File.desc")
  m <- ncol(all.data)

  # Gets a description/information to reference
  # a shared 'big.matrix' object (all.data)
  all.data.desc <- describe(all.data)

  rm(temp.data, unstack.rasters, path.stack)
  invisible(gc()) # Garbage collector
  #--2--#

  #--3--#
  # Gets the target variable classes
  classes <- target.data$classes$class.target

  # Creates auxiliary matrices with 1 row and 'ncell' cols
  # These arrays will store the probabilities values to then assign them
  # to the PIs (probability images).
  eval(parse(text=(paste("aux",classes," <- matrix(NA, nrow = 1,
                         ncol = ncell(aux.raster))", sep = ""))))

  # Converts the auxiliary matrices to 'big.matrix' object,
  # which are allocated to shared memory and used by R workers
  # in the parallel foreach loop
  options(bigmemory.typecast.warning = F)
  eval(parse(text=(paste("sharePB",classes," <- as.big.matrix(
                         x = aux", classes,", type = 'double', separated = F,
                         backingfile = 'ProbBand",classes,"_File.bin',
                         descriptorfile = 'ProbBand", classes,"_File.desc')", sep = ""))))

  # # Gets a description/information to reference
  # a shared 'big.matrix' objects (sharePB)
  eval(parse(text=(paste("sharePB", classes, ".desc <-
                         describe(sharePB", classes, ")", sep = ""))))

  # Removes auxiliary matrices to make memory available
  eval(parse(text=(
    paste("rm(",paste("aux", classes, sep = "", collapse = ", "),")",
          sep = ""))))
  invisible(gc)
  #--3--#


  # Gets the number of cores to be used in the cluster
  use.cores <- detectCores() - 1
  # Creates a set of copies of R running in parallel
  cl <- makeCluster(use.cores)
  # To register the SNOW parrallel backend
  registerDoSNOW(cl)

  # Initializes the workers to be the same as the master
  invisible(clusterCall(cl, function(x) .libPaths(x), .libPaths()))

  # Assign the values of each listed global variables to
  # the same ones (same names) of each worker.
  invisible(
    clusterExport(cl, c('m', 'net', 'target.name', 'var.names', 'intervals',
                        'n', 'study.area', 'classes', 'all.data.desc',
                        paste("sharePB", classes, ".desc", sep="")),
                  envir=environment()))

  cat("\n\n",
      paste("Start of the process:", Sys.time() ))
  cat("\n Computing probabilities...\n")


  # Number of iterations to set progress bar
  iterations <- nrow(scenarios)

  # Creats a progress bar
  invisible(pb <- txtProgressBar(max = iterations, style = 3))

  # Sets the progress bar
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  result <-
    foreach(sc = iter(scenarios, by = 'row'),
            .packages = c('bigmemory','rgdal','raster', 'gRbase','gRain'),
            .options.snow = opts) %dopar%
            {

              # Creates a new 'big.matrix' object
              # based on the descriptor information
              # referencing the previously
              # allocated shared-memory matrix (all.data)
              #new.all.data <- attach.big.matrix("eBayNeRD_File.desc")
              new.all.data <- attach.big.matrix(all.data.desc)

              # Creates a new 'big.matrix' object
              # based on the descriptor information
              # referencing the previously
              # allocated shared-memory matrix (sharePB)
              # eval(parse(text=(
              #   paste("PB", classes, " <- attach.big.matrix(
              #         'ProbBand", classes, "_File.desc')", sep = ""))))
              eval(parse(text=(
                paste("PB", classes, " <- attach.big.matrix(
                      sharePB", classes, ".desc)", sep = "")
              )))


              ind.NA <- which(is.na(sc))

              if (length(ind.NA) > 0 ) {

                # Gets the indices of the pixels
                # which fit to the current scenario
                # 'm' is the number of columns of 'new.all.data'
                eval(parse(text=(
                  paste("ind.raster <- which(",
                        paste("new.all.data[,", 1:m, "] == ", sc, sep = "",
                              collapse = " & "), ")", sep = ""))))

                if (length(ind.raster) > 0) {
                  # 'n' is the number of columns of scenarios
                  if (n != ind.NA){

                    # Gets the conditional distribution of the variables.
                    # (first variable in 'nodes' parameter given another ones)
                    CPT <- querygrain(net, nodes =
                                        c(target.name, var.names[-ind.NA]),
                                      type = "conditional")

                    # Gets the dimensions to access the CPT
                    eval(parse(text=(
                      paste("cpt.dims <- c(",
                            paste("intervals[-ind.NA][[",1:(n - ind.NA),"]]
                                  [", sc[-ind.NA], "]",
                                  sep = "", collapse = ", "),")", sep = ""))))

                    # Gets the probability values of target variable
                    # given the values observed on the context variables
                    eval(parse(text=(
                      paste("prob.values <- CPT[",
                            paste("'", cpt.dims, "'", sep="", collapse = ", "),
                            ",]", sep=""))))

                    # Assigns the probability values to the shared-memory file,
                    # which corresponds to the Probability Band.
                    eval(parse(text=(
                      paste("PB", classes, "[1, study.area[ind.raster]] <-
                            prob.values[",2:(length(classes)+1),"]", sep = ""))))

                  } else {

                    # Gets the marginal probability of target variable
                    # in case there are missing data in all context variables
                    prob.values <- querygrain(net, nodes=target.name,
                                              type = "marginal")[[1]]

                    # Assigns the probability values to the shared-memory file,
                    # which corresponds to the Probability Band.
                    eval(parse(text=(
                      paste("PB", classes, "[1, study.area[ind.raster]] <-
                            prob.values[",2:(length(classes)+1),"]", sep = ""))))
                  }
                  }
                } else {

                  # Gets the indices of the pixels
                  # which fit to the current scenario
                  # 'm' is the number of columns of 'new.all.data'
                  eval(parse(text=(
                    paste("ind.raster <- which(",
                          paste("new.all.data[,", 1:m, "] == ", sc, sep = "",
                                collapse = " & "), ")", sep = ""))))

                  if (length(ind.raster) > 0) {

                    # Gets the conditional distribution of the variables.
                    # (first variable in 'nodes' parameter given another ones)
                    CPT <- querygrain(net, nodes = c(target.name, var.names),
                                      type = "conditional")

                    # Gets the dimensions to access the CPT
                    eval(parse(text=(
                      paste("cpt.dims <- c(",
                            paste("intervals[[",1:n,"]][", sc, "]",sep = "",
                                  collapse = ", "), ")", sep = ""))))

                    # Gets the probability values of target variable
                    # given the values observed on the context variables
                    eval(parse(text=(
                      paste("prob.values <- CPT[",
                            paste("'", cpt.dims, "'", sep="", collapse = ", "), ",]",
                            sep=""))))

                    # Assigns the probability values to the shared-memory file,
                    # which corresponds to the Probability Band.
                    eval(parse(text=(
                      paste("PB", classes, "[1, study.area[ind.raster]] <-
                            prob.values[",2:(length(classes)+1),"]", sep = ""))))
                  }
                }
              }

  flush.console()
  cat("\n\n",
      paste("End of the process:", Sys.time() ))

  close(pb)
  stopCluster(cl)
  rm(result)


  # Creates a raster data (Probability Bands) to store the probability values
  eval(parse(text=(
    paste("ProbBand", classes, " <- raster(aux.raster)",sep = ""))))

  # Assign to the Probability Bands the values stored in the shared-memory files
  eval(parse(text=(paste("ProbBand", classes, "[] <- sharePB", classes, "[]",
                         sep = ""))))

  for (i in 1:length(classes)) {
    dev.new()
    # Plots the Probability Bands

    eval(parse(text=(
      paste("plot(ProbBand", i, ",
            main = 'Probability Band - Class ", i, "')", sep = ""))))
  }

  # Probability Bands files path
  files <- paste("./e-BayNeRD Outcomes/ProbBand_class", classes,".tif",
                 sep = "")

  flush.console()
  cat("\n\n\n Writing the Probability Band(s)...\n\n",
      "File(s) path:\n", paste(files, "\n"))

  # Writes the Probability Bands
  eval(parse(text=(
    paste("writeRaster(ProbBand", classes, ", filename = '", files, "',
          format = 'GTiff', overwrite=T)", sep = ""))))


  # Deletes all the shared-memory files in the cache
  rm(all.data, all.data.desc, scenarios)
  eval(parse(text=(
    paste("rm(", paste("sharePB", classes, sep = "", collapse = ", "),")",
          sep = ""))))

  invisible(gc())
  invisible(file.remove("eBayNeRD_File.desc"))
  invisible(file.remove("eBayNeRD_File.bin"))

  eval(parse(text=(
    paste("invisible(file.remove('ProbBand", classes, "_File.desc'))",
          sep = ""))))
  eval(parse(text=(
    paste("invisible(file.remove('ProbBand", classes, "_File.bin'))",
          sep = ""))))

  # Checking if the files were deleted
  eval(parse(text=(
    paste("if (file.exists('eBayNeRD_File.bin') | ",
          paste("file.exists('ProbBand", classes, "_File.bin')", sep = "",
                collapse = " | "), ") {
          cat('\n\n\n Some .bin files in work folder could not be deleted.\n',
          'If you intend to run this step again, please delete them first.\n\n\n')}"))))


  #Step 6 corresponds to the entry of the  Probability Band(s) generated.
  eBayNeRDinfo$step.completed <- 0:6
  eBayNeRDinfo$prob.bands <- files

  # Save the eBayNeRDinfo file inside the current working folder
  save(eBayNeRDinfo,file="eBayNeRDinfo.RData")
  load("eBayNeRDinfo.RData") #loading eBayNeRDinfo file
  eBayNeRDinfo<<-eBayNeRDinfo

  cat("\n Computing the Shannon Entropy...\n")
  FUNCTION_shannon.entropy()

  if (Sys.info()[[1]] == 'Windows') {
    cat('\n\n === Done! ===\n\n')
    winDialog(type="ok",
              paste("Now the menu to generate the\n",
                    "clissified image has been unlocked."))
    FUNCTION_eBayNeRD.R.menu.activation(
      menu.elements=eBayNeRDinfo$menu, exist.eBayNeRDinfo=T)
  } else {
    cat('\n Done!\n In the next step you should use the function\n',
        'FUNCTION_read.target.variable.for.testing().\n\n')
  }

  invisible(gc())
  invisible(rm(list=ls()))
  }


#
# #function to generate the classified image
# FUNCTION_classification <- function(pbs.files=eBayNeRDinfo$prob.bands)
#   {
#       cat("\n\n")
#       cat("  - e-BayNeRD will now generate the Classified Image.\n")
#       cat("  - This process may take some minutes. After the process,\n")
#       cat("      is done e-BayNeRD will write the Classified Image\n")
#       cat("      in the subfolder ./e-BayNeRD Outcomes.\n\n\n")
#
#       ok <- F
#       while(ok==F)
#       {
#         is.it.ok <- readline("Continue? (y-Yes ; n-No): ")
#         if ((is.it.ok=="y")|(is.it.ok=="Y")) ok <- T else
#           if ((is.it.ok=="n")|(is.it.ok=="N")) stop("\n\nAborted by the user!\n\n",call.=F) else
#             cat("\n\nInvalid option!\n\n")
#       };rm(ok)
#
#       cat("\n\n\n Reading data... (please wait)");
#
#       eval(parse(text=(paste("pb",1:length(pbs.files)," <- raster('",pbs.files,"')",sep=""))))
#       eval(parse(text=(paste("pb0 <- 1-sum(",paste("pb",1:length(pbs.files),sep="",collapse=","),")",sep=""))))
#       eval(parse(text=(paste("PI <- stack(",paste("pb",0:length(pbs.files),sep="",collapse=","),")",sep=""))))
#
#       cat("\n\n\n Generating the Classified Image...\n This process can take some minutes... (please wait)");
#
#       #Classified image
#       Cimg <- raster::which.max(PI)
#       Cimg <- Cimg-1
#
#       file.path <- paste("./e-BayNeRD Outcomes/ClassImage.tif",sep="")
#
#       cat("\n\n\n Recording the Classified Image... (please wait)");
#       if(!(file.exists("./e-BayNeRD Outcomes"))) dir.create("./e-BayNeRD Outcomes")
#       eval(parse(text=(paste("writeRaster(Cimg,filename='",file.path,"', format='GTiff', overwrite=TRUE)",sep=""))))
#
#       eBayNeRDinfo$step.completed <- 0:7 #step 7 corresponds to classified image generated
#       eBayNeRDinfo$class.image <- file.path;
#       save(eBayNeRDinfo,file="eBayNeRDinfo.RData") #save the eBayNeRDinfo file inside the current working folder defined earlier
#       load("eBayNeRDinfo.RData") #loading eBayNeRDinfo file
#       eBayNeRDinfo<<-eBayNeRDinfo
#
#
#       if(Sys.info()[[1]]=='Windows')
#       {
#         cat('\n\n === Done! ===\n\n')
#         winDialog(type="ok","Now the menu to read the target variable for testing has been unlocked.")
#         FUNCTION_eBayNeRD.R.menu.activation(menu.elements=eBayNeRDinfo$menu,exist.eBayNeRDinfo=T)
#       } else cat('\n\nDone! In the next step you may use the function FUNCTION_read.target.variable.for.testing().\n\n')
#
#       invisible(gc())
#       invisible(rm(list=ls()))
#     }


#function used to read the target variable data (reference data for testing)
FUNCTION_read.target.variable.for.testing <- function() {

  target.var <- eBayNeRDinfo$target.variable

  cat("\n\n\nInstructions:\n\n")

  cat(    "   - TARGET data for testing data should be in RASTER format (i.e. it is an image);\n")
  cat(    "   - It must be a GeoTiff file (e.g. 'tif' file extension);\n")
  cat(    "   - It is advised that the raster data is geographically referenced\n")
  cat(    "     matching the TARGET image for training previously entered;\n")
  cat(    "   - It must have only one band (not staked layers);\n")
  cat(    "   - It must have at least two labels (corresponding to the 'target' and 'no-target' thematic classes).\n")
  cat("\n\n")
  cat("Example of entry: D:/project/targetTE.tif\n\n")

  cat("\n\n")
  ok <- F
  while(ok==F)  {
    is.it.ok <- readline("Do you want to use the same TARGET image used as reference for training?\n(y-Yes ; n-No): ")
    if ((is.it.ok=="y")|(is.it.ok=="Y")) {ok <- T;use.same <- T} else
      if ((is.it.ok=="n")|(is.it.ok=="N")) {ok <- T;use.same <- F} else
        cat("\n\nInvalid option!\n\n")
  }
  rm(ok,is.it.ok)

  if (use.same==F) {
    ok <- F
    while(ok==F) {
      name.of.target.file <- readline(paste0("\nEnter the file name (with path) of the\n",
                                            "target data image (including the extension): "))
      if (file.exists(name.of.target.file)==T) ok <- T else cat("\n\nThis file (path) does not exist!\n")
    }
    rm(ok)
  } else name.of.target.file <- target.var$file

  #loading data
  temp.data <- raster(name.of.target.file)
  temp.target <- raster(target.var$file)

  #checking the requirements
  if (nbands(temp.data)>1)
    stop(cat("\n\nThe entered data image has more than one band!!!\n\n\n\n"))
  if(!(compareRaster(temp.data, temp.target, extent=F, rowcol=F, crs=T, res=F,
                     orig=F, rotation=F, values=F, stopiffalse=F, showwarning=F))) {

    cat("\n\nBased on the TARGET image previously entered,\n
        this testing image data has no compatible coordinate
        reference systems!!!\n\n\n\n")
    cat("\n\nChanging it... This process can take several minutes...
        (please wait)\n\n\n");
    target.projection <- projection(temp.target)
    temp.data <- projectRaster(temp.data,crs=target.projection)
  }

  if(!(compareRaster(temp.data, temp.target, extent=F, rowcol=F, crs=F, res=T,
                     orig=F,rotation=F,values=F,stopiffalse=F,showwarning=F))) {

    cat("\n\nBased on the TARGET image previously entered,\nt
        his testing image data has no compatible pixel size!!!\n\n")
    ok <- F
    while(ok==F) {
      is.it.ok <- readline("Would you like to resample it? (y-Yes ; n-No): ")
      if ((is.it.ok=="y")|(is.it.ok=="Y")) {ok <- T;resampling <- T} else
        if ((is.it.ok=="n")|(is.it.ok=="N")) {ok <- T;resampling <- F;
        stop(cat("\n\nThe entered data image must has the
                 compatible pixel size with TARGET (reference for training)
                 variable!!!\n\n\n\n"))} else cat("\n\nInvalid option!\n\n")
        };rm(ok)

    if (resampling==T) {
      cat("\n\nResampling it... (please wait)\n\n\n");
      temp.data <- resample(temp.data,temp.target,method='ngb')
    }
        }

  if(!(compareRaster(temp.data, temp.target, extent=T, rowcol=F, crs=F, res=F,
                     orig=F,rotation=F,values=F,stopiffalse=F,showwarning=F)))
    temp.data <- resample(temp.data,temp.target)

  #just giving a feedback
  cat("\n\n\nReading... (please wait)");
  classes <- as.character(raster::unique(temp.data))
  n.classes <- length(classes)

  target.classes <- length(target.var$classes$class.target)
  if (n.classes<(target.classes+1))
    stop ("\nThe entered data image should have at least ",
          target.classes+1," classes.\n")

  #setting labels to classes
  cat("\n\n    The entered data image has",n.classes,"labels:\n");
  cat("      ",classes,"\n\n");

  if(use.same==T){
    cat("\nThe labels of this data image are the same of the training data?\n")
    cat("Check them if are right:\n\n")
    if (target.var$classes$class.outside=="-999999") cat("- Pixels outside the study area: \n") else  cat("- Pixels outside the study area:",target.var$classes$class.outside,"\n")
    if (target.var$classes$class.NA.inside=="-888888") cat("- Pixels with no-data inside the study area: \n") else cat("- Pixels with no-data inside the study area:",target.var$classes$class.NA.inside,"\n")
    cat("- Pixels of the 'target' class: ",target.var$classes$class.target,"\n\n")

    ok <- F
    while(ok==F){
      is.it.ok <- readline("Are labels correct? (y-Yes ; n-No): ")
      if ((is.it.ok=="y")|(is.it.ok=="Y")) {ok <- T;ok2 <- T; temp3 <- length(target.var$classes$class.target)} else
        if ((is.it.ok=="n")|(is.it.ok=="N")) {ok <- T;ok2 <- F;use.same <- F} else
          cat("\n\nInvalid option!\n\n")
    };rm(ok,is.it.ok)
    cat("\n\n")
    temp <- NULL
    if(ok2==T) temp <- list(class.outside=target.var$classes$class.outside,class.NA.inside=target.var$classes$class.NA.inside,class.target=target.var$classes$class.target)
  }

  if(use.same==F){
    temp <- list(class.outside="-999999",class.NA.inside="-888888",class.target="-777777")

    #reading the label corresponding to pixels outside the study area (mask)
    ok <- F
    while(ok==F){
      temp1 <- ""
      temp1 <- readline(
         paste0("\nEnter the label corresponding to pixels outside\nthe study",
               " area (left blank if there is not): "))
      if (temp1=="") ok <- T else
        if (sum(temp1==classes)>0) {ok <- T;temp$class.outside <- temp1} else {rm(temp1);cat("\n\nThis label does not exist!\n")}
    };rm(ok)

    #reading the label corresponding to pixels with no data inside the study area
    ok <- F
    while(ok==F){
      temp2 <- ""
      temp2 <- readline(
        paste0("\nEnter the label corresponding to pixels with no-data\n",
               "inside the study area (left blank if there is not): "))
      if (temp2=="") ok <- T else
        if (temp2==temp1) {rm(temp2);cat("\n\nLabel already used!\n")} else
          if (sum(temp2==classes)>0) {ok <- T;temp$class.NA.inside <- temp2} else {rm(temp2);cat("\n\nThis label does not exist!\n")}
    };rm(ok)

    #reading the labels corresponding to pixels of the class "target"
    ok1 <- F
    while(ok1==F){
      cat("\n\nHow many classes have the 'target'?\n")
      temp3 <- as.numeric(readline("Answer: "))
      if ((is.na(temp3)) | (temp3==0) | (temp3=="") | ((temp3-floor(temp3))!=0)) cat("\n\nInvalid value!\n")
      else {
        temp4 <- NULL
        for (i in 1:temp3){
          ok2 <- F
          while(ok2==F){
            temp5 <- readline(
              paste0("\nEnter the ", i, "th label",
                     " to pixels of the 'target' class: "))
            if (temp5=="") cat("\n\nYou must enter a valid label!\n") else
              if ((temp5==temp1)|(temp5==temp2)|(sum(temp4==temp5)>0)) {rm(temp5);cat("\n\nLabel already used!\n")} else
                if (sum(temp5==classes)>0) {ok2 <- T;temp4 <- c(temp4,temp5)} else {rm(temp5);cat("\n\nThis label does not exist!\n")}
          }
        }
      };ok1 <- T;
    }
    temp$class.target <- temp4; #labels of 'target' class
  }

  #checking class number consistence
  shoud.have.at.least.X.classes <- temp3+1 #corresponding to, at least, the classes "target" and "no-target"
  if (temp$class.outside!="-999999") shoud.have.at.least.X.classes <- shoud.have.at.least.X.classes+1
  if (temp$class.NA.inside!="-888888") shoud.have.at.least.X.classes <- shoud.have.at.least.X.classes+1
  if (n.classes<shoud.have.at.least.X.classes) stop(cat("\n\nAccording to your entries the target data image should have at least",shoud.have.at.least.X.classes,"labels. However it has only",n.classes,"labels."))

  #just giving a feedback
  cat("\n Recording entered information... (please wait)");


  eval(parse(text=(
    paste("temp.data[ (temp.data == ", as.numeric(temp$class.outside),") |
          (temp.data == ", as.numeric(temp$class.NA.inside),") ] <- NA",
          sep = ""))))

  eval(parse(text=(
    paste("temp.data[ temp.data != ", as.numeric(temp$class.target), "] <-
          0", sep = ""))))

  name.target <- sapply(lapply(strsplit((sub("/.*","",(sapply(lapply(strsplit((paste(strsplit(name.of.target.file,".tif"))), NULL), rev), paste, collapse="")))), NULL), rev), paste, collapse="")

  file.path <- paste("./e-BayNeRD Outcomes/",name.target,"_test.tif",sep="")
  writeRaster(temp.data,filename=file.path,format='GTiff',overwrite=T)

  eBayNeRDinfo$step.completed <- 0:7 
  eBayNeRDinfo$ref.for.test <- list(file=file.path,classes=temp)
  save(eBayNeRDinfo,file="eBayNeRDinfo.RData") #save the eBayNeRDinfo file inside the current working folder defined earlier
  load("eBayNeRDinfo.RData") #loading eBayNeRDinfo file
  eBayNeRDinfo<<-eBayNeRDinfo


  if(Sys.info()[[1]]=='Windows')
  {
    cat('\n\n === Done! ===\n\n')
    winDialog(type="ok","Now the menu to assess the accuracy has been unlocked.")
    FUNCTION_eBayNeRD.R.menu.activation(menu.elements=eBayNeRDinfo$menu,exist.eBayNeRDinfo=T)
  } else {
    cat('\n\n Done!\n In the next step you may use the function\n',
              'FUNCTION_find.bestTPV(criterion.number = ).\n\n')
    }

  invisible(gc())
  invisible(rm(list=ls()))
  }


#function to calculate the kappa or tau coefficient
FUNCTION_accuracy.index<-function(confusion.matrix,index="kappa")
{
  m1<-confusion.matrix
  nc<-dim(m1)[1]
  if (nc!=dim(m1)[2]) stop ("The entered confusion matrix is not a square matrix!!!")
  m2<-cbind(m1,margin.table(m1,1));m2<-rbind(m2,margin.table(m2,2))

  #theta's calculation
  N<- sum(m1)
  theta1<- sum(diag(m1)) / N
  theta2<- sum(m2[,nc+1][1:nc]*m2[nc+1,][1:nc]) / (N^2)
  theta3<-sum(diag(m1)*(m2[,nc+1][1:nc]+m2[nc+1,][1:nc])) / (N^2)
  theta4<-0
  for (i in 1:nc)
    for (j in 1:nc)
      theta4<-theta4 + (m1[i,j] * ( (sum(m1[j,]) + sum(m1[,i])) ^2));
  theta4<-theta4 / (N^3)

  #index calculation
  #exatidao.global<-theta1
  #var.exatidao.global<-(theta1*(1-theta1))/N
  if (index=="kappa")
  {
    index.value<-(theta1-theta2)/(1-theta2)
    var.index.value<- (1/N) * ( ((theta1*(1-theta1))/((1-theta2)^2)) + ((2*(1-theta1)*((2*theta1*theta2)-theta3))/((1-theta2)^3)) + ((((1-theta1)^2)*(theta4-(4*(theta2^2))))/((1-theta2)^4)) )
  } else
    if (index=="tau")
    {
      index.value<-(theta1-(1/nc))/(1-(1/nc))
      var.index.value<-(theta1/N)*((1-theta1)/((1-(1/nc))^2))
    } else stop ("Index name not implemented yet!!!")
  return(list(index.name=index,index.value=index.value,variance.index=var.index.value))
}


#function to compute the sample size needed
FUNCTION_sample.size <- function(alfa=0.05, ref = ref)
{
  ftab <- freq(ref) #frequency table
  if ((sum(is.na(ftab))>0)) ftab <- head(ftab,-1)

  totalnpixels <- sum(ftab[,2])

  B <- qchisq(p = (1-(alfa/nrow(ftab))), df=1) #ftab's row are all the classes

  result <- data.frame(ID=ftab[,1], n.elements=ftab[,2], proportion=ftab[,2]/totalnpixels, sample.size.per.class=-1,worst.case=-1)
  result$sample.size.per.class <- ceiling(((B)*(result$proportion)*(1-result$proportion)/(alfa^2))/nrow(ftab)) #based in multinomial distribution (see Congalton and Green 2009, page 78)
  result$worst.case <- rep(ceiling(((B)/(4*(alfa^2)))/nrow(ftab)),nrow(ftab))

  return(result)
}


FUNCTION_find.bestTPV<-function(n.slices = 101, criterion.number = 1) {

  n <- length(eBayNeRDinfo$target.variable$classes$class.target)
  if (n > 1) {
    stop("This method has not yet been inplemented\n",
         "for target variables with more than two classes (absence and presense)")
  }


  target.class.number <-
    as.numeric(eBayNeRDinfo$ref.for.test$classes$class.target)

  PB  <- raster(eBayNeRDinfo$prob.bands)
  ref <- raster(eBayNeRDinfo$ref.for.test$file)

  cat("\n\n This procedure will define the a value of probability in the\n",
      "Probability Band to be used as threshold probability value (TPV)\n",
      "to generete the best thematic map, based on the chosen criterion.\n",
      "As defaut, it will be used all possible integer values between 0 and 1.
      \n\n")

  cat("\n Choose one criterion option:\n\n")
  cat("   (0) The value entered manually;\n")
  cat("   (1) The nearest to the 100% sensitivity and 100% specificity point;\n")
  cat("   (2) The minimum difference between the sensitivy and specificity;\n")
  cat("   (3) The highest accuracy index;\n")
  cat("   (4) The highest Kappa index;\n")
  cat("   (5) The most similar area (number of pixels) according to reference data;\n")
  cat("   (6) The mininum difference between omission and inclusion erros.\n")

  ok <- FALSE
  while (!ok) {
    opt <- readline("\n Answer: ")
    if (opt %in% 0:6){
      ok <- TRUE
    } else {
      cat("\n Wrong option!\n")
    }
  }

  if ( (minValue(PB) < 0) | (maxValue(PB) > 1))
    cat ("\n\nATENTION:\n",
         "Your Probability Band has values of probability\n",
         "outside of the interval [0,1].\n\n")


  result <- data.frame(slice.level = 1:n.slices,
                       prob.value = seq(0,1,(1-0) / (n.slices-1)),
                       n.pixels.target = -1, difference.with.ref = -1,
                       pixels.inclusion.error = -1, pixels.omission.error = -1,
                       sensitivity = -1, specificity = -1, accuracy = -1,
                       kappa = -1, kappa.var = -1)

  #asking about sampling
  cat("\n")
  ok<-F

  while(ok==F)
  {
    is.it.ok<-readline("Would you like to make a sampling for accuracy assessment?\n(y-Yes ; n-No): ")
    if ((is.it.ok=="y")||(is.it.ok=="Y")) {ok<-T;sampling<-T} else
      if ((is.it.ok=="n")||(is.it.ok=="N")) {ok<-T;sampling<-F} else cat("\n\nInvalid option!\n\n")
  }

  if(sampling==T)
  {
    cat("\n\nThe sampling size is based on the Multinomial statistical distribution.\n")
    ok<-F
    while(ok==F)
    {
      conf<-readline("Enter the confidence value (recommended = 0.95): ")
      conf<-as.numeric(conf)
      if((is.na(conf)) || (conf>1) || (conf<0)) cat("\n\nInvalid value!\n") else ok<-T
    };rm(ok)

    cat("\n\nComputing...\n")
    invisible(flush.console())

    cat("\n\nThe follow table summarises the computations:\n\n")

    sample.size.computations <- FUNCTION_sample.size(alfa = (1-conf), ref = ref)

    print(sample.size.computations)

    cat("\n\n")
    ok<-F
    while(ok==F)
    {
      sample.size<-readline("Enter the desired sample size (each class): ")
      sample.size<-as.numeric(sample.size)
      if((is.na(sample.size)) || (sample.size>min(sample.size.computations$n.elements)) || (sample.size<1) || ((sample.size-floor(sample.size))!=0)) cat("\n\nInvalid value!\n") else ok<-T
    };rm(ok)

    cat("\n\nSampling...\n")
    invisible(flush.console())

    n.classes <- length(eBayNeRDinfo$ref.for.test$classes$class.target)+1


    ref2 <- raster(ref)
    for (i in 1:n.classes)
    {
      trash.sample <- sample(x = which(getValues(ref) ==
                                         sample.size.computations$ID[i]),
                             size = sample.size, replace = F)
      ref2[trash.sample] <- ref[trash.sample]
      rm(trash.sample)
    }
    ref <- ref2
    rm(ref2)
  }

  totalnpixels <- length(which(!is.na(getValues(ref))))

  n.pixels.target <- length(which(getValues(ref) == target.class.number))

  cat("\nThis process can take some time... (please wait!)\n")
  cat("\n Computing the best thematic map...\n")

  # Creats a progress bar
  invisible(pb <- txtProgressBar(max = n.slices, style = 3))

  # Sets the progress bar
  progress <- function(n) setTxtProgressBar(pb, n)

  temp <- matrix(0, ncol = 2, nrow = 2)
  for (i in 1:n.slices)
  {
    class.map <- PB
    class.map[ PB <  result$prob.value[i] ] <- 0
    class.map[ PB >= result$prob.value[i] ] <- 1

    temp[1,1] <- length(which( (getValues(ref) == target.class.number) &
                                 (getValues(class.map) == 1)))
    temp[2,1] <- length(which( (getValues(ref) == target.class.number) &
                                 (getValues(class.map) == 0)))
    temp[1,2] <- length(which( (getValues(ref) != target.class.number) &
                                 (getValues(class.map) == 1)))
    temp[2,2] <- length(which( (getValues(ref) != target.class.number) &
                                 (getValues(class.map) == 0)))

    result$pixels.inclusion.error[i] <- temp[1,2]
    result$pixels.omission.error[i]  <- temp[2,1]

    result$sensitivity[i] <- temp[1,1]/sum(temp[,1])
    result$specificity[i] <- temp[2,2]/sum(temp[,2])

    result$n.pixels.target[i]     <- length(which(getValues(class.map)==1))
    result$difference.with.ref[i] <-
      (result$n.pixels.target[i] - n.pixels.target) / n.pixels.target

    result$accuracy[i] <- (temp[1,1] + temp[2,2]) / sum(temp)

    temp.kappa <- FUNCTION_accuracy.index(confusion.matrix = temp, index="kappa")
    result$kappa[i]     <- temp.kappa$index.value
    result$kappa.var[i] <- temp.kappa$variance.index
    rm(temp.kappa)

    progress(i)
    invisible(gc())

  }
  rm(temp,class.map)


  if(criterion.number==0)
  {#best-TPV entered manually
    criterion<-"value entered manually"

    ok2<-F
    while(ok2==F) {
      ok<-F
      while(ok==F) {
        cat("\n\n")
        prob.read<-readline("Enter the best-TPV (a value between 0 and 1): ")
        prob.read<-as.numeric(prob.read)
        if((is.na(prob.read)) || (prob.read<0) || (prob.read>1))
          cat("\n\nInvalid value!\n") else ok<-T
      }
      cat("\nPlease wait...\n")

      result2 <- data.frame(slice.level = 1, prob.value = prob.read,
                            n.pixels.target = -1, difference.with.ref = -1,
                            pixels.inclusion.error = -1,
                            pixels.omission.error = -1, sensitivity = -1,
                            specificity = -1, accuracy = -1,
                            kappa = -1, kappa.var = -1)

      temp <- matrix(0, ncol = 2, nrow = 2)

      bestTPV <- 1

      class.map <- PB
      class.map[ PB <  result2$prob.value[1] ] <- 0
      class.map[ PB >= result2$prob.value[1] ] <- 1

      temp[1,1] <- length(which( (getValues(ref) == target.class.number) &
                                   (getValues(class.map) == 1)))
      temp[2,1] <- length(which( (getValues(ref) == target.class.number) &
                                   (getValues(class.map) == 0)))
      temp[1,2] <- length(which( (getValues(ref) != target.class.number) &
                                   (getValues(class.map) == 1)))
      temp[2,2] <- length(which( (getValues(ref) != target.class.number) &
                                   (getValues(class.map) == 0)))

      result2$pixels.inclusion.error <- temp[1,2]
      result2$pixels.omission.error  <- temp[2,1]

      result2$sensitivity <- temp[1,1]/sum(temp[,1])
      result2$specificity <- temp[2,2]/sum(temp[,2])

      result2$n.pixels.target     <- length(which(getValues(class.map)==1))
      result2$difference.with.ref <-
        (result2$n.pixels.target - n.pixels.target) / n.pixels.target

      result2$accuracy <- (temp[1,1] + temp[2,2]) / sum(temp)

      temp.kappa <- FUNCTION_accuracy.index(confusion.matrix = temp, index="kappa")
      result2$kappa     <- temp.kappa$index.value
      result2$kappa.var <- temp.kappa$variance.index
      rm(temp.kappa)

      rm(temp)
      temp<-1

      cat("\n\nSummary:\n\n")
      print(result2[,2:length(result2[1,])])
      cat("\n\n")
      is.it.ok<-readline("Are you sure about this value? (y-Yes ; n-No): ")
      if ((is.it.ok=="y")||(is.it.ok=="Y")) {
        ok2<-T
      } else {
        ok2 <- F
        rm(temp,result2,bestTPV,prob.read,class.map)
      }
    }
  } else
    if(criterion.number==1)
    {#best-TPV based on the nearest 100% sensitivity and 100% specificity point
      temp<-which(sqrt(((result$specificity-1)^2)+((result$sensitivity-1)^2))==min(sqrt(((result$specificity-1)^2)+((result$sensitivity-1)^2))))
      #if (length(temp)>1) cat("\n\nAn analysis of the Probability Image (PI) suggests that, if you could\nuse more intervals in the input variables to generate the PI, you\nprobably could be more accurate in selecting the best-TPV. That's\nwhy as more accurated is the discrete probability distribution then\nit becomes more accurate when compared to the continuous probability\nfunction.\n\n")
      bestTPV<-max(temp)
      result2<-result
      criterion<-"nearest 100% sensitivity and 100% specificity point"
    } else
      if(criterion.number==2)
      {#best-TPV based on the minimum difference between sensitivity and specificity
        temp<-which(abs(result$specificity-result$sensitivity)==min(abs(result$specificity-result$sensitivity)))
        #if (length(temp)>1) cat("\n\nAn analysis of the Probability Image (PI) suggests that, if you could\nuse more intervals in the input variables to generate the PI, you\nprobably could be more accurate in selecting the best-TPV. That's\nwhy as more accurated is the discrete probability distribution then\nit becomes more accurate when compared to the continuous probability\nfunction.\n\n")
        bestTPV<-max(temp)
        result2<-result
        criterion<-"minimum difference between the sensitivity and specificity"
      } else
        if(criterion.number==3)
        {#best-TPV based on the highest accuracy index
          temp<-which(result$accuracy==max(result$accuracy))
          #if (length(temp)>1) cat("\n\nAn analysis of the Probability Image (PI) suggests that, if you could\nuse more intervals in the input variables to generate the PI, you\nprobably could be more accurate in selecting the best-TPV. That's\nwhy as more accurated is the discrete probability distribution then\nit becomes more accurate when compared to the continuous probability\nfunction.\n\n")
          bestTPV<-max(temp)
          result2<-result
          criterion<-"accuracy index"
        } else
          if(criterion.number==4)
          {#best-TPV based on the highest kappa index
            temp<-which(result$kappa==max(result$kappa))
            #if (length(temp)>1) cat("\n\nAn analysis of the Probability Image (PI) suggests that, if you could\nuse more intervals in the input variables to generate the PI, you\nprobably could be more accurate in selecting the best-TPV. That's\nwhy as more accurated is the discrete probability distribution then\nit becomes more accurate when compared to the continuous probability\nfunction.\n\n")
            bestTPV<-max(temp)
            result2<-result
            criterion<-"kappa index"
          } else
            if(criterion.number==5)
            {#best-TPV based on the most similar area (number of pixels) according to a reference data
              temp<-which(abs(result$difference.with.ref)==min(abs(result$difference.with.ref)))
              #if (length(temp)>1) cat("\n\nAn analysis of the Probability Image (PI) suggests that, if you could\nuse more intervals in the input variables to generate the PI, you\nprobably could be more accurate in selecting the best-TPV. That's\nwhy as more accurated is the discrete probability distribution then\nit becomes more accurate when compared to the continuous probability\nfunction.\n\n")
              bestTPV<-max(temp)
              result2<-result
              criterion<-"smaller area difference"
            } else
              if(criterion.number==6)
              {#best-TPV based on the minimum difference between omission and inclusion errors

                temp<-which(abs(result$pixels.inclusion.error-result$pixels.omission.error)==min(abs(result$pixels.inclusion.error-result$pixels.omission.error)))
                #if (length(temp)>1) cat("\n\nAn analysis of the Probability Image (PI) suggests that, if you could\nuse more intervals in the input variables to generate the PI, you\nprobably could be more accurate in selecting the best-TPV. That's\nwhy as more accurated is the discrete probability distribution then\nit becomes more accurate when compared to the continuous probability\nfunction.\n\n")
                bestTPV<-max(temp)
                result2<-result
                criterion<-"best omission-inclusion compensation"
              } else stop("\n\nInvalid (criterion.number) parameter.
                          Criterion not implemented yet!!!\n\n",call.=F)


  #drawing the accuracy indeces as function of the TPV
  dev.new()
  plot(result$prob.value,result$prob.value,type="n",xlim=c(0,1.5),ylim=c(0,1),axes=F,main=paste("Criterion:\n",criterion,sep=""),xlab="Target Probability Value",ylab="Index Value")
  axis(2,seq(0,1,0.1),round(seq(0,1,0.1),2))
  axis(1,seq(0,1,0.1),round(seq(0,1,0.1),2))
  box()
  points(result$prob.value,result$sensitivity,type="l",lty=1,col=1, lwd=2)
  points(result$prob.value,result$specificity,type="l",lty=1,col=2, lwd=2)
  points(result$prob.value,result$accuracy,type="l",lty=1,col=3, lwd=2)
  points(result$prob.value,result$kappa,type="l",lty=1,col=4, lwd=2)
  lines(c(result2$prob.value[bestTPV],result2$prob.value[bestTPV]),c(0,1),col='grey',lty=3,lwd=2)
  text(x=(result2$prob.value[bestTPV]+0.015),y=0.02,label=paste("best-TPV = ",round(result2$prob.value[bestTPV]*100,2),"%",sep=""),col=1,pos=4)
  legend(x=1.1,y=0,xjust=0,yjust=0,lty=1, lwd=2,legend=c("Sensitivity","Specificity","Accuracy","Kappa"),col=1:4)

  #computing about ROC curve
  y <- result$sensitivity
  x <- 1-result$specificity
  temp.area<-0
  for (j in 2:n.slices)
    temp.area<-temp.area+((x[j-1]-x[j])*((y[j-1]+y[j])/2))
  ROC.curve <- temp.area

  #drawing the ROC curve
  dev.new()
  plot(x,y,type="l",col=2,xlim=c(0,1),ylim=c(0,1),axes=F,
       xlab="Specificity",ylab="Sensitivity",
       main="ROC curve",sub=paste("Criterion: ",criterion,sep=""), lwd=2)
  axis(2)
  axis(1,seq(0,1,0.1),round(seq(1,0,-0.1),2))
  box()

  #this is necessary if criterion.numeber==0
  if(criterion.number == 0) {
    y <- result2$sensitivity
    x <- 1-result2$specificity
  }

  lines(c(0,1),c(0,1),col="grey",lty=1,type="l", lwd=1.5)
  points(c(x[bestTPV],x[bestTPV]),c(y[bestTPV],y[bestTPV]),col=1, lwd=2)
  lines(c(x[bestTPV],x[bestTPV]),c(0,y[bestTPV]),lty=2,col=1)
  lines(c(0,x[bestTPV]),c(y[bestTPV],y[bestTPV]),lty=2,col=1)
  text(x=x[bestTPV],y=y[bestTPV]-.02,label=paste("best-TPV = ",round(result2$prob.value[bestTPV]*100,2),"%",sep=""),col=1,pos=4)
  text(x=-0.05,y=y[bestTPV]+0.03,label=paste(round(result2$sensitivity[bestTPV]*100,2),"%",sep=""),col="black",pos=4)
  text(x=x[bestTPV],y=0,label=paste(round(result2$specificity[bestTPV]*100,2),"%",sep=""),col="black",pos=4)
  legend(x=0.65, y=0, xjust=0,yjust=0,lty=1,lwd=2,legend=c("ROC Curve","Random Guess"),col=c("red","grey"))
  text(x=0.75, y=0.18, paste("AUC = ", round(ROC.curve,3)*100, "%", sep=""))

  cat("\n\n\n Writing the final thematic map (finalmap.tif)...\n\n")
  class.map <- PB
  class.map[ PB <  result2$prob.value[bestTPV] ] <- 0
  writeRaster(class.map, filename = "./e-BayNeRD Outcomes/finalmap.tif",
              format = 'GTiff', overwrite=T)

  cat("\n")
  cat(paste(" Based on the criterion:\n'",criterion,"':\n",sep=""))
  cat(paste0("\n   - Probability value: ", round(result2[bestTPV, 2], 3)))
  cat(paste0("\n   - Number of target pixels: ", result2[bestTPV, 3]))
  cat(paste0("\n   - Difference with reference: ", round(result2[bestTPV, 4], 3)))
  cat(paste0("\n   - Inclusion error (number of pixels): ", result2[bestTPV, 5]))
  cat(paste0("\n   - Omission error (number of pixels): ", result2[bestTPV, 6]))
  cat(paste0("\n   - Sensitivity: ", round(result2[bestTPV, 7], 3)))
  cat(paste0("\n   - Specificity: ", round(result2[bestTPV, 8], 3)))
  cat(paste0("\n   - Accuracy Index: ", round(result2[bestTPV, 9], 3)))
  cat(paste0("\n   - Kappa Index: ", round(result2[bestTPV, 10], 3)))
  cat(paste0("\n\n   - Area under the ROC curve: ", round(100*ROC.curve, 2), "%"))

  eBayNeRDinfo$accuracy$general.table <- result
  eBayNeRDinfo$accuracy$criterion     <- criterion
  eBayNeRDinfo$accuracy$bestTPV       <- result2[bestTPV,]
  eBayNeRDinfo$accuracy$ROC.curve     <- ROC.curve

  rm(result,criterion,temp,bestTPV,temp.area,x,y)

  eBayNeRDinfo$step.completed<-0:8 #step 8 corresponds to the best-TPV found
  save(eBayNeRDinfo,file="eBayNeRDinfo.RData") #save the BayNeRDinfo file inside the current working folder defined earlier
  load("eBayNeRDinfo.RData") #loading BayNeRDinfo file
  eBayNeRDinfo <<- eBayNeRDinfo


  cat("\n\n\n\n Done!\n Thank you for using e-BayNeRD.\n",
      "We would appreciate if you write\n",
      "an email to alexsandrocos5@gmail.com\n",
      "telling your experiences with e-BayNeRD.\n\n",
      "Best regards,\n\nAlexsandro Candido de Oliveira Silva\n\n")

  invisible(gc())
}




# #function to calculate several assessment indexes
# FUNCTION_assessment <- function(Cimg=eBayNeRDinfo$class.image,
#                                 ref=eBayNeRDinfo$ref.for.test)
#   {
#   cat("\n\n\n Reading data...\n\n");
#   eval(parse(text=(paste("ref.raster <- raster('",ref$file,"')",sep=""))))
#   eval(parse(text=(paste("Cimg.raster <- raster('",Cimg,"')",sep=""))))
#
#   #eval(parse(text=(paste("ref.raster[ref.raster==",as.numeric(ref$classes$class.target),"] <- ",
#   #                       1:length(ref$classes$class.target),";",sep=""))))
#
#   ref.classes <- c(0,as.numeric(ref$classes$class.target))
#   n.classes <- length(ref.classes)
#
#
#   cat("\n\n The sampling size is based on the Multinomial statistical distribution.\n\n")
#   ok <- F
#   while(ok==F) {
#     conf <- readline("Enter the confidence value (recommended = 0.95): ")
#     conf <- as.numeric(conf)
#     if((is.na(conf)) | (conf>1) | (conf<0)) cat("\n\nInvalid value!\n") else ok <- T
#   };rm(ok)
#
#   cat("\n\n The follow table summarises the computations:\n\n")
#
#   sample.size.computations <- FUNCTION_sample.size(alfa=(1-conf),ref.raster=ref.raster)
#   print(sample.size.computations)
#
#   ok <- F
#   while(ok==F) {
#     sample.size <- readline("\n\n Enter the desired sample size (each class): ")
#     sample.size <- as.numeric(sample.size)
#     if((is.na(sample.size)) | (sample.size>min(sample.size.computations$n.elements)) | (sample.size<1) | ((sample.size-floor(sample.size))!=0)) cat("\n\nInvalid value!\n") else ok <- T
#   };rm(ok)
#
#   n <- 5000
#   cat("\n\nComputing...\n")
#   cat(paste("0% processed at ",Sys.time()," ...\n",sep=""))
#
#   accuracy <- kappa <- NULL
#   all.Conf.Matrix <- vector('list', n)
#   for (i in 1:n) {
#     ref.aux <- raster(ref.raster)
#     for (l in 1:n.classes) {
#       trash.sample<-sample(which(getValues(ref.raster) == sample.size.computations$ID[l]),
#                            size=sample.size, replace=F)
#       ref.aux[trash.sample] <- ref.raster[trash.sample]
#       rm(trash.sample)
#     }
#     ind <- which(!is.na(getValues(ref.aux)))
#     obs <- ref.aux[ind]
#     pred <- Cimg.raster[ind]
#     all.Conf.Matrix[[i]] <- table(PRED = pred, OBS = obs)
#
#     accuracy[i] <- sum(diag(all.Conf.Matrix[[i]])) / sum(all.Conf.Matrix[[i]])
#     kappa[i] <- FUNCTION_accuracy.index(all.Conf.Matrix[[i]])[[2]]
#
#
#     if ( ((i/n)*100) %% 10 == 0 )
#     {
#       cat(paste(round( (i/n)*100, 1),"% processed at ",Sys.time()," ...\n",sep=""))
#       invisible(flush.console())
#     }
#   }
#
#   indexes <- data.frame(i = 1:n, Accuracy = as.numeric(accuracy), Kappa = as.numeric(kappa))
#
#   index.plot <- ggplot(indexes, aes(x=i)) +
#     geom_line(aes(y=Accuracy, colour="red"), lwd=.8) +
#     geom_line(aes(y=Kappa, colour="blue"), lwd=.8) +
#     geom_line(y=mean(kappa), linetype = 2) + geom_line(y=mean(accuracy), linetype = 2) +
#     annotate("text", x = 250, y = mean(kappa)+.004, label = paste("Mean =", round(mean(kappa),2)) ) +
#     annotate("text", x = 250, y = mean(accuracy)+.004, label = paste("Mean =", round(mean(accuracy),2)) ) +
#     labs(title = 'Overall Accuracy and Kappa Index', x="i-th Sampling", y="Index Value") +
#     theme(plot.title = element_text(face="bold", size=14),
#           legend.title = element_blank(),
#           axis.title.x = element_text(size=14),
#           axis.title.y = element_text(size=14))
#   ggsave("./e-BayNeRD Outcomes/Index-value.png", plot = index.plot, width = 9, height = 6)
#
#
#   cat("\n\n Overall Accuracy mean:", mean(accuracy))
#   cat("\n\n Kappa Index mean:", mean(kappa))
#
#   #cat("\n\n Average rate of Tau Index: \n")
#   #print(sum(tau)/length(tau))
#
#
#   eBayNeRDinfo$step.completed <- 0:9 #step 9 corresponds to assessment
#   eBayNeRDinfo$ConfMatrix <- all.Conf.Matrix
#   save(eBayNeRDinfo,file="eBayNeRDinfo.RData") #save the eBayNeRDinfo file inside the current working folder defined earlier
#   load("eBayNeRDinfo.RData") #loading eBayNeRDinfo file
#   eBayNeRDinfo<<-eBayNeRDinfo
#
#   cat("\n\n\n Well done!!\n\n\nThank you for unsing BayNeRD. We would appreciate if you write an email to\n  acos@dpi.inpe.br or mello@ieee.org telling us your experiences with BayNeRD.\n\nPlease, don't forget to cite the e-BayNeRD paper in your publication.\n  To see how to cite, please use the menu 'How can I cite BayNeRD'\n  or the funtion 'FUNCTION_how.to.cite.the.program()'.\n\nBest regards,\nAlexsandro Cândido de Oliveira Silva\nand Marcio Pupin Mello.\n\n\n")
#
# }



#-----------------------------------------------------------------------------#
#                             Necessary FUNCTIONS                             #
#-----------------------------------------------------------------------------#

FUNCTION_instructions <- function() {
  # Shows the webpage link with instruction
  cat("\nGo to:\nhttps://github.com/alexsandrocandido/e-BayNeRD\n\n")
}


FUNCTION_load.package <- function(name.of.package) {
  # Loads/Downloads the needed packages
  packages <- installed.packages()[, 'Package']
  if (is.element(name.of.package, packages)) {
    eval(parse(text=(
      paste("library(package='",name.of.package,"', character.only=T)",
            sep=""))))
  } else {
    install.packages(name.of.package)
    eval(parse(text=(
      paste("library(package='",name.of.package,"', character.only=T)",
            sep=""))))
  }
}


FUNCTION_working.folder <- function() {
  # Defines de working folder.
  #
  # Return:
  #   A string corresponding to the working folder.
  working.folder <- getwd()
  cat("\n The current working folder is:\n",
      working.folder, "\n\n")
  resp <- readline("Do you want to change it?\n(y-Yes ; n-No): ")
  if ((resp == "y") | (resp == "Y")) {
    cat("\n\n")
    working.folder <- readline(paste(
      "Type the new forder (eg C:/new/my_folder): "))
    setwd(working.folder)
    if (getwd() == working.folder) {
      cat("\n
          The new current working folder is:\n",
          working.folder,"\n\n")
    }
  } else {
    if ((resp != "n") && (resp != "N")) {
      stop ("\n\nInvalid option!\n\n",call.=F)
    }
    return(working.folder)
  }
  }


FUNCTION_before.beginning <- function() {
  # First function to be run.
  # It installs all the needed packages and
  # it creates the eBayNeRDinfo.RData file to save the process information.
  cat("\n Installing/Loading the needed packages... (please wait)")


  # Installing the needed packages.
  if ('RBGL' %in% installed.packages()[,'Package'] ){
    require('RBGL')
  } else {
    source("http://bioconductor.org/biocLite.R")
    biocLite('RBGL')
    require('RBGL')
  }

  #invisible(FUNCTION_load.package("rgdal"))
  invisible(FUNCTION_load.package("raster"))
  invisible(FUNCTION_load.package("bnlearn"))
  invisible(FUNCTION_load.package("deal"))
  invisible(FUNCTION_load.package("arules"))
  invisible(FUNCTION_load.package("gRain"))
  invisible(FUNCTION_load.package("parallel"))
  invisible(FUNCTION_load.package("snow"))
  invisible(FUNCTION_load.package("doSNOW"))
  invisible(FUNCTION_load.package("foreach"))
  invisible(FUNCTION_load.package("iterators"))
  invisible(FUNCTION_load.package("bigmemory"))
  #invisible(FUNCTION_load.package("ggplot2"))

  # Clearing the console.
  cat("\014")
  # Writing the heading of program.
  FUNCTION_write.the.heading.of.the.program()
  # Defining the working folder.
  temp <- FUNCTION_working.folder()

  # Checking if the e-BayNeRDinfo.RData file exists in current work folder.
  if (sum(dir() == "eBayNeRDinfo.RData") == 0) {
    eBayNeRDinfo <- list(step.completed=0:1)
    eBayNeRDinfo$menu <- FUNCTION_eBayNeRD.R.menu.names()
    eBayNeRDinfo$working.folder <- temp;rm(temp)

    # Save the eBayNeRDinfo file inside the current working folder.
    save(eBayNeRDinfo,file="eBayNeRDinfo.RData")
    # Loading eBayNeRDinfo file.
    load("eBayNeRDinfo.RData")
    eBayNeRDinfo<<-eBayNeRDinfo
  } else {  # If exist the eBayNeRDinfo.RData file in current work folder.
    cat("\n
        A previous e-BayNeRD process had been started before.\n
        Would you like to:\n
        (1) Start a new process.
            All previous work will be lost!\n
        (2) Continue the previous process.\n\n")

    ok <- F
    while (ok == F) {  # Ask about restart or continue the process.
      resp <- readline("Option: ")
      if ((resp == "1") | (resp == "2")) {
        ok <- T
      } else {
        cat("\n\nInvalid option!\n\n")
      }
    }
    rm(ok)

    # If the user chooses to continue.
    if (resp == "2") {
      cat("\n\n\nReading... (please wait)")

      load("eBayNeRDinfo.RData")  # Loading the informations.
      eBayNeRDinfo<<-eBayNeRDinfo
    } else {  # If the user chooses to restart.
      invisible(file.remove("eBayNeRDinfo.RData"))
      eBayNeRDinfo <- list(step.completed=0:1)
      eBayNeRDinfo$menu <- FUNCTION_eBayNeRD.R.menu.names()
      eBayNeRDinfo$working.folder <- temp;rm(temp)
      # Save the eBayNeRDinfo file inside the current working folder
      save(eBayNeRDinfo,file="eBayNeRDinfo.RData")
      load("eBayNeRDinfo.RData") # Loading eBayNeRDinfo file
      eBayNeRDinfo<<-eBayNeRDinfo
    }
    rm(resp)
  }

  if (Sys.info()[[1]] == 'Windows') {
    cat('\n\n === Done! ===\n\n')
    winDialog(type="ok","Now more e-BayNeRD menu options have been unlocked.")
    FUNCTION_eBayNeRD.R.menu.activation(
      menu.elements=eBayNeRDinfo$menu,exist.eBayNeRDinfo=T)
  } else {
    cat("\n\n
        Now you can continue the process...\n
        If you haven't run e-BayNeRD before in this working forder, you may
        start with the funtion 'FUNCTION_read.target.variable().
        \n\n")
  }
  # After this procedure the object "e-BayNeRDinfo"
  # is created in the main workspace of R.
  }


#-----------------------------------------------------------------------------#
#                                    MAIN                                     #
#-----------------------------------------------------------------------------#

options(scipen=10000,  # For non scientific notation of big or small numbers
        warn=-1)       # For not printing warning messages

cat("\014")
FUNCTION_write.the.heading.of.the.program();

if (Sys.info()[[1]] == "Windows") {
  FUNCTION_eBayNeRD.R.menu.activation(
    menu.elements=FUNCTION_eBayNeRD.R.menu.names(),exist.eBayNeRDinfo=F)
}
if (Sys.info()[[1]] == "Windows") {
  winDialog(type="ok",
            paste("Now you can use the new R menu e-BayNeRD\n",
                  "(enhanced Bayesian Networks for Raster Data)"))
} else {
  cat("\n\n
      The functions to run the enhenced Bayesian Network for Raster Data
      (e-BayNeRD) program have been loaded.
      They can be listed using 'ls()'.

      Please, use 'FUNCTION_how.to.cite.the.program()' to see how
      you can cite the e-BayNeRD paper. If you are new to e-BayNeRD,
      typing 'FUNCTION_instructions() you will see a webpage link
      that could help you to know what to do. Otherwise type
      'FUNCTION_before.beginning()' if you are ready to go ahead.\n\n")
}

