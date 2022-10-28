
# Kingpin (in progress)

The kingpin package is a supporting pins management package produced for internal use in the PHEI team. External parties may use this package with caution that certain functionalities may not be applicable. 

The package has a few key prerequisites:
- The data team must be active R users who regularly pin items to a private RSConnect board. 
- The data team uses environmental variables stored in an .Renviron file in their local machines, specifically `CONNECT_SERVER` and `CONNECT_API_KEY`. 

The package aims to address these key objectives:
- Keep track of pin useage. (This includes frequency of reading or writing certain pins, which projects each pin is used in, and which users use each pin.)
- Encourage adding descriptions to each pin.
- A less risky way of deactivating pins using an automatic backup. Rather than deleting a pin and taking the risk of breaking dependent products, deactivate a pin by transferring it to a backup pin for 7 work days before automatic deletion. (This assumes that 7 work days is enough to determine its removal would not break existing products.)

## Installation

First, make sure you have environmental variables `CONNECT_SERVER` and `CONNECT_API_KEY` set up in an .Renviron file. Make sure the `CONNECT_SERVER` value is a URL ending in a slash. If you don't have an API key saved, generate a new API key in RSConnect by going to RStudio Connect > profile icon > API Keys > New API Key. 

Download the current version of the package by running 

`devtools::install_github("herts-phei/kingpin")`

`library(kingpin)`

## Setup 

If this is the first time you are using the kingpin package, follow the instructions below. You can check if this is the case by checking if your RSConnect board has a "kingpin" and "pin_pit" pin with editing privileges given to the most active group. "pin_pit_status" should also be setup as a deployed Rmarkdown file. Do NOT make several copies of any of these files as this will make certain functions error. 

- To set up kingpin and pin_pit, if they are missing: Run `kingpin::setupkingpin(server = Sys.getenv("CONNECT_SERVER", key = Sys.getenv("CONNECT_API_KEY"))` and check if the two pins now exist with appropriate permissions. 
- To set up pin_pit_status, if it is missing: Create a new Rmarkdown file, delete everything in it, and paste in the contents of [this file](https://github.com/herts-phei/kingpin/blob/master/inst/rmd/pin_pit_status.Rmd). Deploy this to your RSConnect board and add the two environmental variables (CONNECT_SERVER, CONNECT_API_KEY). Schedule it to run every work day at a time of your choosing. 

## Using the package

For more examples, please refer to the vignettes found in TBA**. 
For best results, **do not** load in the `pins` package. The syntax for the kingpin package is the same as the pins package. The only difference is that it encourages adding a "comment" to a pin as a description of the data you are pinning.  

**CONNECTING**<br>
Syntax is the exact same as pins package:<br>
`library(kingpin)`

`board <- board_rsconnect(server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_KEY"))`
 
**PINNING**<br>
Syntax is the exact same as pin_write():<br>
`pin_throw(board, iris, "temporary_iris", comment = "Just the iris dataset")`

You can check if this action has been recorded in a new row in `kingpin`:

`kingpin <- pin_return(board, "kingpin")`

**READING**<br>
Syntax is the exact same as pin_read():<br>
`pin_return(board, "temporary_iris")` 

You can check if this action has been recorded in a new row in `kingpin`:

`kingpin <- pin_return(board, "kingpin")`

**DEACTIVATING**<br>
For best practice, use this function instead of deleting pins on RSConnect as it reduces risk. If you are absolutely sure that you do not need a backup (e.g., test data) you can skip using this. <br>
`pin_deactivate(board, server = Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY"), name = "temporary_iris")`

You can check on the RSConnect board if this pin has been deleted or not. Then, check if the contents of this pin has been backed up:

`pin_pit <- pin_return(board, "pin_pit")`

You should see the pin you just deleted in the list. To see the contents, use indexing:

`pin_pit$temporary_iris$content` 

You can see how long it would take before the backup is deleted:

`pin_pit$temporary_iris$countdown` 
