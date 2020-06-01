# 3rd Party IDE Help

Author:   David Hoyle (davidghoyle@gmail.com / [https://github.com/DGH2112](https://github.com/DGH2112))

Version:  1.0a

Date:     01 Jun 2020

Web Page: [https://www.davidghoyle.co.uk/WordPress/?page_id=2051](https://www.davidghoyle.co.uk/WordPress/?page_id=2051)

## Overview

This is a RAD Studio wizard / expert / plug-in that allows you to easily configure 3rd party HTML html files for use with the IDE so that you can get context sensitive help for 3rd party code. It also provides quick access to these files.

## Compilation

This RAD Studio Open Tools API project uses a single DPR/DPROJ file pair for all version of RAD Studio from XE2 through to the current 10.3 Rio. If you need to compile the plug-in yourself just open the project in the IDE and compile and the correct IDE version suffix will be applied to the output DLL.

## Usage

To configure the 3rd party help, open the IDE's Option dialogue and look under the 3rd Party node for an options page entitled "3rd Party Help". On this page you can Add, Edit or Delete 3rd party HTML help files to be used by the IDE for context sensitive help in the editor. The HTML help files you add are then listed in a section in the IDE's Help menu.

## Current Limitations

You can only configure HTML CHM files for use with the IDE.

## Source Code and Binaries

The source code for this project can be downloaded from [Git Hub](https://github.com/DGH2112/3rd-Party-IDE-Help).

You can download binaries for this project if you don't want to compile it yourself from the web page: [3rd Party IDE Help](https://www.davidghoyle.co.uk/WordPress/?page_id=2051).
