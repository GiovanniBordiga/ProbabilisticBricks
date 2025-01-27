# ProbabilisticBricks [![Wolfram Language](https://img.shields.io/badge/Made%20with-Wolfram%20Language-dd1100?logo=wolfram-language&labelColor=34495e)](https://www.wolfram.com/) [![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2FGiovanniBordiga%2FProbabilisticBricks&count_bg=%2327AE60&title_bg=%2334495E&icon=github.svg&icon_color=%23E7E7E7&title=Hits&edge_flat=false)](https://hits.seeyoufarm.com)

*Mathematica* Code for a Micromechanical Model of Masonry

<img align="center" src="example.gif" alt="example.gif">

## Overview
This is a project I developed for my Master's thesis in Civil Engineering at the [*University of Trento (Italy)*](http://www.dicam.unitn.it/).
The aim is to create an implementation for the micromechanical model of masonry formulated in the thesis, which is an extension of the model developed by Bigoni and Noselli (2010) in the context of photoelasticity.

The code can be used to analyze a masonry panel under a general distribution of in-plane loads, including self-weight, taking into account the unilateral Coulomb frictional contacts between the bricks.

## How to install it
The code's root folder `ProbabilisticBricks` needs to be placed inside the `Applications` folder of the `$UserBaseDirectory` of your *Mathematica* installation. This is required in order to ensure the packages' dependencies work properly without any additional settings.

So you can either
- clone the repository using Git or
- dowload the zip file of the repository, unpack it and rename the extracted folder to ProbabilisticBricks
and then move the root folder inside `$UserBaseDirectory/Applications`. You can see where this directory is located on your system just by evaluating `$UserBaseDirectory` in the *Mathematica* FrontEnd.

If you dowload the zip file you can unpack, rename and move the root folder to the correct location using the following *Mathematica* code

```Mathematica
zipName = "ProbabilisticBricks-master.zip"; (*name of the zip file*)
fromPath = FileNameJoin[{$HomeDirectory, "Desktop", zipName}]; (*current path of the zip file: be sure it's on your Desktop or replace "Desktop" with the appropriate location*)
toPath = FileNameJoin[{$UserBaseDirectory, "Applications"}]; (*destination directory*)
ExtractArchive[fromPath, toPath]; (*extract the code in the correct directory*)
RenameDirectory[
  FileNameJoin[{$UserBaseDirectory, "Applications", 
    StringDrop[zipName, -4]}], 
  FileNameJoin[{$UserBaseDirectory, "Applications", 
    "ProbabilisticBricks"}]]; (*rename the extracted directory*)
```

Now the code is ready to be used.
