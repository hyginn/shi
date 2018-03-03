#! /usr/bin/perl
# Logos.pl
#
# Draws generalized plots of character profiles, akin to
# SequenceLogos.
#
# precondition: input is read from STDIN
# "data" records contain tuples of values - a character (or string) and
#         its fractional height in a column of characters.
#         If Characters are given a negative fractional height, they are drawn upside down.
# "grey" or "color" (resp. "gray" or "colour") or "contour" switch
#         the display of characters between empty contours, grayvalue fill or colour fill.
#         "contour" is default.
# "portrait" or "landscape" set the page orientation. "portrait" is default.
# "title" records contain a title string that is to be displayed.
# All other parameters can easily be changed in the perl code.
#
# postcondition: Postscript output is written to STDOUT and can be viewed or converted to pdf ...
# example use: ./Logos.pl < test.data | ps2pdf - test.pdf
# history:
# -- Version 2.1 -------
#     Print motif angles as cross, ensemble stddev as ellipse
#	  Added option to show angles as line-plot.
#	  Print titles and subtitles left-flush, print centred only as option.
#     Add option to use sans-serif font (Helvetica) for letter columns.
#	  Read annotations for sequences, DSSP, Vorocodes etc. and display one character per column ...
#     Tweaked default layout parameters
#	  Add option for monochrome (red / green / blue) color schemes
#	  write a bounding box with padding to postscript file to make output a valid EPS file that should be
#     correctly cropped by DSC compliant programs
#     Changed minimum y-axis height to 0.4 (+1 tickmark) to avod legend overlap problem.
# -- Version 2.0 -------
#     Added functions to show angles in Ramachandran boxes above columns.
#     Completely refactored code, cleaned up to calculate all layout-variables scaled to UnitHeight.
#     Read and print subtitles with smaller font.
# -- Version 1.2 -------
#     Changed CMYK color model to RGB + grayvalue
#     Changed default Character font to Times-Bold
#     Display lowercase as uppercase, but keep group colors (assigned to lowercase)
#     Fill inverted characters with white
#     Print tickmarks up entire axis
#     Print unit-labels on axes
#     Calculate line-widths and font-heights relative to unit-height for easier scaling
#     Read unit height from input datafile
#     Allow multiple characters (groups) per position
#     Test if first character of string has been defined - if not, set RGBK to white
#     Handle input of multiple title lines
#     Parse input input keys case-insensitively
# -- Version 1.1 -------
#     Add record-type "Title" to input data.
#     Add option to print on landscape-paper layout
#     Add labels for columns
#     Use CMYK values to color-fill characters
#
#
# Example Input File
# ==================
#
# landscape
# colour
# unit 112
# profile e 0.05771 yf 0.08655 T 0.11533 RKH 0.28831 VILM 0.40364 
# profile q 0.06494  r 0.06496 v 0.06496   C 0.12993    S 0.19490 N 0.58472 D 1.03950
# profile 
# profile d 0.02808 f 0.028087 i 0.02808 l 0.02808 N 0.05617 Q 0.05617 T 0.05617 A 0.084261 V 0.084261 S 0.11234 E 0.14043 K 0.22469
# profile d 0.07362 l 0.073625 C 0.14725 N 0.14725 S 0.58900 T 1.39888
# profile H 0.06374 l 0.063742 q 0.06374 s 0.06374 v 0.06374 Y 0.12748 K 0.19122 N 0.191227 G 1.274853
# profile d 0.03044 H 0.030448 M 0.03044 q 0.03044 s 0.03044 t 0.03044 y 0.03044 g 0.060896 l 0.060896 A 0.09134 V 0.09134 E 0.21313 K 0.27403
# angles -103.66 -108.07   10.0  124.53 133.88   20.0
# angles  -74.88  -78.6626  5.0  125.62 119.262   9.0
# angles  -64.83  -64.8543 13.0  -39.02 -26.5341  7.0
# angles  -68.07  -79.7636  5.0  -54.94 -42.6067  8.0
# angles  -88.29 -103.881   2.0  -22.44 -16.3286  4.0
# angles   85.80   74.1958 12.0   -5.93  10.2462 17.0
# angles  -65.58  -82.5118 22.0  125.18 144.185  33.0
# title Motif 1cb8_A_673_0_7
# title Support 33 / UniqueSupport 4
# annotation MotifSequence RDLKTGK
# annotation DSSP EETTT--
# annotation Stride EETTTTC
# subtitle Unclassified
#
# in "profile" records, the strings to be printed into the stack (single characters or groups) have to be followed
# by the information in bits.
# in "angle" records the columns are phi angle of the motif, average phi 
# for the ensemble, standard deviation for ensemble phi, psi angle of the motif, average psi for the ensemble
# and standard deviation for ensemble psi.
#
#
# == Issues and ToDo ========================
#  
# ==================================================================================
# Author and Copyright (C) 2003-2005 Boris Steipe <boris.steipe@utoronto.ca>.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, write to
# the Free Software Foundation, email: gnu@gnu,org. (www.gnu.org/home.html)
# ==================================================================================

use warnings;
use strict;

# === Customizable Parameters ================================

# Plot Axis coordinates (Postscript units)
# postscript units are 72/inch (or 2.835 per mm) with
# the lower left corner of the page set as 0.0 , 0.0
# standard letter page format (8.5" x 11") is (612 x 792)


# ===== Absolute Parameters ====================
# These are defined as absolute values and can be redefined through input data

my $UnitHeight = 200;        # Height of one y-axis unit in postscript units.
							 # Font sizes, offsets and linewidths are calculated
						     # relative to this number

my $TickDivisions = 0.2;     # Subdivisions on Y-axis (bits)

my $x0 = 100;			     # (0,0) coordinates of axes on page
my $y0 = 150;

# ===== Layout Parameters =======================
# All loyout parameters are scaled relative to "UnitHeight" - 1.0 bits on the y-axis

my $ColumnWidth = 0.4;	             # Width of one column
my $ColumnPadding = 0.05;	         # Empty space between columns

my $CharacterLineWidth = 0.0015;	     # Linewidth for characters

my $AxisLineWidth = 0.005;            # Linewidth for axis
my $TickLineWidth = $AxisLineWidth / 2.0;           # Linewidth for tickmarks
my $LabelPts = 0.06;                  # Font size for axis labels
my $AngleLabelPts = 0.04;              # Font size for angle-box labels
my $LabelOffset = 0.07;               # Offset of label from axis
my $dx_Tick = $LabelOffset / 2.0;                   # Length of tickmarks

my $dy_Anglebox = $ColumnWidth * 1.0;                # Height of box around angle plots
													 # Width of box is taken as $ColumnWidth
my $AngleboxOffset = $ColumnPadding * 2.0;           # Offset of box around angle plots from top of y-axis
my $AngleboxLineWidth = $AxisLineWidth * 1.0;        # Linewidth for box around angle plots
my $AngleAxisLineWidth = $AngleboxLineWidth *0.5;    # Linewidth for axis in anglebox
my $AngleTickLineWidth = $AngleboxLineWidth * 0.25;  # Linewidth for tickmarks in anglebox
my $l_AngleTick = $ColumnWidth * 0.08;               # Length of angle tickmarks
my $AngleTickDivisions = 30;						 # Subdivisions for angle axis (degrees, should be multiple of 360)
my $AngleCrossLineWidth = $AngleboxLineWidth * 0.8;  # Linewidth for crosshair marking angles
my $AngleEllipsisLineWidth = $AngleboxLineWidth * 1.0;  # Linewidth for ellipsis for ensemble
my $l_AngleCross = $ColumnWidth * 0.25;               # Length of angle crosshairs
my $AngleMarkPts = 0.055;						# Font size for angle marks

my $xUnitsString = "Position";                       # Legend for x-axis
my $yUnitsString = "Information (bits)";             # Legend for y-axis
my $UnitsPts = 0.07;		             # Font size for legends
my $UnitsOffset = 0.18;		         # Offset of legend from axis

my $AngleUnitsPts = 0.06;		             # Font size for angle legends
my $AngleUnitsOffset = 0.12;		         # Offset of angle legend from frame

my $AnnotationPts = 0.10;		             # Font size for annotation characters
my $AnnotationLabelPts = 0.06;		         # Font size for annotation labels
my $AnnotationOffset = 0.04;		         # Spacing above and below annotations

my $TitlePts = 0.08;		             # Point size for Title string
my $SubtitlePts = 0.06;		         # Point size for Subtitle string
my $TitleOffset = 0.12;		         # Offset of Title textblock above y-axis or angle boxes


# === Default flags ===========================================

my %Flags = ( );
   $Flags{"orientation"} = "portrait";  # portrait or landscape
   $Flags{"fill"} = "color";            # "contour": no fill - "grey": grey-values fill - "color": RGB color fill,
										# "monochrome-r": only red shades, similar monochrome-g, monochrome-b
   $Flags{"font"} = "TimesB";           # Serif-font, can be set to sans-serif "HelvB" via datafile command "sans"
   $Flags{"TitleAlign"} = "left";       # Left-flush titles and subtitles - can be changed to "centred" via datafile command "centred"
   $Flags{"AnglePlot"} = "boxes";       # boxes: plot (phi,psi) angles in Ramachandran boxes; lines: plot in line plot"

# === Global processing parameters ============================

my $nColumns;                           # Number of columns for positions
my $nAngles;                            # Counter for angles
my $nAnnotations;                       # Counter for annotations
my $dx_Axis;						    # length of x-Axis
my $dy_Axis;						    # length of y-Axis
my $MaxStackHeight = 0.4;				# highest observed stack of letters in a column

my $xi_Bbox;							# EPS format BoundingBox
my $yi_Bbox;
my $xa_Bbox;
my $ya_Bbox;

# === Data structures =========================================
my @Comments = ([]);
my @CharacterNumbers = ([]);         # Number of characters in each column
my @CharacterLetters = ([([])]);     # Array of characters
my @CharacterHeights = ([([])]);     # Array of heights for each character (in postscript units)
my %CharacterRGBK = ( );             # Hash of arrays with RGBK values for each character
my @Angles = ([]);                   # phi and psi angles for motif and ensemble
my @TitleStrings = ();	             # Array of titlestrings
my @TitleStringPts = ();	         # Array of titlestring fontsizes
my @AnnotationStrings = ();	         # Array of strings for annnotations
my @AnnotationLabels = ();	         # Array of labels for annnotations

# =============================================================

InitializeCharacterRGBK( );

ReadData( );
          
# scale all layout parameters
		  
$ColumnWidth *= $UnitHeight;
$ColumnPadding *= $UnitHeight;
$CharacterLineWidth *= $UnitHeight;
$AxisLineWidth *= $UnitHeight;
$TickLineWidth *= $UnitHeight;
$LabelPts *= $UnitHeight;
$AngleLabelPts *= $UnitHeight;
$LabelOffset *= $UnitHeight;
$dx_Tick *= $UnitHeight;
$dy_Anglebox *= $UnitHeight;
$AngleboxOffset *= $UnitHeight;
$AngleboxLineWidth *= $UnitHeight;
$AngleAxisLineWidth *= $UnitHeight;
$AngleTickLineWidth *= $UnitHeight;
$AngleEllipsisLineWidth *= $UnitHeight;
$l_AngleTick *= $UnitHeight;
$AngleCrossLineWidth *= $UnitHeight;
$AngleMarkPts *= $UnitHeight;
$l_AngleCross *= $UnitHeight;
$UnitsPts *= $UnitHeight;
$UnitsOffset *= $UnitHeight;
$AngleUnitsPts *= $UnitHeight;
$AngleUnitsOffset *= $UnitHeight;
$AnnotationPts *= $UnitHeight;
$AnnotationLabelPts *= $UnitHeight;
$AnnotationOffset *= $UnitHeight;
$TitleOffset *= $UnitHeight;
$MaxStackHeight *= $UnitHeight;

WritePSHeader();                              # Header and definitions

WritePageSetup();                             # Portrait or Landscape

WriteLetters( );

WriteAxes( );

if ($nAnnotations > 0) { 
	WriteAnnotations();
}

if ($nAngles > 0) { 
	if ($Flags{"AnglePlot"} eq "lines") {
	    WriteAngleLines();
	} else {
	    WriteAngleBoxes();
	}
}
	
WriteTitle();

WritePSFooter();

exit;


##############################################
#     S U B R O U T I N E S                  #
##############################################

##############################################
# InitializeCharacterRGBK
##############################################
# Initialize colours/greyvalues for each character.
# Postscript: 1.0 is white, 0.0 black.
# Array values are Red Green Blue and Grayvalue

sub InitializeCharacterRGBK {

# default - white
    $CharacterRGBK{ ' ' } = [1.0, 1.0, 1.0, 1.0];		

# upper case: individual amino acids
# small:               A, G,      = light-blue
    $CharacterRGBK{ 'A' } = [0.70, 0.70, 0.98, 0.45];		
    $CharacterRGBK{ 'G' } = [0.70, 0.70, 0.98, 0.45];
     
# polar:               S, T       = light mauve
    $CharacterRGBK{ 'S' } = [0.87, 0.70, 0.99, 0.55];
    $CharacterRGBK{ 'T' } = [0.87, 0.70, 0.99, 0.55];
# basic:               K, R, H    = blue
    $CharacterRGBK{ 'K' } = [0.25, 0.43, 0.95, 0.80];
    $CharacterRGBK{ 'R' } = [0.25, 0.43, 0.95, 0.80];
    $CharacterRGBK{ 'H' } = [0.25, 0.43, 0.95, 0.80];
# hydrophilic:         N, Q       = purple
    $CharacterRGBK{ 'N' } = [0.51, 0.00, 0.73, 0.70];
    $CharacterRGBK{ 'Q' } = [0.51, 0.00, 0.73, 0.70];
# D/N E/Q ambiguity    B, Z       = red purple
    $CharacterRGBK{ 'B' } = [0.89, 0.00, 0.61, 0.75];
    $CharacterRGBK{ 'Z' } = [0.89, 0.00, 0.61, 0.75];
# acidic:              D, E       = red
    $CharacterRGBK{ 'D' } = [0.87, 0.00, 0.14, 0.60];
    $CharacterRGBK{ 'E' } = [0.87, 0.00, 0.14, 0.60];
# hydrophobic:         V, I, L, M = yellow-green
    $CharacterRGBK{ 'V' } = [0.67, 0.98, 0.00, 0.45];
    $CharacterRGBK{ 'I' } = [0.67, 0.98, 0.00, 0.45];
    $CharacterRGBK{ 'L' } = [0.67, 0.98, 0.00, 0.45];
    $CharacterRGBK{ 'M' } = [0.67, 0.98, 0.00, 0.45];
# aromatic hydrophobic F, Y, W    = dark green
    $CharacterRGBK{ 'F' } = [0.00, 0.77, 0.25, 0.20];
    $CharacterRGBK{ 'Y' } = [0.00, 0.77, 0.25, 0.20];
    $CharacterRGBK{ 'W' } = [0.00, 0.77, 0.25, 0.20];
# special:             P          = yellowish grey
    $CharacterRGBK{ 'P' } = [0.89, 1.00, 0.75, 0.30];
# cys:                 C          = yellow
    $CharacterRGBK{ 'C' } = [1.00, 0.90, 0.05, 0.10];
# unknown:             X, O, J, U  = white
    $CharacterRGBK{ 'X' } = [1.00, 1.00, 1.00, 0.00];
    $CharacterRGBK{ 'O' } = [1.00, 1.00, 1.00, 0.00];
    $CharacterRGBK{ 'J' } = [1.00, 1.00, 1.00, 0.00];
    $CharacterRGBK{ 'U' } = [1.00, 1.00, 1.00, 0.00];

# lower case: groups
# small:               A, G,      = light-blue
    $CharacterRGBK{ 'a' } = [0.70, 0.70, 0.98, 0.45];		
    $CharacterRGBK{ 'g' } = [0.70, 0.70, 0.98, 0.45];
     
# polar:               S, T       = light mauve
    $CharacterRGBK{ 's' } = [0.87, 0.70, 0.99, 0.55];
    $CharacterRGBK{ 't' } = [0.87, 0.70, 0.99, 0.55];
# basic:               K, R, H    = blue
    $CharacterRGBK{ 'k' } = [0.25, 0.43, 0.95, 0.80];
    $CharacterRGBK{ 'r' } = [0.25, 0.43, 0.95, 0.80];
    $CharacterRGBK{ 'h' } = [0.25, 0.43, 0.95, 0.80];
# hydrophilic:         N, Q       = purple
    $CharacterRGBK{ 'n' } = [0.51, 0.00, 0.73, 0.70];
    $CharacterRGBK{ 'q' } = [0.51, 0.00, 0.73, 0.70];
# D/N E/Q ambiguity    B, Z       = red purple
    $CharacterRGBK{ 'b' } = [0.89, 0.00, 0.61, 0.75];
    $CharacterRGBK{ 'z' } = [0.89, 0.00, 0.61, 0.75];
# acidic:              D, E       = red
    $CharacterRGBK{ 'd' } = [0.87, 0.00, 0.14, 0.60];
    $CharacterRGBK{ 'e' } = [0.87, 0.00, 0.14, 0.60];
# hydrophobic:         V, I, L, M = yellow-green
    $CharacterRGBK{ 'v' } = [0.67, 0.98, 0.00, 0.45];
    $CharacterRGBK{ 'i' } = [0.67, 0.98, 0.00, 0.45];
    $CharacterRGBK{ 'l' } = [0.67, 0.98, 0.00, 0.45];
    $CharacterRGBK{ 'm' } = [0.67, 0.98, 0.00, 0.45];
# aromatic hydrophobic F, Y, W    = dark green
    $CharacterRGBK{ 'f' } = [0.00, 0.77, 0.25, 0.20];
    $CharacterRGBK{ 'y' } = [0.00, 0.77, 0.25, 0.20];
    $CharacterRGBK{ 'w' } = [0.00, 0.77, 0.25, 0.20];
# special:             P          = yellowish grey
    $CharacterRGBK{ 'p' } = [0.89, 1.00, 0.75, 0.30];
# cys:                 C          = yellow
    $CharacterRGBK{ 'c' } = [1.00, 0.90, 0.05, 0.10];
# unknown:             X, O, J, U  = white
    $CharacterRGBK{ 'x' } = [1.00, 1.00, 1.00, 0.00];
    $CharacterRGBK{ 'o' } = [1.00, 1.00, 1.00, 0.00];
    $CharacterRGBK{ 'j' } = [1.00, 1.00, 1.00, 0.00];
    $CharacterRGBK{ 'u' } = [1.00, 1.00, 1.00, 0.00];

   return;
}


##############################################
# ReadData
##############################################
# Read string data. One column of characters per line.
# Data lines must be prefixed by "data" string, followed
# by pairs of (character, fractional height)

sub ReadData {

   my @Current = ();
   my $nTitleLines = 0;

   $nColumns      = 0;	# counters
   $nAngles       = 0;   
   $nAnnotations  = 0;   
   
   foreach my $line(<STDIN>) {
      # discard blank lines
      if ($line =~ /^\s*$/) {
        next;

      } elsif ($line =~ /^\s*#/) {                 # its a comment line
		   ; #  ignore
      } else {
        $line =~ s/^\s+//;                         # remove leading whitespace
        @Current = split(/\s+/, $line);            # split at whitespace
        
        $Current[0] =~ tr/A-Z/a-z/;		           # change token to lowercase
        if ($Current[0] eq "profile") {            # parse letters / bits for position
           my $num = scalar(@Current);             # get number of tokens in array

           $CharacterNumbers[$nColumns] =  ($num - 1) / 2;  # number of characters is ($num - first token) / 2
           for (my $i = 1; $i < $num; $i += 2) {

              $CharacterLetters[$nColumns][$i/2] =  $Current[$i];           # assume token is character
              $CharacterHeights[$nColumns][$i/2] =  $Current[$i + 1];       # assume following token is height
           }
           
        $nColumns++;                            # increment counter for positions

        } elsif ($Current[0] eq "angles") { # parse angle values for position

              $Angles[$nAngles][0] = $Current[1];  # read phi for motif
              $Angles[$nAngles][1] = $Current[2];  # read phi for ensemble
              $Angles[$nAngles][2] = $Current[3];  # read stdev for phi for ensemble
              $Angles[$nAngles][3] = $Current[4];  # read psi for motif
              $Angles[$nAngles][4] = $Current[5];  # read psi for ensemble
              $Angles[$nAngles][5] = $Current[6];  # read stddev for psi for ensemble

        $nAngles++;                            # increment counter for angles
		 
        } elsif ($Current[0] eq "annotation") { # parse angle values for position

              $AnnotationLabels[$nAnnotations]  = $Current[1];  # label for annotation
			  $AnnotationStrings[$nAnnotations] = $Current[2];  # annotation string

        $nAnnotations++;                            # increment counter for annotations
		 
		} elsif ($Current[0] eq "portrait")  {
            $Flags{ "orientation" } = "portrait";

        } elsif ($Current[0] eq "landscape")  {
            $Flags{ "orientation" } = "landscape";

        } elsif ($Current[0] eq "contour")  {
            $Flags{ "fill" } = "contour";

        } elsif ($Current[0] eq "monochrome-r")  {
            $Flags{ "fill" } = "monochromeR";

        } elsif ($Current[0] eq "monochrome-g")  {
            $Flags{ "fill" } = "monochromeG";

        } elsif ($Current[0] eq "monochrome-b")  {
            $Flags{ "fill" } = "monochromeB";

        } elsif ($Current[0] eq "grey" || $Current[0] eq "gray")  {
            $Flags{ "fill" } = "grey";

        } elsif ($Current[0] eq "color" || $Current[0] eq "colour")  {
            $Flags{ "fill" } = "color";

        } elsif ($Current[0] eq "sans")  {
            $Flags{ "font" } = "HelvB";

        } elsif ($Current[0] eq "lines")  {
            $Flags{ "AnglePlot" } = "lines";

        } elsif ($Current[0] eq "centred")  {
            $Flags{ "TitleAlign" } = "centred";

        } elsif ($Current[0] eq "unit")  {
            $UnitHeight = $Current[1];

        } elsif ($Current[0] eq "title" || $Current[0] eq "subtitle")  {
            $TitleStrings[$nTitleLines] = "";
            my $num = scalar(@Current);                               # get number of tokens in array
            for (my $i = 1; $i < $num; $i++) {
                $TitleStrings[$nTitleLines] .=  $Current[$i];         # concatenate a word
                $TitleStrings[$nTitleLines] .=  " ";                  # concatenate a space
            }
			
			if ($Current[0] eq "title") {
				$TitleStringPts[$nTitleLines] = $TitlePts;
			} else {
				$TitleStringPts[$nTitleLines] = $SubtitlePts;
			}
			
            $nTitleLines++;
        } else {
			;  # ignore line
        }
      }
   }

   for (my $Col = 0; $Col < $nColumns; $Col++) {            # Scale all character heights
		for (my $Item = 0; $Item < $CharacterNumbers[$Col]; $Item++) {
			$CharacterHeights[$Col][$Item] *= $UnitHeight;
		}
	}
    return;
}

##############################################
# WritePSHeader
##############################################
# Write a Postscript Header for EPS

sub WritePSHeader {

   printf "%%!PS-Adobe-3.0 EPSF-3.0\n";
   printf "%% PostScript Plot File Header\n";
   printf "%% (C) Boris Steipe 1992 - 2005\n";
   printf "%% Variables:\n";
   printf "%% Procedures:\n";
   printf "/p {/points exch def} def\n";
   printf "/cent { \n";
   printf "   moveto dup stringwidth pop\n";
   printf "   2.0 div neg  \n";
   printf "   points 5.0 mul 16.0 div neg \n";
   printf "   rmoveto \n";
   printf "   } def\n";
   printf "/box { %%draw a box using XMIN,XMAX,YMIN,YMAX\n";
   printf "   4 copy pop exch pop m\n";
   printf "   4 copy pop l pop\n";
   printf "   4 copy exch pop l pop\n";
   printf "   4 copy exch pop exch pop l\n";
   printf "   pop exch pop l s\n";
   printf "   } def\n";
   printf "/leavepage { %%invoke nulldevice\n";
   printf "   nulldevice\n";
   printf "   0 0 moveto \n";
   printf "  } def\n";
   printf "/vert { %%vertical print\n";
   printf "   gsave currentpoint translate 90 rotate\n";
   printf "   } def\n";
   printf "/unvert {grestore} def \n";
   printf "/dostring { %% dummy routine: dostring \n";
   printf "    %% must be defined to execute all necessary show/stroke commands that\n";
   printf "    %% are to be displayed for each separate string\n";
   printf "   } def\n";
   printf "/scent { %% moveto centering offset using\n";
   printf "         %% the dostring procedure\n";
   printf "   moveto gsave \n";
   printf "   leavepage\n";
   printf "   dostring \n";
   printf "   currentpoint pop 2.0 div neg \n";
   printf "%%%%                  cave:  points was left as the last def !\n";
   printf "   points 5.0 mul 16.0 div neg \n";
   printf "   grestore rmoveto\n";
   printf "   dostring\n";
   printf "   } def\n";
   printf "/vcent { %%vertical string-centering\n";
   printf "   gsave translate 90 rotate\n";
   printf "   0 0 scent\n";
   printf "   grestore\n";
   printf "   } def\n";
   printf "/supers {%% reduced size superscript\n";
   printf "   0.0 points 7.0 mul 16.0 div rmoveto\n";
   printf "   currentfont 7.0 8.0 div scalefont setfont\n";
   printf "   show\n";
   printf "   0.0 points 7.0 mul 16.0 div neg rmoveto\n";
   printf "   currentfont 8.0 7.0 div scalefont setfont\n";
   printf "   } def\n";
   printf "/subs {%% reduced size subscript\n";
   printf "   0.0 points 5.0 mul 16.0 div neg rmoveto\n";
   printf "   currentfont 6.0 8.0 div scalefont setfont\n";
   printf "   show\n";
   printf "   0.0 points 5.0 mul 16.0 div rmoveto\n";
   printf "   currentfont 8.0 6.0 div scalefont setfont\n";
   printf "   } def\n";
   printf "/cent { \n";
   printf "   moveto dup stringwidth pop\n";
   printf "   2.0 div neg  \n";
   printf "   points 5.0 mul 16.0 div neg \n";
   printf "   rmoveto \n";
   printf "   } def\n";
   printf "/right { \n";
   printf "   moveto dup stringwidth pop neg 0.0\n";
   printf "   rmoveto \n";
   printf "   } def\n";
   printf "/Times {\n";
   printf "   /Times-Roman findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/TimesB {\n";
   printf "   /Times-Bold findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/TimesI {\n";
   printf "   /Times-Italic findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/TimesBI {\n";
   printf "   /Times-BoldItalic findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/Helv {\n";
   printf "   /Helvetica findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/HelvI {\n";
   printf "   /Helvetica-Oblique findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/HelvB {\n";
   printf "   /Helvetica-Bold findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/HelvBI {\n";
   printf "   /Helvetica-BoldOblique findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/Cour {\n";
   printf "   /Courier findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/CourI {\n";
   printf "   /Courier-Oblique findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/CourB {\n";
   printf "   /Courier-Bold findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/CourBI {\n";
   printf "   /Courier-BoldOblique findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/Symb {\n";
   printf "   /Symbol findfont\n";
   printf "   points scalefont setfont\n";
   printf "  } def\n";
   printf "/m {moveto} bind def\n";
   printf "/l {lineto} bind def\n";
   printf "/s {stroke} bind def\n";
   printf "/sli {setlinewidth} bind def\n";
   printf "%% end prolog\n";
   
   return;
}
##############################################
# WritePageSetup
##############################################
# Orient for portrait or landscape

sub WritePageSetup {
    
    if ($Flags{"orientation"} eq "landscape") {
    
        printf "0 792 translate -90.0 rotate\n";    
    }
    
    return;
}

##############################################
# WriteLetters
##############################################
# Write stacks of characters

sub WriteLetters {
       
    my $DoFlip;
    my $TargetX;
    my $TargetY;
    my $Character;
    my $CharHeight;
    my $SumHeights;
    my $CharacterBasePoints = 24.0;
    my @LocalRGBK = ();
    
    printf STDOUT "%% Begin Writing Character Columns ============================= \n";
   
    printf "%5.3f p %s\n", $CharacterBasePoints, $Flags{"font"};
    printf "%5.3f sli\n", $CharacterLineWidth;


    for (my $Col = 0; $Col < $nColumns; $Col ++) {
   
        $SumHeights = 0.0;

        for (my $Item = 0; $Item < $CharacterNumbers[$Col]; $Item ++) {

            printf STDOUT "%% ===================================================== \n";
            $Character = $CharacterLetters[$Col][$Item];
            $CharHeight = $CharacterHeights[$Col][$Item];
            
            if ( !defined(substr($Character, 0, 1)) ) {
               @LocalRGBK = @{$CharacterRGBK{" "}};  # strings starting with unknown characters get default color scheme
            } else {
               @LocalRGBK = @{$CharacterRGBK{substr($Character, 0, 1)}};  # strings gets color scheme of first character
            }
            
            if ($CharHeight < 0.0) {
                $CharHeight *= -1.0;
                $DoFlip = "TRUE";
            } else {
                $DoFlip = "FALSE";
            }
            
            $TargetX = $x0 + $ColumnPadding + ($Col * $ColumnPadding) + ($Col * $ColumnWidth);
            $TargetY = $y0 + $SumHeights;
            $SumHeights += $CharHeight;
            if ($SumHeights > $MaxStackHeight) { $MaxStackHeight = $SumHeights};
        
            printf "gsave\n";

            if ($DoFlip eq "TRUE") {		# inverted character: fill with white
                printf "1.0 setgray\n";    
                
            } elsif ($Flags{"fill"} eq "grey") {		# determine fill-color
                printf "%5.3f setgray\n", $LocalRGBK[3];    
        
            } elsif ($Flags{"fill"} eq "monochromeR") {		
                printf "%5.3f %5.3f %5.3f setrgbcolor\n",
                        $LocalRGBK[0]*(1-($LocalRGBK[1]+$LocalRGBK[2])*0.5)+($LocalRGBK[1]+$LocalRGBK[2])*0.5,
                        $LocalRGBK[0]*($LocalRGBK[1]+$LocalRGBK[2])*0.5,
                        $LocalRGBK[0]*($LocalRGBK[1]+$LocalRGBK[2])*0.5;    
        
            } elsif ($Flags{"fill"} eq "monochromeG") {		
                printf "%5.3f %5.3f %5.3f setrgbcolor\n",
                        $LocalRGBK[1]*($LocalRGBK[0]+$LocalRGBK[2])*0.5,
                        $LocalRGBK[1]*(1-($LocalRGBK[0]+$LocalRGBK[2])*0.5)+($LocalRGBK[0]+$LocalRGBK[2])*0.5,
                        $LocalRGBK[1]*($LocalRGBK[0]+$LocalRGBK[2])*0.5;    
        
            } elsif ($Flags{"fill"} eq "monochromeB") {		
                printf "%5.3f %5.3f %5.3f setrgbcolor\n",
                        $LocalRGBK[2]*($LocalRGBK[0]+$LocalRGBK[1])*0.5,
                        $LocalRGBK[2]*($LocalRGBK[0]+$LocalRGBK[1])*0.5,    
                        $LocalRGBK[2]*(1-($LocalRGBK[0]+$LocalRGBK[1])*0.5)+($LocalRGBK[0]+$LocalRGBK[1])*0.5;
        
            } elsif ($Flags{"fill"} eq "color") {
                printf "%5.3f %5.3f %5.3f setrgbcolor\n",
                        $LocalRGBK[0],
                        $LocalRGBK[1],
                        $LocalRGBK[2];    
            
            } else {   # option "contour" is default
                printf "1.0 setgray\n";
            }

            printf "%f %f m\n", $TargetX, $TargetY;
            printf "currentpoint translate\n";
            printf "newpath\n";
            printf "0.0 0.0 m\n";                           # figure out dx and dy of the pathbox. The lower left is NOT
                                                            # guaranteed to be the same as the last moveto point,
                                                            # each font will have a slight offset. 
            # scale on the character's own width and height
            printf "(%s) false charpath flattenpath\n", $Character;
            printf "pathbbox\n";                            # stack: ... xl yl xu yu        
            printf "4 copy pop exch pop exch sub\n";        # stack: ... xl yl xu yu dx
            printf "5 copy pop exch pop exch sub exch pop\n";#stack: ... xl yl xu yu dx dy
            printf "%f exch div\n", $CharHeight;            # stack: ... xl yl xu yu dx sy
            printf "exch %f exch div exch\n", $ColumnWidth; # stack: ... xl yl xu yu sx sy
            printf "scale\n";                               # stack: ... xl yl xu yu
            printf "pop pop pop pop\n";                     # stack: ...


            if ($DoFlip eq "TRUE") {
                printf "180.0 rotate\n";
                printf "newpath\n";
                printf "0.0 0.0 m\n";          
                printf "(%s) false charpath flattenpath\n", $Character;
                printf "pathbbox\n";                            # stack: ... xl yl xu yu        
                printf "4 copy pop exch pop exch sub\n";        # stack: ... xl yl xu yu dx
                printf "5 copy pop exch pop exch sub exch pop\n";#stack: ... xl yl xu yu dx dy
                printf "neg exch neg exch\n";                   # stack: ... xl yl xu yu -dx -dy        
                printf "6 copy pop pop\n";                      # stack: ... xl yl xu yu -dx -dy xl yl xu yu        
                printf "pop pop 0.0 exch sub \n";               # stack: ... xl yl xu yu -dx -dy xl (0-yl) 
                printf "exch 0.0 exch sub exch \n";             # stack: ... xl yl xu yu -dx -dy (0-xl) (0-yl) 
                printf "4 copy\n";

                printf "newpath\n";
                printf "moveto rmoveto\n";                      # use copy of four off stack
                printf "(%s) true charpath  flattenpath\n", $Character;     # fill with color defined above
                printf "fill \n";

                printf "newpath\n";
                printf "moveto rmoveto\n";                      # use last four off stack
                printf "0.0 setgray\n";
                printf "(%s) false charpath  flattenpath\n", $Character;    # stroke with black
                printf "stroke \n";

                printf "pop pop pop pop\n";                     # clear remaining off stack: ...

            } else {
                printf "newpath\n";
                printf "0 0 m\n";          
                printf "(%s) false charpath  flattenpath\n", $Character;
                printf "pathbbox\n";                            # stack: ... xl yl xu yu        
                printf "pop pop 0.0 exch sub \n";               # stack: ... xl (0-yl) 
                printf "exch 0.0 exch sub exch \n";             # stack: ... (0-xl) (0-yl) 

                printf "2 copy\n";

                printf "newpath\n";
                printf "m\n";                                   # use copy of two off stack
                printf "(%s) true charpath  flattenpath\n", $Character;     # fill with color defined above
                printf "fill \n";

                printf "newpath\n";
                printf "m\n";                                   # use last two off stack
                printf "0.0 setgray\n";
                printf "(%s) false charpath  flattenpath\n", $Character;    # stroke with black
                printf "stroke \n";
            }
            printf "grestore\n";
           #Debug: print crosshairs to lower left corner of each character
           #printf "newpath %9.5f %9.5f m %9.5f %9.5f l %9.5f %9.5f m %9.5f %9.5f l \n",
           #        $TargetX-5, $TargetY, $TargetX+5, $TargetY, $TargetX, $TargetY-5, $TargetX, $TargetY+5 ;
           #printf "stroke\n";
        }
    }
    printf STDOUT "%% End Writing Character Columns ============================= \n";

    return;

}

##############################################
# WriteAxes
##############################################
# Write the axes, tickmarks, labels for the Logo

sub WriteAxes {
      
	my $Label = "";
	
	$dx_Axis = $nColumns * $ColumnWidth + ($nColumns + 2 ) * $ColumnPadding;

	# determine coordinates of bounding box
	$xi_Bbox = $x0 - $UnitsOffset - 2.0*$UnitsPts - $ColumnPadding;	# lower x for BoundingBox
	$yi_Bbox = $y0 - $UnitsOffset - 1.5*$UnitsPts - $ColumnPadding;		# lower y for BoundingBox
	$xa_Bbox = $x0 + $dx_Axis + $ColumnPadding;                 	# upper x for BoundingBox
	# $ya_Bbox is determined in subroutine WriteTitle

	# estimate longest stringlength to see if $xa_Bbox has to be adjusted.
	my $k = 0;
    for (my $i=0; $i < scalar(@TitleStrings); $i++) {
		my $l = length($TitleStrings[$i]) * $TitleStringPts[$i] * $UnitHeight * 0.5;	# estimate 0.5 * fontsize * length
		if ($l > $k) { $k = $l; }	# remember maximum
	}
	$k += $x0 + $ColumnPadding;
	if ($k > $xa_Bbox) { $xa_Bbox = $k; }
	
	my $dy_Tick = $TickDivisions * $UnitHeight;		# Distance between tickmarks on Y-axis 
	my $nTicks = int( $MaxStackHeight / $dy_Tick ) + 1;  # number of tickmarks: one above highest column

	$dy_Axis = $nTicks * $dy_Tick;                       # set $dy_Axis to meet highest tickmark

    printf "%% Begin Axes \n";
    
    printf "0.0 setgray\n";              # set graylevel to black
    printf "%f sli\n", $AxisLineWidth; 
    
    printf "newpath\n";                  # X-Axis line
    printf "%f %f m %f %f l\n", $x0, $y0, $x0 + $dx_Axis, $y0; 
    printf "stroke\n";
    
    printf "newpath\n";                  # Y-Axis line
    printf "%f %f m %f %f l\n", $x0, $y0, $x0, $y0 + $dy_Axis; 
    printf "stroke\n";
    
    printf "%f sli\n", $TickLineWidth;
    printf "%5.3f p Helv\n", $LabelPts;  # Set labelfont to Helvetica
    
    for (my $i = 0; $i <= $nTicks; $i++) {   # Y-Axis value labels and tickmarks
        printf "newpath\n";      # tickmarks
        printf "%f %f m\n", $x0,      $y0+($i*$dy_Tick);
        printf "%f %f l\n", $x0-$dx_Tick,  $y0+($i*$dy_Tick);
        printf "stroke\n";
        $Label = sprintf("%3.1f", $i * $TickDivisions);   # Label
        
        HR_V4_String($x0 - $LabelOffset, $y0+($i*$dy_Tick), $Label);

    }
    
    for (my $i = 0; $i < $nColumns; $i++) {   # x-Axis value labels: center-aligned.
    
        my $xic = $x0 + $ColumnPadding + $i*($ColumnWidth + $ColumnPadding) + 0.5*$ColumnWidth;

        printf "newpath\n";      # tickmarks
        printf "%f %f m\n", $xic,      $y0;
        printf "%f %f l\n", $xic,  $y0 - $dx_Tick;
        printf "stroke\n";
        $Label = sprintf("%d", $i+1);   # Label

        HC_VB_String($xic, $y0 - $LabelOffset - $LabelPts, $Label);

    }

    printf "%% End Axes\n";

    printf "%% Begin Unit-Labels \n";
    # === X Axis Label ======================
    printf "%5.3f p Helv\n", $UnitsPts;
    HC_VB_String($x0 + (0.5 * $dx_Axis), $y0 - $UnitsOffset - $UnitsPts, $xUnitsString);

    # === Y Axis Label ======================
    printf "%5.3f p Helv\n", $UnitsPts;
    printf "gsave\n";
    printf "%f %f translate 90.0 rotate\n", $x0 - $UnitsOffset - $UnitsPts, $y0 + (0.5 * $dy_Axis);
    HC_VB_String(0, 0, $yUnitsString);
    printf "grestore\n";

    printf "%% End Unit-Labels \n";


    return;
}

##############################################
# Write Annotations
##############################################
# Write annotation strings for each column

sub WriteAnnotations {
    
    printf "%% Begin Annotations\n";

	$dy_Axis += $AnnotationOffset;	# double offset from axis for first entry
	
	for (my $i=0; $i < $nAnnotations; $i++) {
		my $xi;
		my $xa;
		my $yi = $y0 + $dy_Axis + $AnnotationOffset;
		my $ya = $yi + $AnnotationPts;

		# write annotation label right flush
		printf "%5.3f p Helv\n", $AnnotationLabelPts;
		printf "gsave\n";      
		printf "%f %f translate\n", $x0, $yi;
		printf "newpath\n";     
		printf "0 0 m\n";    
		printf "(%s) false charpath flattenpath pathbbox\n", $AnnotationLabels[$i];   # stack: ... xl yl xu yu
		printf "pop exch pop sub 0\n";                 # stack: ... -dx 0
		printf "m\n";                                       # -dx 0 moveto
		printf "(%s) show \n", $AnnotationLabels[$i];
		printf "grestore\n";

		printf "%5.3f p Cour\n", $AnnotationPts;
        my @Chars = split(//, $AnnotationStrings[$i]);      # split at whitespace
		for (my $j=0; $j < scalar(@Chars); $j++) {

			my $xc = $x0 + $j*($ColumnPadding + $ColumnWidth) + $ColumnPadding + 0.5*$ColumnWidth;
			HC_VB_String($xc, $yi, $Chars[$j]);
		}

	$dy_Axis = $ya - $y0;	# Adjust $dy_Axis
	}
	
    printf "%% End Annotations \n";
	
    return;
}


##############################################
# Write AngleLines
##############################################
# Write the phi and psi angle data - line-plot style

sub WriteAngleLines {
    
    my $xi = $x0 + $ColumnPadding;
    my $xa = $x0 + $nAngles * ($ColumnWidth + $ColumnPadding);
    my $yi = $y0 + $dy_Axis + $AngleboxOffset;
    my $ya = $yi + $dy_Anglebox;
	
	my $x;
	my $phi_ensemble;
	my $psi_ensemble;
	my $phi_motif;
	my $psi_motif;
	my $phi_stdev;
	my $psi_stdev;

    printf "%% Begin Angles - line style \n";

	# write frame
    printf "0.0 setgray\n";              # set graylevel to black

    printf "%f sli\n", $AngleboxLineWidth;
    printf "newpath\n";                  # Frame
    printf "%f %f m %f %f l %f %f l %f %f l %f %f l\n", $xi, $yi, $xi, $ya, $xa, $ya, $xa, $yi, $xi, $yi; 
    printf "stroke\n";
	
    printf "%f sli\n", $AngleAxisLineWidth;
    printf "newpath\n";                  # Frame center axis
    printf "%f %f m %f %f l\n", $xi, $yi + 0.5*($ya - $yi), $xa, $yi + 0.5*($ya - $yi); 
    printf "stroke\n";
	
	my $nTicks = int (360 / $AngleTickDivisions);
	my $dy_T = ($ya - $yi) / $nTicks; 
	
    printf "%f sli\n", $AngleTickLineWidth;
    printf "newpath\n";                  # Angle frame thin lines
	for (my $i = 1; $i < $nTicks; $i++) {
		printf "%f %f m %f %f l\n", $xi, $yi+$i*$dy_T, $xa, $yi+$i*$dy_T;
	}
    printf "stroke\n";

    printf "%f sli\n", $AngleTickLineWidth;
    
	printf "newpath\n";      # tickmarks
	printf "%f %f m %f %f l\n", $xi, $yi, $xi-$dx_Tick, $yi;
	printf "%f %f m %f %f l\n", $xi, $yi+0.5*($ya-$yi), $xi-$dx_Tick, $yi+0.5*($ya-$yi);
	printf "%f %f m %f %f l\n", $xi, $ya, $xi-$dx_Tick, $ya;
	printf "stroke\n";
        
    printf "%5.3f p Helv\n", $AngleLabelPts;  # Set labelfont to Helvetica
	HR_V4_String($xi - $LabelOffset, $ya, "+180.0");
	HR_V4_String($xi - $LabelOffset, $yi+0.5*($ya-$yi), "0.0");
	HR_V4_String($xi - $LabelOffset, $yi, "-180.0");
	
	
	printf "0.0 setgray\n";              # set graylevel to black
	printf "%f sli\n", $AngleCrossLineWidth;
	
	printf "newpath\n";                  # phi_motif line segments
	$x = $x0 + $ColumnPadding + 0.5*$ColumnWidth;
	$phi_motif = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[0][0];
	printf "%f %f m\n", $x, $phi_motif; 
    for (my $i = 1; $i < $nAngles; $i ++) {
		$x = $x0 + (($i + 1) * $ColumnPadding) + ($i * $ColumnWidth) + 0.5 * $ColumnWidth;
		$phi_motif = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[$i][0];
	    printf "%f %f l\n", $x-($AngleMarkPts*0.75), $phi_motif; 
	}
	printf "stroke\n";
	
	printf "newpath\n";                  # psi_motif line segments
	$x = $x0 + $ColumnPadding + 0.5*$ColumnWidth;
	$psi_motif = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[0][3];
	printf "%f %f m\n", $x, $psi_motif; 
    for (my $i = 1; $i < $nAngles; $i ++) {
		$x = $x0 + (($i + 1) * $ColumnPadding) + ($i * $ColumnWidth) + 0.5 * $ColumnWidth;
		$psi_motif = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[$i][3];
	    printf "%f %f l\n", $x+($AngleMarkPts*0.75), $psi_motif; 
	}
	printf "stroke\n";

		

    for (my $i = 0; $i < $nAngles; $i ++) {

		my $x = $x0 + (($i + 1) * $ColumnPadding) + ($i * $ColumnWidth) + 0.5 * $ColumnWidth;

		$psi_motif = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[$i][3];
		$psi_ensemble = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[$i][4];
		$psi_stdev    = (($ya-$yi)/360) * $Angles[$i][5];
		WriteAngleMark( $x+($AngleMarkPts*1.2), $psi_motif, 'y', $psi_ensemble, $psi_stdev );

		$phi_motif = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[$i][0];
		$phi_ensemble = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[$i][1];
		$phi_stdev    = (($ya-$yi)/360) * $Angles[$i][2];
		WriteAngleMark( $x-($AngleMarkPts*0.75), $phi_motif, 'f', $phi_ensemble, $phi_stdev );
	}

    printf "%% End Angles \n";
	
	$dy_Axis = $ya - $y0;	# Adjust $dy_Axis
	
    return;
}
##############################################
# WriteAngleMark 
##############################################
# Write one boxed letter centred on coordinates in symbol font
sub WriteAngleMark {
    
	my ($xc, $yc, $s, $e, $v) = @_;

	printf "%f sli\n", $AngleCrossLineWidth;
	
	my $d = $AngleMarkPts * 0.5;
	
	printf "0.0 setgray\n";              # ensemble average and standard deviations  
    printf "newpath\n";    
    printf "%f %f m %f %f l\n", $xc-3*$d, $e-$v, $xc+3*$d, $e-$v;	# y_ensemble - stddev
    printf "%f %f m %f %f l\n", $xc-3*$d, $e+$v, $xc+3*$d, $e+$v;	# y_ensemble + stddev
    printf "%f %f m %f %f l\n", $xc, $e+$v, $xc, $e-$v;	# connecting line
    printf "stroke\n";    

	printf "1.0 setgray\n";              # white filled box    
    printf "newpath\n";    
    printf "%f %f m %f %f l %f %f l %f %f l %f %f l\n", $xc-$d, $yc+$d, $xc+$d, $yc+$d, $xc+$d, $yc-$d, $xc-$d, $yc-$d, $xc-$d, $yc+$d;
    printf "fill\n";    

	printf "0.0 setgray\n";              # black frame box  
    printf "newpath\n";    
    printf "%f %f m %f %f l %f %f l %f %f l %f %f l\n", $xc-$d, $yc+$d, $xc+$d, $yc+$d, $xc+$d, $yc-$d, $xc-$d, $yc-$d, $xc-$d, $yc+$d;
    printf "stroke\n";    
	
	printf "%5.3f p Symb\n", $AngleMarkPts;

	# print a string horizontally and vertically centred on $xc, $yc
    printf "gsave\n";      
    printf "%f %f translate\n", $xc, $yc;
    printf "newpath\n";     
    printf "0 0 m\n";    
    printf "(%s) false charpath flattenpath\n", $s;    # calculate offset to center-align
    printf "pathbbox\n";                               # stack: xl yl xu yu
	printf "3 -1 roll exch\n";						   # stack: xl xu yl yu
	printf "2 copy exch sub 0.5 mul\n";				   # stack: xl xu yl yu 0.5dy
	printf "exch pop add neg\n";				       # stack: xl xu y_off
	printf "3 1 roll\n";				               # stack: y_off xl xu
	printf "2 copy exch sub 0.5 mul\n";				   # stack: y_off xl xu 0.5dx
	printf "exch pop add neg\n";				       # stack: y_off x_off
	printf "exch\n";								   # stack: x_off y_off
    printf "m\n";									   # stack: ...
    printf "(%s) show \n", $s;
    printf "grestore\n";

    return;
}

#   debug:
#   printf "4 copy m pop pop 4 copy 4 1 roll exch l pop pop 4 copy pop pop l 4 copy 4 1 roll pop pop exch l 4 copy l pop pop stroke\n";    # debug: box with xl yl xu yu
#   printf "pstack\n";

##############################################
# Write AngleBoxes
##############################################
# Write the phi and psi angle Data Ramachandran box style

sub WriteAngleBoxes {
    
	my $xi = $x0 + $ColumnPadding;
	my $xa = $xi + $ColumnWidth;
    my $yi = $y0 + $dy_Axis + $AngleboxOffset;
    my $ya = $yi + $dy_Anglebox;

    printf "%% Begin Angles - Ramachandran box style\n";
	
	# write psi-labels only for first box

    printf "%f sli\n", $AngleTickLineWidth;    
	printf "newpath\n";      # tickmarks
	printf "%f %f m %f %f l\n", $xi, $yi, $xi-$dx_Tick, $yi;
	printf "%f %f m %f %f l\n", $xi, $yi+0.5*($ya-$yi), $xi-$dx_Tick, $yi+0.5*($ya-$yi);
	printf "%f %f m %f %f l\n", $xi, $ya, $xi-$dx_Tick, $ya;
	printf "stroke\n";
        
    printf "%5.3f p Helv\n", $AngleLabelPts;  # Set labelfont to Helvetica
	HR_V4_String($xi - $LabelOffset, $ya, "+180");
	HR_V4_String($xi - $LabelOffset, $yi+0.5*($ya-$yi), "0");
	HR_V4_String($xi - $LabelOffset, $yi, "-180");
    printf "%5.3f p Symb\n", $AngleUnitsPts;  # Set font to Symbol
	HR_V4_String($xi - $AngleUnitsOffset, $yi+0.5*($ya-$yi), "y");

    for (my $i = 0; $i < $nAngles; $i ++) {

		$xi = $x0 + (($i + 1) * $ColumnPadding) + ($i * $ColumnWidth);
		$xa = $xi + $ColumnWidth;

		my $phi_ensemble = ($xi+($xa-$xi)/2) + (($xa-$xi)/360) * $Angles[$i][1];
		my $psi_ensemble = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[$i][4];
		my $phi_stdev    = (($xa-$xi)/360) * $Angles[$i][2];
		my $psi_stdev    = (($ya-$yi)/360) * $Angles[$i][5];

# print STDERR "$phi_ensemble | $psi_ensemble | $phi_stdev | $psi_stdev\n";

		WriteAngleEllipse( $phi_ensemble, $psi_ensemble, $phi_stdev, $psi_stdev );

		my $phi_motif = ($xi+($xa-$xi)/2) + (($xa-$xi)/360) * $Angles[$i][0];
		my $psi_motif = ($yi+($ya-$yi)/2) + (($ya-$yi)/360) * $Angles[$i][3];

		printf "0.0 setgray\n";              # set graylevel to black
		printf "%f sli\n", $AngleCrossLineWidth;
		printf "newpath\n";                  # Angle crosshairs
		printf "%f %f m %f %f l\n", $phi_motif-0.5*$l_AngleCross, $psi_motif, $phi_motif+0.5*$l_AngleCross, $psi_motif; 
		printf "%f %f m %f %f l\n", $phi_motif, $psi_motif-0.5*$l_AngleCross, $phi_motif, $psi_motif+0.5*$l_AngleCross; 
		printf "stroke\n";

		printf "%f sli\n", $AngleboxLineWidth;
		WriteAngleboxFrame( $xi, $xa, $yi, $ya );
		
	}
    printf "%% End Angles \n";
	
	$dy_Axis = $ya - $y0;	# Adjust $dy_Axis

    return;
}

##############################################
# WriteAngleEllipse 
##############################################
# Write ellipse, centred on ensemble average and with horizontal and vertical axes corresponding to stddevs
# approximate $kappa 0.5522 (see: http://www.whizkidtech.redprince.net/bezier/circle/ )
sub WriteAngleEllipse {
    
	my ($xc, $yc, $x_dev, $y_dev) = @_;
	my $k = 0.5522;
	
		printf "0.5 setgray\n";              # set graylevel to 0.5
		printf "%f sli\n", $AngleEllipsisLineWidth;

		if ($Flags{"fill"} eq "grey" || $Flags{"fill"} eq "contour") {		# determine color or grey
                printf "0.5 setgray\n";    
		} elsif ($Flags{"fill"} eq "monochromeG") {		
                printf "0.0 1.0 0.0 setrgbcolor\n";
		} elsif ($Flags{"fill"} eq "monochromeB") {		
                printf "0.3 0.3 1.0 setrgbcolor\n";
		} else {
                printf "1.0 0.0 0.0 setrgbcolor\n";
		}   

    printf "newpath\n";                  # ellipsis
    printf "%f %f m\n", $xc, $yc+$y_dev; # start at top
	printf "%f %f %f %f %f %f curveto\n", $xc+($x_dev*$k), $yc+$y_dev, $xc+$x_dev, $yc+($y_dev*$k), $xc+$x_dev, $yc;	# top    right segment
	printf "%f %f %f %f %f %f curveto\n", $xc+$x_dev, $yc-($y_dev*$k), $xc+($x_dev*$k), $yc-$y_dev, $xc, $yc-$y_dev;	# bottom right segment
	printf "%f %f %f %f %f %f curveto\n", $xc-($x_dev*$k), $yc-$y_dev, $xc-$x_dev, $yc-($y_dev*$k), $xc-$x_dev, $yc;	# top    left  segment
	printf "%f %f %f %f %f %f curveto\n", $xc-$x_dev, $yc+($y_dev*$k), $xc-($x_dev*$k), $yc+$y_dev, $xc, $yc+$y_dev;	# bottom right segment
    printf "stroke\n";

    return;
}

##############################################
# WriteAngleboxFrame 
##############################################
# Write frame for box angleplots

sub WriteAngleboxFrame {
    
	my ($xi, $xa, $yi, $ya) = @_;
    printf "0.0 setgray\n";              # set graylevel to black

    printf "%f sli\n", $AngleboxLineWidth;

    printf "newpath\n";                  # Anglebox
    printf "%f %f m %f %f l %f %f l %f %f l %f %f l\n", $xi, $yi, $xi, $ya, $xa, $ya, $xa, $yi, $xi, $yi; 
    printf "stroke\n";
	
    printf "%f sli\n", $AngleAxisLineWidth;

    printf "newpath\n";                  # Anglebox axes
    printf "%f %f m %f %f l\n", $xi + 0.5*($xa - $xi), $yi, $xi + 0.5*($xa - $xi), $ya; 
    printf "%f %f m %f %f l\n", $xi, $yi + 0.5*($ya - $yi), $xa, $yi + 0.5*($ya - $yi); 
    printf "stroke\n";
	
    printf "%f sli\n", $AngleTickLineWidth;
	
	my $nTicks = int (360 / $AngleTickDivisions);
	my $dx_T = ($xa - $xi) / $nTicks; 
	my $dy_T = ($ya - $yi) / $nTicks; 
	
    printf "newpath\n";                  # Anglebox ticks

	for (my $i = 1; $i < $nTicks; $i++) {
		printf "%f %f m %f %f l\n", $xi+$i*$dx_T, $ya-0.5*$l_AngleTick, $xi+$i*$dx_T, $ya;	# x-top
		printf "%f %f m %f %f l\n", $xi+$i*$dx_T, $yi+0.5*($ya-$yi)-0.5*$l_AngleTick, $xi+$i*$dx_T, $yi+0.5*($ya-$yi)+0.5*$l_AngleTick;	# x-centre
		printf "%f %f m %f %f l\n", $xi+$i*$dx_T, $yi+0.5*$l_AngleTick, $xi+$i*$dx_T, $yi;	# x-bottom
 
		printf "%f %f m %f %f l\n", $xi+0.5*$l_AngleTick, $yi+$i*$dy_T, $xi, $yi+$i*$dy_T; # y-left
		printf "%f %f m %f %f l\n", $xi+0.5*($xa-$xi)-0.5*$l_AngleTick, $yi+$i*$dy_T, $xi+0.5*($xa-$xi)+0.5*$l_AngleTick, $yi+$i*$dy_T; # y-centre
		printf "%f %f m %f %f l\n", $xa-0.5*$l_AngleTick, $yi+$i*$dy_T, $xa, $yi+$i*$dy_T; # y-right
	
	}
    printf "stroke\n";
	
    return;
}

##############################################
# WriteTitle
##############################################
# Write the title and subtitle strings

sub WriteTitle {
    # === Title ======================
    printf "%% Begin Title \n";
    

	my $yMin_Title = $y0 + $dy_Axis + $TitleOffset;

    for (my $i = 0; $i < scalar (@TitleStrings); $i++) {   # calculate y offset for each line (1.5 line spacing) and print

		printf "%5.3f p Helv\n", $TitleStringPts[$i] * $UnitHeight;
		
		if ($Flags{"TitleAlign"} eq "centred") {
		
			HC_VB_String($x0 + (0.5 * $dx_Axis), 
                    $yMin_Title + (scalar (@TitleStrings) - 1 - $i) * 1.5 * $TitlePts * $UnitHeight,
                    $TitleStrings[$i]);
		} else {
			printf ("%5.3f %5.3f m\n", $x0, $yMin_Title + (scalar(@TitleStrings) - 1 - $i) * 1.5 * $TitlePts * $UnitHeight);
			printf ("(%s) show \n", $TitleStrings[$i]);
		}
    }

	$ya_Bbox = $yMin_Title + (scalar(@TitleStrings) * 1.5 * $TitlePts * $UnitHeight) + $ColumnPadding;		# upper y for BoundingBox

    printf "%% End Title \n";
    
    return;
}

##############################################
# HR_V4_String
##############################################
# Horizontal right-align a string on x,
# Vertical align on -1/3 y

sub HR_V4_String {

    my ($x, $y, $String) = @_;
          
    printf "gsave\n";      
    printf "%f %f translate\n", $x, $y;
    printf "newpath\n";     
    printf "0 0 m\n";    
    printf "(%s) false charpath pathbbox\n", $String;   # stack: ... xl yl xu yu
    printf "exch 4 1 roll sub 0.25 mul\n";              # stack: ... xu xl -0.25dy
    printf "3 1 roll exch sub exch\n";                  # stack: ... -dx -0.25dy
    printf "m\n";                                       # -dx -0.25dy moveto
    printf "(%s) show \n", $String;
    printf "grestore\n";

    return;
}


##############################################
# HC_VB_String
##############################################
# Horizontal Center a string on x,
# Vertical bottom align on y

sub HC_VB_String {
    my ($x, $y, $String) = @_;
      
    printf "gsave\n";      
    printf "%f %f translate\n", $x, $y;
    printf "newpath\n";     
    printf "0 0 m\n";    
    printf "(%s) false charpath \n", $String;          # calculate offset to center-align
    printf "pathbbox\n";                               # stack: ... xl yl xu yu
    printf "pop exch pop sub\n";                       # stack: ... -dx
    printf "0.5 mul 0 m\n";                            # -0.5dx 0 moveto
    printf "(%s) show \n", $String;
    printf "grestore\n";

    return;
}


##############################################
# WritePSFooter
##############################################
# Write a Footer for a Postscript File

sub WritePSFooter {

#   my ($HANDLE) = @_;
      
   printf "%% Begin footer\n";
   printf "%%%%BoundingBox: %d %d %d %d \n", int($xi_Bbox), int($yi_Bbox), int($xa_Bbox)+1, int($ya_Bbox)+1;

   # printf "showpage\n"; # only needed if ps output is to be sent to printer

   printf "%% End Footer\n";
   printf "%% EOF\n";

   return;
}

##############################################
# 
##############################################
