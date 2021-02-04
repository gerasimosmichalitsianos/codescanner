# CodeScanner

This is a Python 3 command-line program to scan scientific source codes for security flaws,
as well as raise warnings and flags for best as well as possible poor scientific programming practices. 
This code was developed in NOAA's Office of Satellite Products and Operations (NOAA/OSPO). To this 
end, it runs several well-known static command-line tools (**cppcheck**, **flawfinder**, **shellcheck**), as 
well as parses source codes manually and write an output text final report. This report will be 
present in the directory being scanned (the directory containing your source codes 
that is being analyzed).

For example, if your source-code directory contains Fortran 90 source files, this command-line tool
will list those Fortran 90 source files that do not contain **IMPLICIT NONE**, whose use in Fortran 90
is highly recommended. This program will also scan C/C++ source codes, and if present, make sure
the **new** keyword is accompanied by a corresponding usage of **delete []**, that pointer variables 
created with **alloc()** or **malloc()** are checked for allocation success (e.g. comparison to NULL
in an if-statement), and so on and so forth. All results are written to an a single output
report, with the exception of the results from flawfinder, shellcheck, and cppcheck, which
are written to their own separate text-files (which also go to the directory being scanned as their
final destination).
     
## Version

    CodeScanner Version 1.0.0
    4 February 2021

## Installation

Installation occurs in a UNIX or Linux command-line environment (e.g. Mac OSX works too).
Requires docker (https://www.docker.com/) to be installed onto your system.
 
    $ git clone https://github.com/gerasimosmichalitsianos/codescanner
    $ cd codescanner/codescanner
    $ docker build -t codescanner .
       
## Usage

Once you build the docker image from the installation above, you just specify the
input directory of your source codes to be scanned, and run the docker container:
       
    $ DIR=/home/my/directory/with/some/source/codes
    $ docker run -v $DIR:$DIR codescanner --directory $DIR
       
This is the recommended usage.
       
Alternatively, after checking out the code, you could simply do the following
to run this code directly with your local python interpreter (e.g. **/usr/bin/python3**):
 
    $ DIR=/home/gmichali/CCAP_MiRS_v1-0_20201228_FilesToReview
    $ git clone https://github.com/gerasimosmichalitsianos/codescanner
    $ cd codescanner/codescanner
    $ python3 codescanner.py --directory $DIR
       
  Note that if **docker** is not used per above, you will need to manually install the following 
  command-line tools onto your system (as well as Python 3) to run the code directly:
       
    (1) shellcheck
    (2) cppecheck
    (3) flawfinder
    (4) count lines of code (cloc)

## Items Covered

  **i. Source code files that may lack proper header or prologue.**
    
    It is important that scientific codes contain at least a few comments at the top of the
    source file to describe what the source file is used for. Or at least some sort of header
    or prologue. Those source file-names are reported that may lack a header or comments at
    the top of the file. Note that machine-generated files do not need such headers, and may
    be reported as false positives.
    
  **ii. system() calls are parsed and reported** 
           
    It is important for the variables or strings inside system() calls to be well-parameterized.
    The system() calls are written to the output report so the programmer may check to see they
    are well-parameterized.
    
  **iii. long source code lines**

    Lines that are longer than 120 characters are reported to the final CodeScanner output report.
    It is best practice that source code lines should be 120 characters or less.
    
  **iv. Comparisons using == , !=, /= , or other similar operators**
  
    Possible instances are reported to the final output report if the ==,!= or other similar 
    operators are used in which a mathematical expression is compared to a float. This may
    create errors in rounding or accuracy that could break the code.
    
  **v. Leading Tabs**
  
    Source code lines that may contain leading tabs are reported. It is recommended to use
    spaces instead of leading tabs, especially in Fortran codes (some would argue Python too).
    
  **vi. Possible Hard-Coded IP Addresses**

    IP addresses should never be hard-coded. Or, at least, they should at least be inside
    configuration files or something similar. This item writes out possible instances of 
    hard-coded IP addresses. Again, false-positives are possible.
    
  **vii. Possible instances of hard-coded directories or filenames**
  
    Possible instances of hard-coded directories or filenames are reported. It is recommended
    that these be placed into configuration files (e.g. yaml). Such instances of these are
    reported so the programmer may see if hard-coded directories or filenames are at least
    well-parameterized or placed into a configuration file.
    
  **viii. Possible hard-coded passwords**
  
    Possible instances of hard-coded passwords are reported. Various terms are grepped or 
    searched for, including "username,"password,"uname,", and so on and so forth. These 
    are reported, should they exist. False positives are possible as usual.
    
  **ix. Instances of hard-coded filenames in open() and close() statements**
  
    Again, filenames should be well-parameterized, and not hard-coded into open() and
    close() statements in your codes. Possible instances of this are reported.
    
  **x. Possible Division by Zero**
  
    Possible instances of division by zero are repoted. False positives occur often.
    
  **xi. Usage of Magic Numbers**
  
    Instances of possible magic numbers are reported for all languages. Magic numbers
    are hard-coded numbers that often appear in conditional statements (e.g. if, else,
    while statements); these numbers should be parameterized if possible.

## @author: 
    Gerasimos Michalitsianos
    gerasimosmichalitsianos@gmail.com
    4 February, 2021
