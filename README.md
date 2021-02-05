# CodeScanner

This is a **Python 3** command-line program used to scan scientific source 
codes for security flaws, as well as raise warnings and flags for 
best as well as possible poor scientific programming practices. This 
code was developed in NOAA's Office of Satellite Products and Operations 
(NOAA/OSPO). To this end, it runs several well-known static command-line 
tools (**cppcheck**, **flawfinder**, **shellcheck**), as well as parses 
source codes manually and writes an output text final report. This report 
will be present in root of the directory being scanned (the directory containing 
your source codes that is being analyzed) with the name *codeScannerReport.txt*.

For example, if your source-code directory contains Fortran 90 source 
files, this command-line tool will list those Fortran 90 source files that 
do not contain **IMPLICIT NONE**, whose use in Fortran 90 is highly recommended. 
This program will also scan C/C++ source codes, and if C/C++ is present, will check to see if
the **new** keyword is accompanied by a corresponding usage of **delete []** (C++). 
It will also look to see if pointer variables created with **alloc()** or **malloc()** 
are checked for allocation success (e.g. comparison to **NULL** in a subsequent
**if**-statement), and so on and so forth. False-positives are possible for most
items described below. All results are written to an a single output report, 
with the exception of the results from *flawfinder*, *shellcheck*, and *cppcheck*, which
are written to their own separate text-files (which also go to the root of the directory 
being scanned as their final destination). For most items, unless stated otherwise, 
the names of the source files, line numbers, and source code for that line are written out.

See the descriptions below in "Items Covered."
     
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

## Output

    A text file codeScannerReport.txt will be created in the output directory that
    is being scanned (e.g. in the directory defined by $DIR above).

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
    are well-parameterized. Hard-coded strings are less desirable in system() calls e.g.
    
      system( "rm outname.txt" ) or die("failure.");
      
    which may be better written as:
    
      my command = "rm outname.txt";
      my @args = ("rm", "outname.txt");
      system(@args) == 0
        or die "system @args failed: $?";
        
    For this item, all found lines that may contain system() calls are reported. It is then
    up to the developer to decide if this look OK and are appropriate.
    
  **iii. long source code lines**

    Lines that are longer than 120 characters are reported to the final CodeScanner output report.
    It is best practice that source code lines should be 120 characters or less. Just good 
    practice. Ultimately, the devlopers review and decide if these are OK.
    
  **iv. Comparisons using == , !=, /= , or other similar operators**
  
    Possible instances are reported to the final output report if the ==,!= or other similar 
    operators are used in which a mathematical expression is compared to a float. This may
    create errors in rounding or accuracy that could break the code e.g.
    
      if( (3.1459 * 0.001 * x ) == 0.01 ) { ... do stuff ... }
      
    would be very bad practice.
    
    All instances of usage of ==, !=, and /= etc. are reported (the source code lines)
    and written to the output CodeScanner report. It is then up to the developer to view
    the report and make sure such usage of !=, ==, and /= look OK and will not result
    in rounding errors.
    
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
    
    Possible instances (with false positives possible) of hard-coded directories and filenames
    are reported to the output CodeScanner report. It is up to the developers to decide if these
    are OK and can remain as-is.
    
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
    
  **xii. Instances of POINTER variables in Fortran 90+ codes that do not initialize to NULL**
  
    It is good practice to initialize Fortran POINTER variables to NULL e.g.
    
    INTEGER, POINTER, ALLOCATABLE :: x => NULL(). Those instances where NULL is not
    used are listed in the output report.
    
  **xiii. Instances of use of GOTO in FORTRAN codes.**
  
    An outdated practice. Should be replaced with CYCLE and EXIT statements for control-flow
    of loops.
    
  **xiv. Fortran source files that lack use of IMPLICIT NONE.**

    It's good practice to use IMPLICIT NONE in all FORTRAN codes (including Fortran 90+).
    Those source files in the input directory, being scanned, will be listed in the output
    CodeScanner report. Fortran MODULE files are ignored.
    
  **xv. Fortran usage of IMPLICIT variables.**
  
    Usage of IMPLICIT variables may be outdated. Instances of usage where variables are 
    declared as IMPLICIT are listed in the output report.
    
  **xvi. Fortran instances of usage of EQUIVALENCE**
  
    Another outdated Fortran method. EQUIVALENCE and its usage are listed in the output report.
    
  **xvii. Instances of Fortran COMMON block**
  
    The outdated usage of COMMON blocks are listed in the Fortran output report.
    
  **xviii. Instances of INCLUDE in Fortran**
  
    Usage of FORTRAN INCLUDE statements are listed. Again, this is outdated.
    
  **xix. Instances of Fortran numbered loops**
  
    Like GOTO statements, which numbered loops and statements often come hand-in-hand with,
    lines with numbered loops are listed in the output report.
    
  **xx. Fortran PRINT statements**
  
    Fortran PRINT statements should only be used for console output, and never with file
    handles (e.g. to write to a file). To this end, WRITE should be used. PRINT statements
    are listed so the programmer may view them in the CodeScanner output report.
    
  **xxi. Fortran WRITE statements**
    
    Instances of Fortran WRITE statements are written to the output CodeScaner report.
    
  **xxii. Usage of CALL EXIT in Fortran codes**
  
    Usage of CALL EXIT(N) is outdated. Instances are listed in the output report
    (lines that contain these statements), should they exist.
    
  **xxiii. Variable declarations that do not use KIND keyword**
  
    It's good practice in Fortran codes, to use the KIND keyword when declaring variables
    (e.g. of those of type REAL or INTEGER). This helps with code portability. Lines that
    lack use of KIND are listed in the output CodeScanner report.
    
  **xxiv. Instances of hard-coded array dimensions in Fortran code**
  
    It is less desirable to have hard-coded array dimension e.g. REAL,DIMENSION(10000) :: x.
    The value of 10,000 should go into a MODULE file or configuration file. Those lines that
    may contain hard-coded array declarations are listed.
    
  **xxv. Instances of C++ overloading of && , || , or "," (comma) operators**
   
    Some might agree its bad practice to overload the &&, ||, or "," operators in C++. This is
    becuase these symbols are used elsewhere in the syntax of C/C++. Such possible instances
    of this kind of overloading are listed in the output CodeScanner report.
    
  **xxvi. C++ lines with POINTER ARITHMETIC i.e.**
  
    Pointer arithmetic e.g. *(p+1) is common in C/C++, and is great for code optimization, and is much
    faster than array indexing. Nontheless, one must be careful to make sure the pointer arithmetic
    does not get too complicated. Possible instances of pointer arithmetic are listed in the
    output report for the programmer to review.
    
  **xxvii. C/C++ source files that may have DEEP NESTING{}:**
  
    Source files that may have very deep nesting, going as far as 5+ levels deep, are listed.
    Just so the programmer can review the algorithm and decide if the nesting can go too far.
    Again, no source code lines are listed here for this item, just source-code filenames.
    The names of files only.
    
  **xxviii. C++ instances of CONSTRUCTOR, DECONSTRUCTOR, COPY CONSTRUCTOR, and/or ASSIGNMENT OPERATOR**
  
    C++ usage of constructors, deconstructors, copy constructors, and assignment operators are parsed
    and listed for the programmer to review.
    
  **xxix. C/C++ instances of pointer casting**
  
    C/C++ lines with pointer casting are listed. Pointer casting is very common in C/C++. But one must
    be careful not to cast, for example, a pointer to an integer to a pointer to a double. This could
    possibly result in a buffer overflow (memory leak). Instances of pointer casting are listed for
    the programmer to review.
    
  **xxx. C++ instances of NEW without use of delete []:**
  
    When the C++ "new" keyword is used to allocate a new block of memory, the delete [] operator should
    always also accompany that variable later-on. Memory should be released as appropraite. Instances 
    of "new" without use of delete [] for variables in the C++ are listed in the CodeScanner report. 
    As usual, false-positives are possible because this tool does not perform cross-checking 
    between source files.
    
  **xxxi. C++ instances of magic numbers**
  
    Like above, magic numbers should be avoided in C/C++. This could be a maintainability issue.
    Try to parameterize variables as much as possible. Possible instances of C++ magic numbers
    are listed in the output CodeScanner report. Magic numbers are basically hard-coded numbers
    that often appear in conditional statements e.g. if( rain<1100 ) { do something ... ; }.
    What does the value of 1,100 even mean in this if-statement? Could it change? Best to 
    declare it with #define, or perhaps in some sort of configuration file.
    
  **xxxii. C instances of magic numbers**
  
    Like above, magic numbers should be avoided in C. This could be a maintainability issue.
    Try to parameterize variables as much as possible (e.g. with #define). Possible instances 
    of C magic numbers are listed in the output CodeScanner report.
    
  **xxxiii. C/C++ pointer indirection**
  
    Instances of pointer indirection are listed (four levels or more). This can be confusing.
    For example, source code lines that contain something like:
    
      float***** data;
      
    are flagged and written to the output CodeScanner report. This is alot of pointer
    indirection and may be confusing to those trying to read the code.
    
  **xxxiv. C/C++ instances of hard-coded array dimensions**
  
    Instances of possible hard-coded array dimensions are listed in the output CodeScanner report.
    Declaring small, hard-coded arrays is generally acceptable by most coding standards e.g. 
    small character arrays:
    
      char[8] name;
      
    This is fine and very common in C/C++. But in the case of:
    
      double[100000] data;
      
    Dynamic memory allocation really should be used (e.g. new and delete [ ] in C++ and
    free() and malloc() in C codes). 
    
    For this item, all (found) instances of hard-coded arrays are reported in C/C++ 
    source files, and those lines containing such are written to the output report. False
    positives are possible. It is then up to the programmer to view the output report 
    from CodeScanner and decide whether these are acceptable or not.
  
  **xxxv. C++ usage of NULL instead of nullptr**
  
    Modern C++ should use nullptr instead of NULL. Lines that use NULL (C++ only) are listed in the 
    CodeScanner output report.
    
    So C++ source code lines that may contain something like:
    
      int* x = NULL;
      
    will be written and reported to the output CodeScanner report and flagged.
    In reality, this line may be rewritten as:
    
      int* x = nullptr;
    
  **xxxvi. C/Fortran interoperability**
  
    For this item, its important to align each declaration type name, avoiding long or 
    continuation lines when declaring variables passed from the C to the Fortran. Possible
    lines that are too long containing the C-Fortran arguments are reported.
    
  **xxxvii. C/C++ interoperability**
  
    Use of extern "C" should be used to call C routines from C++. Lines with extern "C" are
    reported to the output CodeScanner report so the programmer may view them.
    
  **xxxviii. C/C++ large, hard-coded array (i.e. poor use of memory, use NEW or MALLOC)**
  
    This item is very similar to item #34. With #34 however, ALL instances of hard-coded
    array dimensions are listed (lines are listed in output CodeScanner report). For this
    item, numeric variables of type { double,integer,float, ...} are reported and only
    those lines are reported that contain array declarations with large dimensions
    (that lack the new or malloc keyword and have repeating numbers).
    
  **xxxix. C/C++ usage of malloc(),alloc() in which allocation is not checked for success**
    
    When the malloc() or alloc() functions are used, a pointer is created that points
    to that block of memory. This is good practice for use in dynamic memory allocation.
    
    After using these functions, the pointer should be checked for successful allocation
    of the memory e.g. comparison to NULL or nullptr. This item reports those lines that 
    use alloc() or malloc(), and checks to see in subsequent lines if that variable is 
    checked to be NULL. The output in the CodeScanner report may look like:
    
    Line 253:   lat_data = (float *)malloc(bytes_per_scan);
      check for allocation success: if (NULL == lat_data) { ...
      
    A warning is written to the output CodeScanner report if no such if-statement is found e.g.
    
    Line 117:   iobuf = (byte1 *)malloc(row_bytes);
      ... WARNING: Variable may not have been checked for allocation and may be NULL ...
      
  **xxxx. C/C++ switch{} statements that may lack a default{ } clause or block**
  
    It's good practice that all switch{} statements in C/C++ have default clauses.
    Those blocks of code that may lack a default clause are reported and written
    to the output CodeScanner report, which the developer may review.
  
## @author: 

    Gerasimos Michalitsianos
    gerasimosmichalitsianos@gmail.com
    4 February, 2021
    
    Also, this project would not have been possible without
    the extreme professionalism, diligence, and guidence of a
    Mr. Ian Simpson, a scientific programmer at NOAA. 
    
    Thank you Ian.
