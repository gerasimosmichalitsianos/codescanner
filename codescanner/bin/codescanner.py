import os 
import sys 
import getopt 
import argparse
import re
import glob 
import fnmatch 
import subprocess 
import datetime 
import pkg_resources
from scanner import Scanner
from fortranscanner import FortranScanner
from ccppscanner import CCPPScanner

class directoryscanner(object): 
	
  def __init__(self,InputDirectory,OutputDirectory=None): 
    self.InputDirectory  = InputDirectory 
    self.OutputDirectory = OutputDirectory

  def GenerateMasterReport(self):

    # first scrub directory of files that are not needed
    # (files that are not source files)
    #   i.e. .log, .exe, .dll, .png, .bin, .nc, ...
    # --------------------------------------------------
    Scanner.ScrubDirectory( self.InputDirectory )

    # Run flawfinder on the input directory
    # this looks at C/C++ codes
    # -------------------------------------

    CCPPReader    = CCPPScanner( self.InputDirectory )
    CCPPFileNames = CCPPReader.GetSourceCodeDirectoryFileNames() 
    
    if len( CCPPFileNames )>0:
      FlawFinderFile = os.path.join( self.OutputDirectory, 'ALL_FLAWFINDER.txt' )
      if os.path.isfile( FlawFinderFile ): os.remove( FlawFinderFile )
      FlawFinderCommand = ' '.join( ['flawfinder --minlevel=5 --falsepositives --singleline ', \
        self.InputDirectory,' 2>&1 | tee ', FlawFinderFile ])
      Scanner.RunCommand( FlawFinderCommand )
    
    # Run cppcheck on the input directory
    # this command line tool also looks like C/C++ codes
    # --------------------------------------------------
    
    if len( CCPPFileNames )>0:
      CPPCheckFile = os.path.join( self.OutputDirectory, 'ALL_CPPCHECK.txt' )
      if os.path.isfile( CPPCheckFile ): os.remove( CPPCheckFile )
      CPPCheckCommand = ' '.join([ 'cppcheck' , self.OutputDirectory, \
        '--quiet' , ' 2>&1 | tee ' , CPPCheckFile ]) 
      Scanner.RunCommand( CPPCheckCommand )

    CCPPReader.close()
    del CCPPReader
    
    # Run shellcheck commadn-line tool
    # --------------------------------
    Scanner.RunShellCheck( self.InputDirectory,self.OutputDirectory )    
    outnameReport = os.path.join(self.OutputDirectory,'codeScannerReport.txt')
    if os.path.isfile(outnameReport): os.remove(outnameReport) 

    # run perlcritic command line tool
    # --------------------------------
    ReportWriter = open(outnameReport,'w')
    ReportWriter.write('%s\n'%' -----------------------------------------------------------')
    ReportWriter.write('%s\n'%' NOAA Office of Satellite and Product Operations (NOAA/OSPO)')
    ReportWriter.write('%s\n'%' CodeScanner 1.0.0')
    ReportWriter.write('%s\n'%' -----------------------------------------------------------')
    ReportWriter.write('%s\n'%' ') 
    ReportWriter.write('%s\n'%'  Contact Information') 
    ReportWriter.write('%s\n'%'  @author  : Gerasimos Michalitsianos')
    ReportWriter.write('%s\n'%'  @email   : gerasimos.michalitsianos@noaa.gov')
    ReportWriter.write('%s\n'%'  @phone   : 301-683-3269 (Work) , 301-503-8968 (Mobile)')
    ReportWriter.write('%s\n'%('  @updated : '+LastUpdated)) 
    ReportWriter.write('%s\n'%' ')
    ReportWriter.write('%s\n'%' ----------------------------------------')
    ReportWriter.write('%s\n'%' General Summary of Source Code Directory                           ')
    ReportWriter.write('%s\n'%' ----------------------------------------')
    ReportWriter.write('%s\n'%' ')
    ReportWriter.write('%s\n'%'  Directory scanned:')
    ReportWriter.write('%s\n'%('  '+self.InputDirectory))
    ReportWriter.write('%s\n'%' ')
    ReportWriter.write('%s\n'%('  Date/Time of Scanning: '+str(datetime.datetime.now())))
    ReportWriter.write('%s\n'%' ')

    cmd = 'cloc '+self.InputDirectory
    Scanner.WriteLines( 
      Scanner.FilterLinesCLOC( Scanner.RunCommand(cmd) ), ReportWriter, '  ' )

    # Get general code-scanning results (any langauge)
    # ------------------------------------------------
    DirectoryScanner = Scanner( self.InputDirectory )

    Scanner.WriteItemHeader(
      'I','Files that may lack a proper header and/or prologue description: ',
      ReportWriter )
    DirectoryScanner.SourceFilesWithoutHeader( ReportWriter )

    Scanner.WriteItemHeader(
      'II','Files with SYSTEM() calls: ',
      ReportWriter )
    SystemCallsDict = DirectoryScanner.GetLinesWithSystemCalls()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Files with SYSTEM() calls' , SystemCallsDict , ReportWriter,None )

    Scanner.WriteItemHeader(
      'III','Files with LONG SOURCE CODE LINES ',
      ReportWriter )
    LongLinesDict = DirectoryScanner.SourceFilesWithLongLines()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Files with LONG SOURCE CODE LINES' , LongLinesDict , ReportWriter,500,5 )

    Scanner.WriteItemHeader(
      'IV','Files that may have a FLOATING POINT == or /= comparison: ',
      ReportWriter )
    LinesWithFloatComparisons = DirectoryScanner.GetLinesWithFloatComparison()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Files with FLOATING POINT == or /= comparison' , LinesWithFloatComparisons, ReportWriter, None )
    
    Scanner.WriteItemHeader(
      'V','Files that may have lines with LEADING TABS: ',
      ReportWriter )
    LinesWithLeadingTabs = DirectoryScanner.GetLinesWithLeadingTabs()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Files with lines that contain LEADING TABS' , LinesWithLeadingTabs, ReportWriter,None )

    Scanner.WriteItemHeader(
      'VI','Files that may have lines with HARD CODED IP addresses: ',
      ReportWriter )
    LinesWithHardCodedIPs = DirectoryScanner.GetLinesWithHardCodedIPs()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Files with lines with possible hard-coded IP addresses' , LinesWithHardCodedIPs, ReportWriter, None )

    Scanner.WriteItemHeader(
      'VII','Files that may have lines with HARD CODED FILENAMES OR PATHS: ',
      ReportWriter )
    LinesWithPossibleHardCodedPaths = DirectoryScanner.GetLinesWithHardCodedPaths()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Files with lines with possible HARD CODED FILENAMES OR PATHS' , LinesWithPossibleHardCodedPaths, ReportWriter, None )

    Scanner.WriteItemHeader(
      'VIII','Files that may have possible hard-coded passwords: ',
      ReportWriter )
    LinesWithPossibleHardCodedPasswords = DirectoryScanner.GetLinesWithPossibleHardCodedCredentials()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Files with lines with possible hard-coded credentals or passwords' , LinesWithPossibleHardCodedPasswords, ReportWriter, None )

    Scanner.WriteItemHeader(
      'IX','Files and lines with HARD-CODED FILENAMES in OPEN() or other FUNCTION calls/statements: ',ReportWriter )
    LinesWithHardCodedFileNameInOpenStatement = DirectoryScanner.GetLinesWithHardCodedPathsInOpenStatement()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Instances of HARD-CODED FILENAMES IN OPEN() or other FUNCTION CALLS/STATEMENTS ' , LinesWithHardCodedFileNameInOpenStatement , ReportWriter, None )
    DirectoryScanner.close()

    Scanner.WriteItemHeader(
      'X','Files and lines with POSSIBLE DIVISION BY ZERO (false positives possible): ',ReportWriter )
    LinesWithPossibleDivisionByZero = DirectoryScanner.GetLinesWithPossibleDivisionByZero()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Instances of possible division by zero (false positives possible) ' , LinesWithPossibleDivisionByZero , ReportWriter, None )

    Scanner.WriteItemHeader(
      'XI','Files and lines with possible HARD-CODED NUMBERS (MAGIC NUMBERS): ',ReportWriter )
    LinesWithMagicNumbers = DirectoryScanner.LinesWithMagicNumbersAnyLanguage()
    Scanner.WriteFileNamesAndLinesToReport( 
      'Files and lines with possible MAGIC NUMBERS ' , LinesWithMagicNumbers , ReportWriter, None )
    DirectoryScanner.close()

    # Get FORTRAN code scanning results
    # ---------------------------------

    FortranReader  = FortranScanner( self.InputDirectory )
    Scanner.WriteItemHeader( 'XII' , 'FORTRAN source files that do not initialize POINTER variables to NULL(): ', ReportWriter ) 
    LinesWithPointerWithoutNULL = FortranReader.InstancesOfPointerDeclarationsWithoutNull()
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN source files without NULL() POINTER initialization',LinesWithPointerWithoutNULL, ReportWriter, 4, 2 )

    Scanner.WriteItemHeader( 'XIII' , 'FORTRAN source files with GOTO: ', ReportWriter ) 
    LinesWithGoTo = FortranReader.InstancesOfFortranGoTo()
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN source files without GOTO',LinesWithGoTo, ReportWriter, None )

    # list FORTRAN source files that lack use of IMPLICIT NONE 
    # --------------------------------------------------------
    Scanner.WriteItemHeader( 'XIV' , 'FORTRAN source files that may lack IMPLICIT NONE: ', ReportWriter ) 
    FilesLackingImplicitNone = FortranReader.FilesWithoutImplicitNone()
    Scanner.WriteFileNamesToReport( 
      'FORTRAN source files without IMPLICIT NONE',FilesLackingImplicitNone,ReportWriter )

    # list instances where implicit variables are indeed used in Fortran
    # ------------------------------------------------------------------
    Scanner.WriteItemHeader( 'XV' , 'FORTRAN usage of IMPLICIT variables: ', ReportWriter ) 
    InstancesOfImplicitVariables = FortranReader.InstancesOfImplicitVariableUse()
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN source files that use implicit variables',InstancesOfImplicitVariables,ReportWriter,None )

    Scanner.WriteItemHeader( 
       'XVI' , 'FORTRAN source files that may have instances of EQUIVALENCE: ', ReportWriter ) 
    LinesWithEquivalence = FortranReader.InstancesOfEquivalence()
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN source files with EQUIVALENCE' , LinesWithEquivalence, ReportWriter, None )

    LinesWithCommonBlock = FortranReader.InstancesOfCommonBlock()
    Scanner.WriteItemHeader( 
       'XVII' , 'FORTRAN source files that may have instances of COMMON: ', ReportWriter ) 
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN source files with COMMON' , LinesWithCommonBlock, ReportWriter, None )

    LinesWithCommonBlock = FortranReader.InstancesOfInclude()
    Scanner.WriteItemHeader( 
       'XVIII' , 'FORTRAN source files that may have instances of INCLUDE: ', ReportWriter ) 
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN source files with INCLUDE' , LinesWithCommonBlock, ReportWriter, None )

    LinesWithNumberedLoops = FortranReader.InstancesOfNumberedLoops()
    Scanner.WriteItemHeader( 
       'XIX' , 'FORTRAN source files that may have instances of NUMBERED LOOPS: ', ReportWriter ) 
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN source files with NUMBERED LOOPS' , LinesWithNumberedLoops, ReportWriter, None, 5 )

    LinesWithPrintStatements = FortranReader.InstancesOfPrintStatements()
    Scanner.WriteItemHeader( 
       'XX' , 'FORTRAN PRINT statements: ', ReportWriter ) 
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN PRINT statements', LinesWithPrintStatements, ReportWriter, None  )

    LinesWithWriteStatements = FortranReader.InstancesOfWriteStatements()
    Scanner.WriteItemHeader( 
       'XXI' , 'FORTRAN source files WRITE statements: ', ReportWriter ) 
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN PRINT statements' , LinesWithWriteStatements, ReportWriter, None  )

    LinesWithCallExitStatements = FortranReader.InstancesOfCallExitStatements()
    Scanner.WriteItemHeader( 
       'XXII' , 'FORTRAN source files CALL EXIT statements: ', ReportWriter ) 
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN CALL EXIT statements' , LinesWithCallExitStatements, ReportWriter, None, 2  )

    LinesWithoutKind = FortranReader.InstancesOfDeclarationsWithoutKind()
    Scanner.WriteItemHeader( 
       'XXIII' , 'FORTRAN source files that may lack usage of KIND or formal type declarations: ', ReportWriter ) 
    Scanner.WriteFileNamesAndLinesToReport( 
      'FORTRAN source files that may lack usage of KIND' , LinesWithoutKind, ReportWriter, None  )

    LinesWithHardCodedArrays = FortranReader.GetLinesWithHardCodedArrayDimensions() 
    Scanner.WriteItemHeader( 
       'XXIV' , 'FORTRAN source files that may have hard-coded array dimensions: ', ReportWriter ) 
    Scanner.WriteFileNamesAndLinesToReport( 
       'FORTRAN source files that have hard-coded array dimensions' , LinesWithHardCodedArrays, ReportWriter, None )
    FortranReader.close()

    # Get C++ code scanning results
    # -----------------------------
   
    CCPPReader = CCPPScanner( self.InputDirectory )
    CCPPReader.SwitchStatementsWithoutDefault(ReportWriter,'XXIV')

    LinesWithOperatorOverloading = CCPPReader.InstancesOfOperatorOverloading()     
    Scanner.WriteItemHeader( 
      'XXV' , 'C++ source files with OPERATOR OVERLOADING of: , (comma) || && ): ', ReportWriter ) 
    Scanner.WriteFileNamesAndLinesToReport( 
      'C++ instances of OPERATOR OVERLOADING { "," , "&&", "||" } ' , LinesWithOperatorOverloading, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXVI' , 'C++ lines with POINTER ARITHMETIC i.e. *( p+i ) : ', ReportWriter ) 
    LinesWithPointerArithmetic = CCPPReader.LinesWithPointerArithmetic() 
    Scanner.WriteFileNamesAndLinesToReport( 
      'C++ instances of POINTER ARITHMETIC: ' , LinesWithPointerArithmetic, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXVII' , 'C/C++ source files that may have DEEP NESTING{}: ', ReportWriter ) 
    FileNamesWithDeepNesting = CCPPReader.CCPPInstancesOfDeepNesting()
    Scanner.WriteFileNamesToReport( 
      'C/C++ source files with DEEP NESTING{}',FileNamesWithDeepNesting, ReportWriter )

    Scanner.WriteItemHeader( 
      'XXVIII' , 'C++ instances of CONSTRUCTOR, DECONSTRUCTOR, COPY CONSTRUCTOR, and/or ASSIGNMENT OPERATOR: ', ReportWriter ) 
    FileNamesAndLinesWithMissingClassDefinitions = CCPPReader.CCPPDefinedConstructors()
    Scanner.WriteFileNamesAndLinesToReport( 
      'C++ instances of DECONSTRUCTOR,CONSTRUCTOR(s),ASSIGNMENT OPERATOR,...' , 
      FileNamesAndLinesWithMissingClassDefinitions, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXIX' , 'C/C++ instances of POINTER CASTING: ', ReportWriter ) 
    PointerCastingLines = CCPPReader.InstancesOfCastingPointers()
    Scanner.WriteFileNamesAndLinesToReport( 
      'C/C++ instances of POINTER casting' , PointerCastingLines, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXX' , 'C++ instances of NEW without use of DELETE []: ', ReportWriter ) 
    LinesWithNewAndNoDelete = CCPPReader.InstancesOfNewWithoutDelete('new','delete', 'C++' )
    Scanner.WriteFileNamesAndLinesToReport( 
      'C++ instances of NEW without DELETE' , LinesWithNewAndNoDelete, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXXI' , 'C++ POSSIBLE instances of MAGIC NUMBERS: ', ReportWriter ) 
    LinesWithMagicNumbers = CCPPReader.LinesWithMagicNumbers('C++')
    Scanner.WriteFileNamesAndLinesToReport( 
      'C++ POSSIBLE instances of MAGIC NUMBERS' , LinesWithMagicNumbers, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXXII' , 'C POSSIBLE instances of MAGIC NUMBERS: ', ReportWriter ) 
    LinesWithMagicNumbers = CCPPReader.LinesWithMagicNumbers('C')
    Scanner.WriteFileNamesAndLinesToReport( 
      'C POSSIBLE instances of MAGIC NUMBERS' , LinesWithMagicNumbers, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXXIII' , 'C/C++ too much pointer indirection: ', ReportWriter ) 
    LinesWithTooMuchIndirection = CCPPReader.LinesWithTooMuchPointerIndirection()
    Scanner.WriteFileNamesAndLinesToReport( 
      'C/C++ lines with too much pointer indirection (i.e. float **** )' , LinesWithTooMuchIndirection, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXXIV' , 'C/C++ with hard-coded array dimensions: ', ReportWriter ) 
    LinesWithHardCodedArrayDimensions = CCPPReader.LinesWithHardCodedArrayDimensions()
    Scanner.WriteFileNamesAndLinesToReport( 
      'C/C++ lines with hard-coded array dimensions' , LinesWithHardCodedArrayDimensions, ReportWriter, None  )
    
    Scanner.WriteItemHeader( 
      'XXXV' , 'C++ lines that use NULL instead of null_ptr: ', ReportWriter ) 
    LinesWithNullAndNotNullPtr = CCPPReader.LinesWithNullAndNotNullPtr()
    Scanner.WriteFileNamesAndLinesToReport( 
      'C++ lines that do not use null_ptr and use NULL instead.' , LinesWithNullAndNotNullPtr, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXXVI' , 'C/Fortran interoperability: ', ReportWriter ) 
    LinesWithFortranCInteroperability = CCPPReader.LinesWithFortranInteroperability()
    Scanner.WriteFileNamesAndLinesToReport( 
      'C/Fortran interoperability.' , LinesWithFortranCInteroperability, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXXVII' , 'C/C++ interoperability: ', ReportWriter ) 
    LinesWithCandCPPCInteroperability = CCPPReader.LinesWithCandCPPInteroperability()
    Scanner.WriteFileNamesAndLinesToReport( 
      'C/C++ interoperability.' , LinesWithCandCPPCInteroperability, ReportWriter, None  )

    Scanner.WriteItemHeader( 
      'XXXVIII' , 'C/C++ large, hard-coded array (i.e. poor use of memory, use NEW or MALLOC): ', ReportWriter ) 
    LinesWithoutDynamicMemoryAllocation = CCPPReader.LinesWithHardCodedArraysThatShouldUseNewOrAlloc()
    Scanner.WriteFileNamesAndLinesToReport( 
      'C/C++ arrays that should be dynamically allocated with MALLOC or NEW' , LinesWithoutDynamicMemoryAllocation, ReportWriter, None  )
    #CCPPReader.LinesWithPointerInitializationWithoutNULL()

    Scanner.WriteItemHeader( 
      'XXXIX' , 'reporting of C/C++ usage of alloc(),malloc() in which the allocation is checked for success or not (e.g. == NULL): ', ReportWriter ) 
    LinesCheckingForAllocationSuccess = CCPPReader.LinesWithMallocWithoutCheckingForSuccess()
    Scanner.WriteFileNamesAndLinesToReport( 
      'C/C++ POINTERs to memory blocks, checking alloc(),malloc() for success' , LinesCheckingForAllocationSuccess, ReportWriter, None  )
    ReportWriter.close()
    #CCPPReader.LinesWithPointerInitializationWithoutNULL()

def usage(): 
		
  print(''' 
    ----------------------------------------------------
    NOAA Office of Satellite and Product Operations OSPO
    CodeScanner 1.0.1
    ----------------------------------------------------

    Contact Information
    @author(s)   : Gerasimos Michalitsianos, Scientific Programmer II
    @company     : NOAA/SGT, Inc.
    @email       : gerasimos.michalitsianos@noaa.gov
    @phone       : 301-683-3267
    Last Updated : 20 December 2019 

    --------
    PURPOSE:
    --------

    Over the years, numerous sets of scientific codes have been
    submitted to NOAA's Office of Satellite Products and Operations (OSPO).
    These codes may have come from NOAA's Center for Satellite Applications
    and Research (STAR), or from elsewhere. With numerous sets of scientific
    codes in a variety of programming and/or scripting languages comes the
    need for a code-scanning tool to quickly find deficiencies or potential
    errors or issues in NOAA scientific source codes and report those in
    an organized manner.
  
    ------------
    DESCRIPTION: 
    ------------

    This tool uses the Python programming language as "glue" to bring
    together a variety of pre-existing code-scanning tools
    (i.e. cppcheck, shellcheck, and so on), as well as custom-developed
    Python functions, classes, and methods, to scan source-code files
    and generate a user-friendly report.

    ------
    USAGE:
    ------ 

    To use this script, just execute on the command-line with the
    desired or relevant source code directory passed-in: 
	
     $ python codescanner.py 
       -v, --version          : Print the version number of this software. 
       -d, --directory        : Run this software on an input source code directory. 
       -h, --help             : Print this help message. 
       -o, --output-directory : Pass in an optional output directory to write code-scanning results to.  	
  ''') 
  sys.exit(1) 

def printVersion(): 
  print(''' 
    CodeScanner Version 1.0.0
    @author        : Gerasimos Michalitsianos
    @title         : Scientific Programmer II
    @Organization  : NOAA/OSPO
    @email         : gerasimos.michalitsianos@noaa.gov
    @phone         : 301-503-8968
  ''')
  sys.exit(1) 

def main(): 

  # --------------------------------------------
  # specify input directory and output directory 
  # as empty strings to start out with 
  # --------------------------------------------

  inputDirectory='' 
  outputDirectory=''
  global LastUpdated
  global verbose
  LastUpdated = '7 January 2021' 

  # ---------------------------------------------------------------
  # get input directory as command-line argument
  #   -h, --help             : show help message
  #   -v, --version          : show version of this software 
  #   -o, --output-directory : output directory (optional)
  #   -d, --directory        : directory to be scanned (required)
  # ---------------------------------------------------------------
  
  parser = argparse.ArgumentParser(description='for scanning source code directories.')
  parser.add_argument('-d','--directory',required=False,type=str,
    dest='inputDirectory',help='input directory to be scanned.')
  parser.add_argument('-v','--version',required=False,
    dest='version',help='show version.',action='store_true')
  parser.add_argument('-o','--output-directory',required=False,
    type=str,dest='outputDirectory',help='specify output directory.')
  parser.add_argument('-u','--usage',required=False,
    dest='showhelp',action='store_true',help='show help message.')
  args = parser.parse_args()

  version   = args.version
  showHelp  = args.showhelp
  inputDir  = args.inputDirectory
  outputDir = args.outputDirectory 

  # -----------------------------------------
  # if user wants to see version info, do it.
  # -----------------------------------------
  if version:
    printVersion()

  # --------------------------------------------
  # if user wants to see usage message, show it.
  # --------------------------------------------
  if showHelp:
    usage()

  # -------------------------------------------
  # if number of input arguments less than two, 
  # then print a message saying this tool needs 
  # more arguments, then printing the usage 
  # message 
  # -------------------------------------------
  if len(sys.argv)<2: 
    print 
    print('    \n This program needs at least one command line argument: an input directory with source code. Exiting ... ')
    usage() 

  # --------------------------------------------------------
  # if we've made it to this point, then the user did not 
  # ask for the version (-v,--version) or the help message 
  # (-h,--help), so we assume that the user passed-in a 
  # valid source-code directory to be scanned. If they did
  # not, raise an error message. 
  # --------------------------------------------------------

  if inputDir == '': 
    print 
    print( '    \n User must pass in a valid source-code input directory to be scanned using the -d or --directory flags. ')
    usage()

  # --------------------------------------------------------
  # if the output directory was not specified, then just set 
  # it equal to the input directory 
  # --------------------------------------------------------
  if outputDir is None: 
    outputDir = inputDir

  # ---------------------------------------------------
  # make sure both input directory and output directory 
  # are indeed exisiting directories in the filesystem 
  # ---------------------------------------------------
  if not os.path.isdir(inputDir):
    print 
    print( '    \n Not a directory (input directory). Please use full path: '  , inputDir )
    print  
    sys.exit(1) 

  if not os.path.isdir(outputDir): 
    print 
    print( '    \n Not a directory (output directory). Please use full path: ' , outputDir )
    print 
    sys.exit(1) 

  # ---------------------------------------------------------
  # create a new object, which has a method to run the entire 
  # code-scanning tool on the input directory. Create output 
  # report of code-scanning results found. 
  # ---------------------------------------------------------
  obj = directoryscanner(inputDir,outputDir) 
  obj.GenerateMasterReport()

if __name__ == '__main__': 
  main() 
