import os
import re
import sys
import subprocess
from itertools import groupby

class Scanner(object):

  # Define possible source code filename extensions 
  # for the following programming languages:
  #   (1) FORTRAN
  #   (2) Python 
  #   (3) Perl
  #   (4) C++
  #   (5) C
  # -----------------------------------------------

  AllFileExtensions = { 
    'Fortran' : [ 'f','F','f77','F77','for','FOR','FTN','ftn','pfo','f90','F90','f95','f95' ],
    'Python'  : [ 'py' ],
    'Perl'    : [ 'pl' ],
    'C++'     : [ 'C','c++','cc','cpp','cxx','pcc','h' ],
    'C'       : [ 'c', 'ec' , 'pgc' ],
    'CorC++'  : [ 'hpp','C','c++','cc','cpp','cxx','pcc','h', 'c', 'ec' , 'pgc', 'h' ],
    'Header' :  [ 'h' ]
  }

  # extensions for non-source code files
  # (files that should be removed)
  # ------------------------------------
  NonSourceCodeExtensions = [ '.txt','.nc',\
    '.bin','.dll','.a','.dat','.docx','.doc',\
    '.grib','.grb','.TXT','.hdf','.bufr','.mod','.o',\
    '.png','.pyc','.gif','.PNG','.GIF','.jpg','.cfg','.jpeg','.log']
  ShellScriptExtensions = [ '.bash','.sh','.zsh','.tcsh','.ksh','.ps1' ]

  def __init__(self,Directory):
    self.Directory = Directory

  def __del__(self):
    pass

  def close(self):
    self=None

  def GetDirectory(self):
    return self.Directory

  def GetSourceCodeFileNameExtensions(self):
    return Scanner.AllFileExtensions

  @staticmethod
  def RunShellCheck(SrcDirectory,OutputDirectory):
    OutNameShellCheck = os.path.join( OutputDirectory, 'ALL_SHELLCHECK.txt' )
    if os.path.isfile( OutNameShellCheck ): os.remove( OutNameShellCheck )
    ShellCheckWriter = open( OutNameShellCheck, 'w' )

    for Root,Directories,Files in os.walk(SrcDirectory):
      Path = Root.split(os.sep)
      for FileName in Files:
        FullPathName = os.path.join( Root, FileName )
        Extension = os.path.splitext( FullPathName )[1]
        if Extension in Scanner.ShellScriptExtensions and not os.path.islink( FullPathName ): 
          ShellCheckCmd='shellcheck '+FullPathName
          Lines=Scanner.RunCommand( ShellCheckCmd )
          for Line in Lines: 
            Line=Line.strip()
            ShellCheckWriter.write('%s\n'%Line)
    ShellCheckWriter.close()

  @staticmethod
  def ScrubDirectory(Directory):
    ExtensionsToIgnore = Scanner.NonSourceCodeExtensions
    for Root,Directories,Files in os.walk(Directory):
      Path = Root.split(os.sep)
      for FileName in Files:
        FullPathName = os.path.join( Root, FileName )

        if not 'codeScannerReport.txt' in FullPathName:
          Extension = os.path.splitext( FullPathName )[1]
          if Extension in ExtensionsToIgnore:
            os.remove( FullPathName )

  def IsSourceCodeFile( self,FileName,Language=None ):
    if Language is None:
      ExtensionDict = self.GetSourceCodeFileNameExtensions()
    else:
      ExtensionDict = { Language : self.GetSourceCodeFileNameExtensions()[Language] }

    FileNameExtension = os.path.splitext( FileName )[1]
    for FileType,ExtensionList in iter(ExtensionDict.items()):
      for Extension in ExtensionList:
        if '.'+Extension == FileNameExtension: return True
    return False

  def GetSourceCodeDirectoryFileNames(self,Language=None):
    FileNames = []
    for SubDir,SubDirs,FileList in os.walk( self.Directory ):
      for FileName in FileList:
        if self.IsSourceCodeFile( FileName, Language ):
          FileNames.append( os.path.abspath( os.path.join( SubDir, FileName )))
    return FileNames

  def GetSourceCodeDirectoryFileNamesAndLines(self,Language=None):
    '''function GetSourceCodeDirectoryFileNames(Language=None):
    This function returns either all source filenames and their
    contents as a Python dictionary{} in the source code directory,
    or only those for a specific language.
    '''
    FileNamesAndLines = {}
    for SubDir,SubDirs,FileList in os.walk( self.Directory ):
      for FileName in FileList:
        if self.IsSourceCodeFile( FileName, Language ):
          FullFileNamePath = os.path.abspath( os.path.join( SubDir, FileName ))
          FileNamesAndLines[ FullFileNamePath ] = [ 
            Line.rstrip() for Line in open( FullFileNamePath, 'r').readlines() ]
    return FileNamesAndLines

  def SearchFileNamesWithRegex(self,Pattern,SourceFileType=None,IgnoreCase=True):

    # initialize dict{} with filenames as keys 
    # and an empty list for holding lines and line numbers
    # for expression found
    # ----------------------------------------------------
    FileNames = self.GetSourceCodeDirectoryFileNames( SourceFileType )
    OutSearchResults = {}
    for FileName in FileNames: 
      OutSearchResults[ FileName ] = list() 
 
    # Iterate through relevant Code source files based on 
    # SourceFileType flag (i.e. Fortran, Perl, ... )
    # ----------------------------------------------------

    for FileName in FileNames: 

      # Get lines from source code file. Replace any comment
      # lines with the empty string ''
      # ------------------------------------------------------

      LinesWithComments = open(FileName,'r').readlines()
      LinesWithoutComments = Scanner.RemoveCommentsFromFile( FileName )
      LinesWithoutCommentsFilled = Scanner.FillCommentLines( 
        LinesWithComments, LinesWithoutComments)
      
      LineNumber=1
      for Line in LinesWithoutCommentsFilled:
        
        # Remove any preceding or trailing whitespace characters
        # from the source code line.
        # (if we are NOT regexing for leading tabs though) 
        # ------------------------------------------------------
        Line=Line.rstrip()
       
        # Perform regular expression (regex) search. 
        # ------------------------------------------

        if IgnoreCase: 
          SearchResults = re.search( Pattern, Line , re.IGNORECASE )
        else:        
          SearchResults = re.search( Pattern, Line ) 
    
        if SearchResults is not None:
          OutSearchResults[ FileName ].append('Line '+str(LineNumber)+': '+Line )

        LineNumber+=1

    SearchResults={}
    for FileName,LineList in iter(OutSearchResults.items()):
      if len(LineList)>0: SearchResults[ FileName ] = LineList 
    return SearchResults

  @staticmethod
  def WriteLines( Lines, FileWriter , Indent=None):
    if Indent is None:
      Indent=''
    for Line in Lines:
      FileWriter.write('%s\n'%(Indent+Line))

  @staticmethod
  def RunCommand(Command):
    OutLines=[]
    Proc = subprocess.Popen( Command, shell=True, stdout=subprocess.PIPE,stderr=subprocess.PIPE )
    ProcLines = Proc.stdout.readlines()
    for Line in ProcLines:
      OutLines.append( Line.rstrip() )
    return OutLines

  @staticmethod
  def WriteFileNamesToReport( Message,FileNames,ReportWriter ):
    if len(FileNames)<1:
      ReportWriter.write('%s\n'%('   No instances found for: '+Message ))
      ReportWriter.write('%s\n'%' ')
    else:
      FileCounter=1
      for FileName in FileNames:
        ReportWriter.write('%s\n'%('  '+str(FileCounter)+'. '+FileName.rstrip() ))
        FileCounter+=1

  @staticmethod
  def WriteFileNamesAndLinesToReport( Message,FileLinesDict,ReportWriter,MaxFiles,LineLimitPerFile=None ):
    if len(FileLinesDict)<1:
      ReportWriter.write('%s\n'%('   No instances found for: '+Message ))
    else:
      FileNumber=1
      for FileName,LineSet in iter(FileLinesDict.items()):
        ReportWriter.write('%s\n'%('  '+str(FileNumber)+'. '+FileName))
        LineCounter=0
        for Line in LineSet:
          ReportWriter.write('%s\n'%('    '+Line.strip()))
          if LineLimitPerFile is not None and LineCounter>LineLimitPerFile: 
            ReportWriter.write('%s\n'%'    ... ')
            break
          LineCounter+=1
        FileNumber+=1
        if MaxFiles is not None:
          if FileNumber>MaxFiles:
            ReportWriter.write('%s\n'%' ')
            ReportWriter.write('%s\n'%'   ... ')
            ReportWriter.write('%s\n'%' ')
            ReportWriter.write('%s\n'%('  '+' ... (more instances in other source files.)'))
            ReportWriter.write('%s\n'%' ')
            ReportWriter.write('%s\n'%'   ... ')
            ReportWriter.write('%s\n'%' ')
            break

  @staticmethod
  def WriteItemHeader(ItemNumber,Message,ReportWriter):
    ''' 
    This function, WriteItemHeader(), is a static (class) method that writes
    an item header to the output CodeScanner report. In other words, it writes
    out the title of a section that covers one of the code-review items from 
    the Word Document or Spreadsheet. It will write the item number 
    (if applicable) as well as a short one-line description, along with 
    strings that look like "-----" that delineate the header from its 
    content. This function is called numerous times to write and organize
    the output CodeScanner report, helping to delineate the sections. 

    For example, this function might be called with: 
      Scanner.WriteItemHeader(34,"instances of IMPLICIT NONE not found" , reportWriterObj)

    Args: 
      int: item number, if applicable. 
      str: message. A short description of the code-review item. Phrase. 
      reportWriter: object: CodeScanner file-writer object. Output from Python's open() method.  
    Returns: 
      None: But does write lines to output CodeScanner report via reportWriter object. 
    '''
    BarrierString = '-'*(len(str(ItemNumber))+len(Message)+2)
    ReportWriter.write('%s\n'%' ')
    ReportWriter.write('%s\n'%(' '+BarrierString))
    ReportWriter.write('%s\n'%(' '+'('+str(ItemNumber)+') '+Message))
    ReportWriter.write('%s\n'%(' '+BarrierString))
    ReportWriter.write('%s\n'%' ')

  @staticmethod
  def RemoveCommentsFromFile( FileName ):
    '''
    A static method to remove comment-lines from a source-code file. 
    This is done using the Count Lines of Code (CLOC) command-line tool, 
    which should be installed on the system in which CodeScanner is being 
    run. This method returns those lines of a source, in a Python list[], 
    file, which are not comments. 
 
    Args: 
      str (FileName): Input filename string (full path) for a source-code file. 
    Returns: 
      list: Python list[] of lines from a source file not including comment-lines. 
    '''
    cmd = 'cloc '+FileName+' --strip-comments --original-dir > temp.txt'
    os.system(cmd)
    os.remove('temp.txt')
    outname = os.path.basename(FileName)+'.--original-dir'
    if os.path.isfile( outname ):
      lines=open(outname,'r').readlines()
      outlines=[]
      for line in lines: outlines.append(line.rstrip())
      os.remove(outname)
      return outlines
    else:
      return open( FileName, 'r').readlines()

  @staticmethod
  def FillCommentLines(CommentedLines, NonCommentedLines, FillerString=None):
    ''' 
    This function takes in a list of source-code lines, some of 
    which may be comments. It then creates a new list of source-code 
    lines with these comment lines replaced by blank lines (i.e. "")
    or a line with an optional fillerString (passed-in). This function 
    hence removes comment lines so they may be ignored in future 
    parsing. This function is important such that instead of just 
    removing the comment-lines, and replacing them with blank strings, 
    we may still get accurate line-numbers later on when reporting results. 

    Args: 
      list: commentedLines, a list of source-code lines. 
      list: nonCommentedLines, a list of source-code lines with comment-lines stripped out. 
      str: optional string to replace comment line with. Default is empty string "". 
    Returns:  
      list: List[] of source-code lines with comment-lines replaced with empty strings "".     
    '''
    NonCommentLinesFilled = []
    for Line in CommentedLines:
      Line=Line.rstrip()
      HasMatchingLine = False
      for NonCommentLine in NonCommentedLines:
        NonCommentLine=NonCommentLine.rstrip()
        if NonCommentLine == Line: HasMatchingLine = True

      if HasMatchingLine:
        NonCommentLinesFilled.append(Line)
      else:
        if FillerString is not None:
          NonCommentLinesFilled.append('          '+FillerString+'          ')
        else:
          NonCommentLinesFilled.append(' ')
    return NonCommentLinesFilled

  @staticmethod
  def MergeScanningDictResults(Dict1,Dict2):

    # initialize output dictionary{} to hold merged results.
    OutDict={}

    # get list[] of ALL files from both result dictionaries{}
    AllFileNameKeys = list(Dict1.keys())+list(Dict2.keys()) # list[] of all filenames 

    # fill-in output dictionary with all filename keys
    for FileNameKey in AllFileNameKeys:
      OutDict[ FileNameKey ] = list()
    for FileNameKey,LineSet in iter(Dict1.items()):
      for Line in LineSet:
        OutDict[FileNameKey].append( Line )

    for FileNameKey,LineSet in iter(Dict2.items()):
      for Line in LineSet:
        OutDict[FileNameKey].append( Line )
    return OutDict

  def GetLinesWithFloatComparison(self):
    FirstSet = self.SearchFileNamesWithRegex( 
      r'(?:!=|/=|==|.eq.|.ne.)[ \t]*\d+\.\d+' , None )
    SecondSet = self.SearchFileNamesWithRegex( 
      r'\d+\.\d+[ \t]*(?:!=|/=|==|.eq.|.ne.)(.*)' , None )
    MergedResults=Scanner.MergeScanningDictResults( FirstSet,SecondSet )
    return MergedResults

  def GetLinesWithLeadingTabs(self):
    return self.SearchFileNamesWithRegex( r'^\t' , None )

  def GetLinesWithHardCodedIPs(self):
    return self.SearchFileNamesWithRegex( r'[0-9]+(?:\.[0-9]+){3}' , None )

  def GetLinesWithHardCodedPaths(self):
    return self.SearchFileNamesWithRegex( 
      r'^[^#]*\/[a-zA-Z0-9._-]+\/[a-zA-Z0-9._-]+\/[a-zA-Z0-9._-]+'      , None )

  def GetLinesWithHardCodedPathsInOpenStatement(self):
    return self.SearchFileNamesWithRegex( 
      r'(.*)(?:open|fopen)[ \t]*\([ \t]*[\"\']+' , None )

  def GetLinesWithPossibleHardCodedCredentials(self):
    return self.SearchFileNamesWithRegex( 
      r'\b(?:password|username|user|passwd|token|authorization|passcode)\b' , None )

  def GetLinesWithHardCodedNumbers(self):
    InitialResultsHardCodedNumbers = self.SearchFileNamesWithRegex( 
      r'[ \t]*([-+]?[0-9]*\.?[0-9]+[\/\+\-\*])+([-+]?[0-9]*\.?[0-9]+)' , None )
    FilteredResults = {}
    for FileName in InitialResultsHardCodedNumbers.keys(): 
      FilteredResults[FileName] = list()

    for FileName,LineSet in iter(InitialResultsHardCodedNumbers.items()):
      for Line in LineSet:
        if re.search( r'=[ \t]*[`\"\']+', Line ) is None \
          and not Line.split(':')[1].strip().startswith('"') and not Line.split(':')[1].strip().startswith('#'):
            FilteredResults[FileName].append( Line )
    return {FileName:LineList for FileName,LineList in FilteredResults.items() if LineList}

  def LinesWithMagicNumbersAnyLanguage(self):
    # look for lines with operators < , > , <=, => , etc. and with number greater than 9
    return self.SearchFileNamesWithRegex( r'(?:if|for|else|elif|while)(.*)[<>=]+[ \t]*(?:[1-9][0-9]+|9)' , None  )

  def GetLinesWithSystemCalls(self):
    return self.SearchFileNamesWithRegex( r'(.*)system[ \t]*\((.*)\)' , None )

  def GetLinesWithPossibleDivisionByZero(self):
    return self.SearchFileNamesWithRegex( r'/0+(?:\.?0*)?(?!\d)' , None )

  @staticmethod
  def FilterLinesCLOC( Lines ):
    OutLines = []
    Append   = False
    for Line in Lines:
      Line=Line.strip()
      Line=Line.decode('utf-8')
      if Line.startswith('----'):
        Append = True
      if Append:
        OutLines.append(Line)
    return OutLines

  def SourceFilesWithLongLines(self):

    ResultsDictLongLines = {}
    SourceFileNamesAndLines = self.GetSourceCodeDirectoryFileNamesAndLines()
    for FileName in SourceFileNamesAndLines.keys():ResultsDictLongLines[FileName]=list()

    for FileName,LineSet in iter(SourceFileNamesAndLines.items()):
      OriginalLines = LineSet
      NonCommentedLines = Scanner.RemoveCommentsFromFile(FileName)
      NonCommentedLinesFilled = Scanner.FillCommentLines( 
        OriginalLines , NonCommentedLines , ' THIS IS A COMMENT LINE ' )
      LineNumber=1
      for Line in NonCommentedLinesFilled:
        LineStripped=Line.strip()
        if len(LineStripped)>120:
          ResultsDictLongLines[ FileName ].append('Line '+str(LineNumber)+': '+Line )
        LineNumber+=1
    OutDict={}
    for FileName,LineSet in iter(ResultsDictLongLines.items()):
      if len(LineSet)>0: OutDict[FileName ] = LineSet
    return OutDict

  def SourceFilesWithoutHeader(self,ReportWriter):
    ''' 
    This function reports those source files in the input source-code 
    directory that may not have a proper header. It does this by looking 
    for a repeating sequences of comment lines in the first 60 lines of 
    the source file. This sequence has to be at least 5 lines long for 
    it to be a likely header. 

    Args: 
      ReportWriter (file): Open file object for writing CodeScanner report.
    Returns: 
      None: But does write results to output CodeScanner report. 
    '''
    SourceFileNamesAndLines = self.GetSourceCodeDirectoryFileNamesAndLines()
    LineLimit = 100
    FileNamesWithoutHeader=[]
    FoundFileWithoutHeader=False

    for FileName,LineSet in iter(SourceFileNamesAndLines.items()):

      OriginalLines = LineSet
      NonCommentedLines = Scanner.RemoveCommentsFromFile(FileName)
      NonCommentedLinesFilled = Scanner.FillCommentLines( 
        OriginalLines , NonCommentedLines , ' THIS IS A COMMENT LINE ' )
      IsComment=[]

      LineCounter=1
      for Line in NonCommentedLinesFilled:
        if 'THIS IS A COMMENT LINE' in Line:
          IsComment.append(1)
        else:
          IsComment.append(0)
        LineCounter+=1
        if LineCounter>LineLimit: break

      GroupedEntries = [(val, sum(1 for index in group)) for val,group in groupby(IsComment)]
      RepeatedPatternVals = [ x[1] for x in GroupedEntries if x[0]!=0 ]
      if len(RepeatedPatternVals)>0:
        if max(RepeatedPatternVals)<5:
          FoundFileWithoutHeader=True
          FileNamesWithoutHeader.append(FileName)
    Scanner.WriteFileNamesToReport( 'Source files that may lack full HEADER or PROLOGUE' ,FileNamesWithoutHeader,ReportWriter )     
