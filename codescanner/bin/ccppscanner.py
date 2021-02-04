import os
import sys
import re
from scanner import Scanner
from pyparsing import nestedExpr

class CCPPScanner( Scanner ):

  def __init__(self,*args,**kwargs):
    super(CCPPScanner,self).__init__( *args, **kwargs )

  def InstancesOfOperatorOverloading(self):
    return self.SearchFileNamesWithRegex( r'operator[ \t]*[&|,]+' , 'C++'  )

  def InstancesOfCastingPointers(self):
    #return self.SearchFileNamesWithRegex( r'[ \t]*(.*)=[ \t]*\((?:int|double|float|char)[ \t]*\*\)[ \t]*&' , 'CorC++'  )
    return self.SearchFileNamesWithRegex( r'[ \t]*(.*)\=[ \t]*\((?:int|double|float|char)[ \t]*\*\)[ \t]*' , 'CorC++'  )

  def LinesWithMagicNumbers(self,Language):
    # look for lines with operators < , > , <=, => , etc. and with number greater than 9
    return self.SearchFileNamesWithRegex( r'(?:if|for|else|while)(.*)[<>=]+[ \t]*(?:[1-9][0-9]+|9)' , Language  )

  def LinesWithTooMuchPointerIndirection(self):
    return self.SearchFileNamesWithRegex(  r'(.*)=(.*)\*\*\*\*' , 'CorC++' )

  def LinesWithFortranInteroperability(self):
    return self.SearchFileNamesWithRegex(  r'[ \t]*(.*)_\(' , 'CorC++' )

  def LinesWithCandCPPInteroperability(self):
    return self.SearchFileNamesWithRegex(  r'extern[ \t]*\"C\"' , 'CorC++' )

  def LinesWithMallocWithoutCheckingForSuccess(self):
    Results    = self.SearchFileNamesWithRegex( r'(.*)=(.*)alloc', 'CorC++' )
    OutResults = {}
    for FileName,Lines in Results.items():
      Msgs=[]
      for line in Lines : 
        try:
          LineNumber        = line.split(':')[0].replace('Line ','').strip()
          LineNumber        = int(LineNumber)
          AllocatedVariable = line.split(':')[1].strip().split('=')[0].strip()
          SrcLines          = open( FileName,'r').readlines()

          AllocationLine = '   ... WARNING: Variable may not have been checked for allocation and may be NULL ... '
          for SrcLine in SrcLines:
            SrcLine=SrcLine.strip()
            if AllocatedVariable in SrcLine and 'if' in SrcLine and ( 'NULL' in SrcLine or 'nullptr' in SrcLine ):
              AllocationLine = '    check for allocation success: ' + SrcLine
          AllocationMsg = line+'\n  '+AllocationLine
          Msgs.append(AllocationMsg)
        except:
          pass
      OutResults[FileName]=Msgs
    return OutResults

  def LinesWithHardCodedArraysThatShouldUseNewOrAlloc(self):
    InitialLines = self.SearchFileNamesWithRegex(  r'[ \t]*(?:int|float|double)[ \t]*\[(.*)[0-9]{1,}(.*)\]' , 'CorC++' )
    FilteredResults = {}
    for f,lines in iter(InitialLines.items()):
      filtered_lines=[]
      for line in lines:
        if 'new' in line or 'malloc' in line:continue
        filtered_lines.append(line)
      if len(filtered_lines)>0:
        FilteredResults[f]=filtered_lines
    return FilteredResults

  def LinesWithHardCodedArrayDimensions(self):
    # char * CharCoverageType = new char[64];
    return self.SearchFileNamesWithRegex(  r'(.*)new(.*)\[[ \t]*[0-9]+[ \t]*\](.*)' , 'CorC++' )

  def LinesWithNullAndNotNullPtr(self):
    return self.SearchFileNamesWithRegex( r'(.*)=[ \t]*NULL[ \t]*;' , 'C++'  )

  def LinesWithPointerArithmetic(self):
    return self.SearchFileNamesWithRegex( r'^[ \t]*\*[ \t]*\((.*)[+-]+[ \t]*' , 'CorC++'  )

  def InstancesOfNewWithoutDelete(self,CreateKeyword,ReleaseKeyword,FileType):
    LinesWithNew = self.SearchFileNamesWithRegex( r'[ \t]*=[ \t]*'+\
      re.escape(CreateKeyword)+r'[ \t]*(?:int|float|char|double|struct|union)' , FileType  )
    DictLinesWithoutDelete = {}
    for Key in LinesWithNew.keys(): 
      DictLinesWithoutDelete[Key] = list()

    for FileName,Lines in iter(LinesWithNew.items()):
      for Line in Lines:
        if '=' in Line:
          VariableName = list(filter(None,Line.split('=')[0].split(' ')))[-1].replace('*','')
          CPPReader=open(FileName,'r')
          CPPLines=CPPReader.readlines()
          CPPReader.close()
          LacksDelete=True
          for CPPLine in CPPLines:
            if VariableName in CPPLine and ReleaseKeyword in CPPLine: 
              LacksDelete=False
          if LacksDelete:
            DictLinesWithoutDelete[FileName].append(Line)
   
    OutDict={}
    for FileName,Lines in iter(DictLinesWithoutDelete.items()):
      if len(Lines)>0: 
        OutDict[FileName] = Lines
    return OutDict 

  def GetFileNamesCCPP(self):
    SourceFileNamesC   = self.GetSourceCodeDirectoryFileNames('C')
    SourceFileNamesCPP = self.GetSourceCodeDirectoryFileNames('C++')
    return SourceFileNamesC + SourceFileNamesCPP 

  def SwitchStatementsWithoutDefault(self,ReportWriter,ItemNumberString):

    Scanner.WriteItemHeader( ItemNumberString , 'C/C++ instances of SWITCH{ } statements without DEFAULT: ', \
      ReportWriter )
    FileNames=self.GetFileNamesCCPP()
    FileCounter=1
    FoundSwitchNoDefault=False

    for FileName in FileNames:

      FileReader=open(FileName,'r')
      Contents=FileReader.read()
      FileReader.close()

      SwitchStatementsNoDefault=[]
      OccurencesOfSwitch = [ s.start() for s in \
        re.finditer( r'switch[ \t\n]*\(', Contents ) ]

      if len(OccurencesOfSwitch)>0:

        for SwitchStartIndex in OccurencesOfSwitch:

          SwitchStatement = Contents[ SwitchStartIndex: ]
          CharCounter=0
          EndBrace=0

          for Char in SwitchStatement[ SwitchStatement.find('{'): ]:
            if Char == '{': CharCounter+=1
            if Char == '}': CharCounter-=1
            if CharCounter == 0: break
            EndBrace+=1

          SwitchStatementChars = SwitchStatement[ 0:EndBrace+SwitchStatement.find('{')+50  ]
          if 'default' not in SwitchStatementChars: 
            SwitchStatementsNoDefault.append( SwitchStatementChars )

      if len(SwitchStatementsNoDefault)>0:
        FoundSwitchNoDefault=True
        ReportWriter.write('%s\n'%('  '+str(FileCounter)+'. '+FileName))
        for SwitchStatement in SwitchStatementsNoDefault:
          ReportWriter.write('%s\n'%'')
          #ReportWriter.write(SwitchStatement.replace('\n','      \n'))
          for SwitchStatementLine in SwitchStatement.split('\n'):
            SwitchStatementLine=SwitchStatementLine.rstrip()
            ReportWriter.write('%s\n'%('        '+SwitchStatementLine))
          ReportWriter.write('%s\n'%'')
          ReportWriter.write('%s\n'%'           ... ... ')
          ReportWriter.write('%s\n'%'')
        FileCounter+=1

    if not FoundSwitchNoDefault:
      ReportWriter.write('%s\n'%('   No instances found for SWITCH{ } STATEMENTS WITHOUT DEFAULT.'))

  def CCPPDefinedConstructors(self):

    # Initialize output dictionary{} that will hold C/C++ source
    # filenames as keys, and values an empty list that will hold
    # source lines with deconstructors, constructors, 
    # assignment operators, and assignment operators for C++. 
    # ----------------------------------------------------------   

    ClassInfoDict = {}
    CCPPFileNames = self.GetFileNamesCCPP()
    for FileName in CCPPFileNames:
      ClassInfoDict[ FileName ] = list()

    for FileName in CCPPFileNames:

      # Get only non-comment lines from C/C++ source file
      # -------------------------------------------------
      LinesWithComments = open(FileName,'r').readlines()
      LinesWithoutComments = Scanner.RemoveCommentsFromFile( FileName )
      LinesWithoutCommentsFilled = Scanner.FillCommentLines(
        LinesWithComments, LinesWithoutComments)
   
      # check to see if source file has a class declaration   
      # ---------------------------------------------------

      HasClass  = False
      ClassName = ''
      for Line in LinesWithoutCommentsFilled:
        Line=Line.strip()
        if re.search( r'^[ \t]*class[ \t]+', Line ) is not None:
          HasClass=True
          ClassName=list(filter(None,Line.strip().split(' ')))[1]

      if HasClass:
        ClassInfoDict[ FileName ].append('ISCLASS')
        LineNumber=1 
        for Line in LinesWithoutCommentsFilled:

          # Look for lines using regular expressions , for the following:
          #   (1) Copy constructor
          #   (2) Deconstructor
          #   (3) Assignment Operator
          #   (4) Constructor
          # -------------------------------------------------------------
          if re.search( r'[ \t]*'+re.escape(ClassName)+r'[ \t]*\([ \t]*const(.*)&' , Line ) is not None:
            ClassInfoDict[ FileName ].append('Line '+str(LineNumber)+' (CLASS COPY CONSTRUCTOR METHOD): '+Line )
          if re.search( r'~[ \t]*([ \t]*)' , Line ) is not None:
            ClassInfoDict[ FileName ].append('Line '+str(LineNumber)+' (CLASS DECONSTRUCTOR METHOD): '+Line )
          if re.search( r'[ \t]*'+re.escape(ClassName)+r'(.*)operator=' , Line ) is not None:
            ClassInfoDict[ FileName ].append('Line '+str(LineNumber)+' (ASSIGNMENT OPERATOR METHOD): '+Line )
          if re.search( r'[ \t]*^[^~]*'+re.escape(ClassName)+r'[ \t]*\([ \t]*(.*)*' , Line ) is not None:
            ClassInfoDict[ FileName ].append('Line '+str(LineNumber)+' (CONSTRUCTOR AND/OR COPY CONSTRUCTOR METHOD): '+Line )
          LineNumber+=1

    OutContentClassDict={}    
    for FileName,ClassLines in iter(ClassInfoDict.items()):
      if len( ClassLines )<10 and 'ISCLASS' in ClassLines: 
        ClassLines.remove( 'ISCLASS' )
        OutContentClassDict[ FileName ] = ClassLines
    return OutContentClassDict
 
  def CCPPInstancesOfDeepNesting(self):
    ''' 
    function CCPPInstancesOfDeepNesting(): 
    This instance method CCPPInstancesOfDeepNesting() iterates
    through the relevant C/C++ source files and reports 
    those filenames that may potentially have deep nesting. 
    No line numbers are reported here. Just the filename. 
    The method here is simple. All the characters in the 
    source file are iterated through one by one. If the 
    character is an opening or closing curly brace (usually 
    used to enclose blocks, { or }), then the character is 
    appended to a growing string. In the end, if the string 
    has a repeating sequence of {{{{{{ or }}}}}} , then it 
    is fairly likely there is some deep-nesting going on 
    within the C/C++ source file. 

    Args:
      None: This is an instance method. 
    Returns:  
      None: Writes all output to the text-file report for the currently running instance of CodeScanner. 
    '''

    # first get list of all C/C++ source files 
    # to iterate through 
    # ---------------------------------------- 
    RelevantFileNamesCCPP  = self.GetFileNamesCCPP()

    # define the characters for opening 
    # or closing curly braces in a Python list[]
    # ------------------------------------------
    TestCurlyBraceChars = ['{','}']

    # define initially empty list 
    # of source files that may 
    # contain deep-nesting. 
    # --------------------------
    FileNamesWithPossibleDeepNesting = []

    # --------------------------------------------
    # Iterate through all C/C++ source files 
    # in the package, and initialize empty string. 
    # Loop through all characters in source file. 
    # If character is opening or closing brace, 
    # append the character. If '{{{{{{' or '}}}}}}' 
    # is found in the appended string, add the 
    # filename to the list defined above. 
    # --------------------------------------------

    for FileName in RelevantFileNamesCCPP:
      CCPPSrcCode = open( FileName,'r').read()
      TestString=''
      for Char in CCPPSrcCode:
        if Char in TestCurlyBraceChars: TestString += Char
      if '{{{{{{' in TestString or '}}}}}}' in TestString:
        FileNamesWithPossibleDeepNesting.append( FileName )
    return FileNamesWithPossibleDeepNesting 
