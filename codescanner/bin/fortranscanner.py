import os
import sys
import re
from scanner import Scanner

class FortranScanner(Scanner):

  def __init__(self,*args,**kwargs):
    super(FortranScanner,self).__init__( *args, **kwargs )

  def FilesWithoutImplicitNone(self):
    FilesWithoutImplicitNone=[]
    for FileName in self.GetSourceCodeDirectoryFileNames('Fortran'):

      # define TRUE or FALSE for if SOURCE 
      # file contains IMPLICIT NONE 
      # -----------------------------------
      HasImplicitNone = False

      # Get Lines from FORTRAN File. Replace 
      # comment-lines with empty strings. 
      # ------------------------------------
      FortranFileContents = '\n'.join( Scanner.RemoveCommentsFromFile( 
        FileName 
      )).lower()

      # Look for IMPLICIT NONE in string.
      # This includes any number of spaces or tabs
      # between "implicit" and "none" 
      # ------------------------------------------
      pattern = r'implicit[ \t]+none'
      SearchResults = re.findall( pattern, FortranFileContents )
      if len(SearchResults)<1 and 'module' not in FortranFileContents.lower():
        FilesWithoutImplicitNone.append( FileName )
    return FilesWithoutImplicitNone

  def InstancesOfImplicitVariableUse(self):
    return self.SearchFileNamesWithRegex( r'^(?!.[\s\S]*none).[\s\S]*implicit.*$', 'Fortran' )

  def InstancesOfEquivalence(self):
    return self.SearchFileNamesWithRegex( r'^[ \t]*equivalence*' , 'Fortran'  )

  def InstancesOfCommonBlock(self):
    return self.SearchFileNamesWithRegex( r'^[ \t]*common*' , 'Fortran' )

  def InstancesOfInclude(self):
    return self.SearchFileNamesWithRegex( r'^[ \t]*include*' , 'Fortran'  )
  
  def InstancesOfNumberedLoops(self):
    return self.SearchFileNamesWithRegex( r'^(.*)do[ \t]+[0-9]+' , 'Fortran'  )
    #return self.SearchFileNamesWithRegex( r'^(.*)do[ \t]+\d' , 'Fortran'  )

  def InstancesOfPrintStatements(self):
    #return self.SearchFileNamesWithRegex( r'[ \t]*print[ \t]*^[^*](.*),' , 'Fortran'  )
    return self.SearchFileNamesWithRegex( r'^[ \t]*print[ \t]*' , 'Fortran'  )

  def InstancesOfWriteStatements(self):
    return self.SearchFileNamesWithRegex( r'^[ \t]*write[ \t]*\(' , 'Fortran'  )
  
  def InstancesOfCallExitStatements(self):
    return self.SearchFileNamesWithRegex( r'(.*)call[ \t]*exit' , 'Fortran'  )

  def InstancesOfDeclarationsWithoutKind(self):
    return self.SearchFileNamesWithRegex( r'^[ \t]*(?:real|integer)[ \t]*\,' , 'Fortran'  )

  def InstancesOfFortranGoTo(self):
    return self.SearchFileNamesWithRegex( r'(.*)go[ \t]*to(.*)*' , 'Fortran' )

  def InstancesOfPointerDeclarationsWithoutNull(self):
    return self.SearchFileNamesWithRegex( r'^[ \t]*(?!.*null).*pointer.*$' , 'Fortran' )

  def GetLinesWithHardCodedArrayDimensions(self):
    return self.SearchFileNamesWithRegex( r'[ \t]*(?:real|integer|character)(.*)\(\d{2,}\)' , 'Fortran' )
