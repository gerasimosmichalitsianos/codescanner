import os
import sys
import re
from scanner import Scanner

class IDLScanner(Scanner):

  def __init__(self,*args,**kwargs):
    super(IDLScanner,self).__init__( *args, **kwargs )

  def InstancesOfIDLGoTo(self):
    return self.SearchFileNamesWithRegex( r'(.*)go[ \t]*to(.*)' , 'IDL' )

  def InstancesOfIDLOpen(self):
    return self.SearchFileNamesWithRegex( r'(.*)open(.*)' , 'IDL' )

  def InstancesOfIDLClose(self):
    return self.SearchFileNamesWithRegex( r'(.*)close(.*)*' , 'IDL' )

  def InstancesOfIDLLoopsWithoutN_elements(self):
    return self.SearchFileNamesWithRegex( r'(.*)(?:for|while)(.*)do(.*)', 'IDL' )
