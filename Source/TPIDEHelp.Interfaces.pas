(**
  
  This module contains interfaces to be used through the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Oct 2018
  
**)
Unit TPIDEHelp.Interfaces;

Interface

Uses
  System.Classes;

Type
  (** An interface to be implemented by the IDE options frame. **)
  ITPHelpOptionsFrame = Interface
  ['{8AB2B3FE-0129-45FA-BD3C-32A986C07D33}']
    Procedure InitialiseFrame;
    Procedure FinaliseFrame;
  End;

  (** An interface for triggering the creation / updating of the Custom Help Actions. **)
  ITPHelpUpdateHelpAction = Interface
  ['{B9636877-EA44-47A3-91CF-6166DB0F9C4D}']
    Procedure CreateCustomHelpActions;
  End;

  (** An interface for working with the list of Custom Help Items. **)
  ITPHCustomHelpList = Interface
  ['{812816E8-3199-469A-86E4-FF457D5E9675}']
    Function  GetUsedHelpNames : TStringList;
    Function  GetFilename(Const strName : String) : String;
    Function  GetCount : Integer;
    Function  GetName(Const iIndex : Integer): String;
    Procedure Add(Const strName, strFileName : String);
    Procedure Update(Const strOldName, strNewName, strFileName : String);
    Procedure Delete(Const strName : String);
    (**
      This method returns a string list of IDE Help names already used.
      @precon  None.
      @postcon A stringlist of used help names are returned.
      @return  a TStringList
    **)
    Property  UsedHelpNames : TStringList Read GetUsedHelpNames;
    (**
      This method returns the indexed xustom help filename.
      @precon  None.
      @postcon The filename of the indexed custom help item
      @param   strName as a String as a constant
      @return  a String
    **)
    Property  Filename[Const strName : String] : String Read GetFilename;
    (**
      This property returns the number of custom help files in the list (exc the IDEs).
      @precon  None.
      @postcon The number of custom help items in returned.
      @return  an Integer
    **)
    Property  Count : Integer Read GetCount;
    (**
      This property returns the indexed custom help name.
      @precon  iIndex must be between 0 and Count - 1.
      @postcon The name of the indexed custom help item.
      @param   iIndex as an Integer as a constant
      @return  a String
    **)
    Property  Name[Const iIndex : Integer] : String Read GetName;
  End;

Implementation

End.
