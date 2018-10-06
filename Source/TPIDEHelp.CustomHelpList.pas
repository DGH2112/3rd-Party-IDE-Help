(**
  
  This module encapsulates the Custom Help list for use through the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Oct 2018
  
**)
Unit TPIDEHelp.CustomHelpList;

Interface

Uses
  System.Classes,
  System.Generics.Collections,
  TPIDEHelp.Interfaces;

Type
  (** A class whch implemets the ITPHCustomHelpList interface for managing th list of Custom IDE Helps
      files. **)
  TTPHCustomHelpList = Class(TInterfacedObject, ITPHCustomHelpList)
  Strict Private
    Type
      (** An enumerate for identifying the type of help item. **)
      TTPHHelpType = (htEbmt, ht3rdParty, htDelete);
      (** A record to describe the help items in the registry. **)
      TTPHHelpRecord = Record
        FOldName  : String;
        FNewName  : String;
        FFilename : String;
        FHelpType : TTPHHelpType;
        Constructor Create(Const strName, strFilename : String; Const eHelpType : TTPHHelpType);
      End;
  Strict Private
    FCustomHelp    : TList<TTPHHelpRecord>;
    FUsedHelpNames : TStringList;
  Strict Protected
    // ITPHCustomHelpList
    Function  GetUsedHelpNames: TStringList;
    Function  GetFilename(Const strName: String): String;
    Function  GetCount : Integer;
    Function  GetName(Const iIndex : Integer) : String;
    Procedure Add(Const strName: String; Const strFilename: String);
    Procedure Update(Const strOldName, strNewName, strFileName : String);
    Procedure Delete(Const strName: String);
    Procedure SaveListChanges;
    // General Methods
    Procedure LoadCustomHelp;
    Procedure SaveCustomHelp;
    Function  GetIDEVersionNum(Const strBDSDir : String) : String;
    Function  GetIDERegPoint() : String;
    Function  Find(Const strName  :String) : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  System.SysUtils,
  System.Win.Registry,
  System.RegularExpressions;

Const
  (** A constant for the registry location of the IDEs 3rd party help. **)
  strHelpRegKey = 'Software\Embarcadero\%s\%s\Help';
  (** A constant for the custom 2td party HTML file entries. **)
  strHTMLHelpFiles = 'HTMLHelp1Files';
  (** A constant for the RAD Studio IDE base directory environment variable. **)
  strBDSEnviroVar = 'BDS';

(**

  A constructor for the THelpRecord class.

  @precon  None.
  @postcon Initalises the record.

  @param   strName     as a String as a constant
  @param   strFilename as a String as a constant
  @param   eHelpType   as a TTPHHelpType as a constant

**)
Constructor TTPHCustomHelpList.TTPHHelpRecord.Create(Const strName, strFilename : String;
  Const eHelpType : TTPHHelpType);

Begin
  FOldName  := strName;
  FNewName  := strName;
  FFilename := strFilename;
  FHelpType := eHelpType;
End;

(**

  This method adds a third party help item to the collection.

  @precon  None.
  @postcon The third party help is added to the aollection - no checks are made to ensure it is unique.

  @param   strName     as a String as a constant
  @param   strFilename as a String as a constant

**)
Procedure TTPHCustomHelpList.Add(Const strName, strFilename: String);

Begin
  FCustomHelp.Add(TTPHHelpRecord.Create(strName, strFilename, ht3rdParty));
End;

(**

  A constructor for the TTPHCustomHelpList class.

  @precon  None.
  @postcon Creates a generic collection for the Custom Help and a list of IDE Help names.

**)
Constructor TTPHCustomHelpList.Create;

Begin
  Inherited Create;
  FCustomHelp := TList<TTPHHelpRecord>.Create;
  FUsedHelpNames := TStringList.Create;
  LoadCustomHelp;
End;

(**

  This method marks the named custom help for deletion.

  @precon  None.
  @postcon The custom help (if found) is marked fo deletion.

  @param   strName as a String as a constant

**)
Procedure TTPHCustomHelpList.Delete(Const strName: String);

Var
  iIndex: Integer;
  R: TTPHHelpRecord;

Begin
  iIndex := Find(strName);
  If iIndex > -1 Then
    Begin
      R := FCustomHelp[iIndex];
      R.FHelpType := htDelete;
      FCustomHelp[iIndex] := R;
    End;
End;

(**

  A destructor for the TTPHCustomHelpList class.

  @precon  None.
  @postcon Frees the memory used by the list.

**)
Destructor TTPHCustomHelpList.Destroy;

Begin
  FUsedHelpNames.Free;
  FCustomHelp.Free;
  Inherited Destroy;
End;

(**

  This method attempts to return the index of the named 3rd part help file.

  @precon  None.
  @postcon the index into the collection is returned for the named third party help file.

  @param   strName as a String as a constant
  @return  an Integer

**)
Function TTPHCustomHelpList.Find(Const strName: String): Integer;

Var
  iHelp: Integer;

Begin
  Result := - 1;
  For iHelp := 0 To FCustomHelp.Count - 1 Do
    If CompareText(strName, FCustomHelp[iHelp].FNewName) = 0 Then
      Begin
        Result := iHelp;
        Break;
      End;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the count of the third party custom help files ONLY (exc IDE help).

  @return  an Integer

**)
Function TTPHCustomHelpList.GetCount: Integer;

Var
  iHelp : Integer;
  
Begin
  Result := 0;
  For iHelp := 0 To FCustomHelp.Count - 1 Do
    If FCustomHelp[iHelp].FHelpType = ht3rdParty Then
      Inc(Result);
End;

(**

  This is a getter method for the Filename property.

  @precon  None.
  @postcon Attempts to returns the filename associated with the given named third party custom help file.

  @param   strName as a String as a constant
  @return  a String

**)
Function TTPHCustomHelpList.GetFilename(Const strName: String): String;

Var
  iIndex: Integer;

Begin
  Result := '';
  iIndex := Find(strName);
  If iIndex > -1 Then
    Result := FCustomHelp[iIndex].FFilename;
End;

(**

  This method searches the IDEs command line parameters for an alternate registration point (-rXxxxx)
  and returns that alternate point instead of the standard BDS if found.

  @precon  None.
  @postcon Returns the activty IDEs registration point.

  @return  a String

**)
Function TTPHCustomHelpList.GetIDERegPoint: String;

Const
  strDefaultRegPoint = 'BDS';
  iSwitchLen = 2;

Var
  iParam: Integer;

Begin
  Result := strDefaultRegPoint;
  For iParam := 1 To ParamCount Do
    If CompareText(Copy(ParamStr(iParam), 1, iSwitchLen), '-r') = 0 Then
      Begin
        Result := ParamStr(iParam);
        System.Delete(Result, 1, iSwitchLen);
        Break;
      End;
End;

(**

  This method returns the IDEs version number from the end of the BDS environment variable passed.

  @precon  None.
  @postcon the version number is returned.

  @param   strBDSDir as a String as a constant
  @return  a String

**)
Function TTPHCustomHelpList.GetIDEVersionNum(Const strBDSDir: String): String;

Var
  RegEx : TRegEx;
  M: TMatch;

Begin
  Result := '0.0';
  RegEx := TRegEx.Create('\d+\.\d', [roIgnoreCase, roCompiled, roSingleLine]);
  M := RegEx.Match(strBDSDir);
  If M.Success Then
    Result := M.Value;
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon This method attempts to returns the nae of the indexed third party help item.

  @param   iIndex as an Integer as a constant
  @return  a String

**)
Function TTPHCustomHelpList.GetName(Const iIndex: Integer): String;

Var
  i3rdThirdCount : Integer;
  iHelp: Integer;
  
Begin
  i3rdThirdCount := -1;
  For iHelp := 0 To FCustomHelp.Count - 1 Do
    Begin
      If FCustomHelp[iHelp].FHelpType = ht3rdParty Then
        Inc(i3rdThirdCount);
      If i3rdThirdCount = iIndex Then
        Begin
          Result := FCustomHelp[iHelp].FNewName;
          Break;
        End;
    End;
End;

(**

  This is a getter method for the UsedHelpNames property.

  @precon  None.
  @postcon returns a reference to a string list of IDE Help names NOT to be used.

  @return  a TStringList

**)
Function TTPHCustomHelpList.GetUsedHelpNames: TStringList;

Begin
  Result := FUsedHelpNames;
End;

(**

  This method loads the all the IDE help files from the regsitry marking them either IDE or 3rd Party.

  @precon  None.
  @postcon All the help file are loaded into the list.

**)
Procedure TTPHCustomHelpList.LoadCustomHelp;

Var
  R : TRegIniFile;
  slHelp: TStringList;
  iHelp: Integer;
  strBDSDir: String;
  strFileName: String;
  eHelpType: TTPHHelpType;

Begin
  strBDSDir := System.SysUtils.GetEnvironmentVariable(strBDSEnviroVar);
  R := TRegIniFile.Create(Format(strHelpRegKey, [GetIDERegPoint(), GetIDEVersionNum(strBDSDir)]));
  Try
    slHelp := TStringList.Create;
    Try
      R.ReadSection(strHTMLHelpFiles, slHelp);
      For iHelp := 0 To slHelp.Count - 1 Do
        Begin
          strFileName := R.ReadString(strHTMLHelpFiles, slHelp[iHelp], '');
          eHelpType := ht3rdParty;
          If CompareText(Copy(strFilename, 1, Length(strBDSDir)), strBDSDir) = 0 Then
            Begin
              eHelpType := htEbmt;
              FUsedHelpNames.Add(slHelp[iHelp]);
            End;
          FCustomHelp.Add(TTPHHelpRecord.Create(slHelp[iHelp], strFileName, eHelpType));
        End;
    Finally
      slHelp.Free;
    End;
  Finally
    R.Free;
  End;
End;

(**

  This method saves changes to the 3rd Party custom help files back to the registry.

  @precon  None.
  @postcon All changes to the third party custom help files are saved.

**)
Procedure TTPHCustomHelpList.SaveCustomHelp;

Var
  iHelp: Integer;
  strBDSDir: String;
  R: TRegIniFile;

Begin
  strBDSDir := System.SysUtils.GetEnvironmentVariable(strBDSEnviroVar);
  R := TRegIniFile.Create(Format(strHelpRegKey, [GetIDERegPoint(), GetIDEVersionNum(strBDSDir)]));
  Try
    For iHelp := 0 To FCustomHelp.Count - 1 Do
      Case FCustomHelp[iHelp].FHelpType Of
        htDelete: R.DeleteKey(strHTMLHelpFiles, FCustomHelp[iHelp].FOldName);
        ht3rdParty:
          If CompareText(FCustomHelp[iHelp].FOldName, FCustomHelp[iHelp].FNewName) <> 0 Then
            Begin
              R.DeleteKey(strHTMLHelpFiles, FCustomHelp[iHelp].FOldName);
              R.WriteString(strHTMLHelpFiles, FCustomHelp[iHelp].FNewName, FCustomHelp[iHelp].FFilename);
            End Else
          If CompareText(FCustomHelp[iHelp].FFilename,
            R.ReadString(strHTMLHelpFiles, FCustomHelp[iHelp].FOldName, '')) <> 0 Then
            R.WriteString(strHTMLHelpFiles, FCustomHelp[iHelp].FOldName, FCustomHelp[iHelp].FFilename);
      End;
  Finally
    R.Free;
  End;
End;

(**

  This method saves the changes made to the list of custom third party help file.

  @precon  None.
  @postcon The changes are saved back to the registry.

**)
Procedure TTPHCustomHelpList.SaveListChanges;

Begin
  SaveCustomHelp;
End;

(**

  This method updates an existing list item with a new name and filename.

  @precon  None.
  @postcon The existing item (if found) is updated.

  @param   strOldName  as a String as a constant
  @param   strNewName  as a String as a constant
  @param   strFilename as a String as a constant

**)
Procedure TTPHCustomHelpList.Update(Const strOldName, strNewName, strFilename : String);

Var
  iIndex : Integer;
  R : TTPHHelpRecord;
  
Begin
  iIndex := Find(strOldName);
  If iIndex > -1 Then
    Begin
      R := FCustomHelp[iIndex];
      R.FNewName := strNewName;
      R.FFilename := strFilename;
      FCustomHelp[iIndex] := R;
    End;
End;

End.
