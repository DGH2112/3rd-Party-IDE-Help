(**
  
  This module contains functions for use throughout the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Oct 2018
  
**)
Unit TPIDEHelp.Functions;

Interface

Type
  (** A record to describe the version information in the plug-in. **)
  TTPHVersionInfo = Record
    FMajor, FMinor, FBugFix, FBuild : Integer;
  End;
  (** A record to encapsulate all the general methods used throughout the application. **)
  TTPHelpFunction = Record
    Class Procedure BuildNumber(Var recVersionInfo : TTPHVersionInfo); Static;
  End;

Const
  (** A constant string to represent the revision numbers (bug fixes) of the application. **)
  strRevisions = ' abcedfghijklmnopqrstuvwxyz';


Implementation

Uses
  Windows;

(**

  This method returns the version information for the plug-in.

  @precon  None.
  @postcon The version information for the plug-in is returned.

  @param   recVersionInfo as a TTPHVersionInfo as a reference

**)
Class Procedure TTPHelpFunction.BuildNumber(Var recVersionInfo : TTPHVersionInfo);

Const
  iShiftRight16 = 16;
  iWordMask = $FFFF;

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  strModuleName: String;
  iSize: Cardinal;

Begin
  SetLength(strModuleName, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strModuleName), MAX_PATH);
  SetLength(strModuleName, iSize);
  recVersionInfo.FMajor := 0;
  recVersionInfo.FMinor := 0;
  recVersionInfo.FBugFix := 0;
  recVersionInfo.FBuild := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(strModuleName), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      Try
        GetFileVersionInfo(PChar(strModuleName), 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        recVersionInfo.FMajor := VerValue^.dwFileVersionMS Shr iShiftRight16;
        recVersionInfo.FMinor := VerValue^.dwFileVersionMS And iWordMask;
        recVersionInfo.FBugFix := VerValue^.dwFileVersionLS Shr iShiftRight16;
        recVersionInfo.FBuild := VerValue^.dwFileVersionLS And iWordMask;
      Finally
        FreeMem(VerInfo, VerInfoSize);
      End;
    End;
End;

End.
