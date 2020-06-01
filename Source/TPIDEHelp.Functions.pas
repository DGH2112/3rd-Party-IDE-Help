(**
  
  This module contains functions for use throughout the application.

  @Author  David Hoyle
  @Version 1.098
  @Date    30 May 2020

  @license

    3rd Party Help is a RAD Studio IDE plug-in that allows you to easily install
    additional 3rd Party HTML Help files into the IDE to provide context
    sensitive help for libraries and components.

    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Delphi-IDE-Explorer)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  WinAPI.Windows;

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
