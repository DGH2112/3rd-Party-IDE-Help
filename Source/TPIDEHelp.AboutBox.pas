(**
  
  This module contains code for adding and remove an about box entry in the RAD Studio IDE.

  @Author  David Hoyle
  @Version 1.212
  @Date    15 Nov 2023

  @license

    3rd Party Help is a RAD Studio IDE plug-in that allows you to easily install
    additional 3rd Party HTML Help files into the IDE to provide context
    sensitive help for libraries and components.

    Copyright (C) 2023  David Hoyle (https://github.com/DGH2112/3rd-Party-IDE-Help)

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
Unit TPIDEHelp.AboutBox;

Interface

Type
  (** A record to encapsulate the about box functionality. **)
  TTPHelpAboutBox = Record
    Class Function  AddAboutBox() : Integer; Static;
    Class Procedure RemoveAboutBox(Const iAboutBoxIndex : Integer); Static;
  End;

Implementation

{$INCLUDE CompilerDefinitions.inc}

Uses
  ToolsAPI,
  System.SysUtils,
  VCL.Forms,
  {$IFDEF RS110}
  VCL.Graphics,
  {$ELSE}
  WinAPI.Windows,
  {$ENDIF RS110}
  TPIDEHelp.Functions,
  TPIDEHelp.ResourceStrings;

(**

  This method adds an about box entry to the RAD Studio IDE.

  @precon  None.
  @postcon The About Box entry is added to the IDE and its index is returned.

  @return  an Integer

**)
Class Function TTPHelpAboutBox.AddAboutBox: Integer;

Const
  strTPHelpSplashScreen = 'TPHelpSplashScreen48x48';

ResourceString
  strAboutBoxDescription = 'A RAD Studio plug-in to manage and access Third Party help.';

Var
  ABS : IOTAAboutBoxServices;
  {$IFDEF RS110}
  AboutBoxBitmap : TBitMap;
  {$ELSE}
  bmAboutBox : HBITMAP;
  {$ENDIF RS110}
  recVersionInfo: TTPHVersionInfo;
  
Begin
  Result := -1;
  TTPHelpFunction.BuildNumber(recVersionInfo);
  If Supports(BorlandIDEServices, IOTAAboutBoxServices, ABS) Then
    Begin
      {$IFDEF RS110}
      AboutBoxBitmap := TBitMap.Create();
      Try
        AboutBoxBitmap.LoadFromResourceName(hInstance, strTPHelpSplashScreen);
        Result := ABS.AddPluginInfo(
          Format(str3rdPartyIDEHelpFor, [recVersionInfo.FMajor, recVersionInfo.FMinor,
            strRevisions[Succ(recVersionInfo.FBugFix)], Application.Title]),
          strAboutBoxDescription,
          [AboutBoxBitmap],
          {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
          Format(strSplashScreenBuild, [recVersionInfo.FMajor, recVersionInfo.FMinor, recVersionInfo.FBugFix,
            recVersionInfo.FBuild])
        );
      Finally
        AboutBoxBitmap.Free;
      End;
      {$ELSE}
      bmAboutBox := LoadBitmap(hInstance, strTPHelpSplashScreen);
      Result := ABS.AddPluginInfo(
        Format(str3rdPartyIDEHelpFor, [recVersionInfo.FMajor, recVersionInfo.FMinor,
          strRevisions[Succ(recVersionInfo.FBugFix)], Application.Title]),
        strAboutBoxDescription,
        bmAboutBox,
        {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
        Format(strSplashScreenBuild, [recVersionInfo.FMajor, recVersionInfo.FMinor, recVersionInfo.FBugFix,
          recVersionInfo.FBuild])
      );
      {$ENDIF RS110}
    End;
End;

(**

  This method removes the About Box entry from the IDE.

  @precon  None.
  @postcon The About Box entry with the given index is removed from the IDE.

  @param   iAboutBoxIndex as an Integer as a constant

**)
Class Procedure TTPHelpAboutBox.RemoveAboutBox(Const iAboutBoxIndex : Integer);

Var
  ABS : IOTAAboutBoxServices;

Begin
  If iAboutBoxIndex > -1 Then
    If Supports(BorlandIDEServices, IOTAAboutBoxServices, ABS) Then
      ABS.RemovePluginInfo(iAboutBoxIndex);
End;

End.
