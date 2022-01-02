(**
  
  This module contains code to create a splash screen entry on the RAD Studio IDE splash screen.

  @Author  David Hoyle
  @Version 1.233
  @Date    31 Dec 2021

  @license

    3rd Party Help is a RAD Studio IDE plug-in that allows you to easily install
    additional 3rd Party HTML Help files into the IDE to provide context
    sensitive help for libraries and components.

    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/3rd-Party-IDE-Help)

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
Unit TPIDEHelp.SplashScreen;

Interface

Type
  (** A record to encapsulate all of the splash screen methods. **)
  TTPHelpSplashScreen = Record
    Class Procedure AddSplashScreenItem(); Static;
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

  This method adds a splash screen entry to the RAD Studio IDE splash screen.

  @precon  None.
  @postcon The entry is added to the splash screen.

**)
Class Procedure TTPHelpSplashScreen.AddSplashScreenItem();

Const
  {$IFDEF D2007}
  strTPHelpSplashScreen = 'TPHelpSplashScreen24x24';
  {$ELSE}
  strTPHelpSplashScreen = 'TPHelpSplashScreen48x48';
  {$ENDIF}

Var
  SSS : IOTASplashScreenServices;
  {$IFDEF RS110}
  SplashScreenBitMap : TBitMap;
  {$ELSE}
  bmSplashScreen : HBITMAP;
  {$ENDIF RS110}
  recVerInfo : TTPHVersionInfo;

Begin
  TTPHelpFunction.BuildNumber(recVerInfo);
  If Supports(SplashScreenServices, IOTASplashScreenServices, SSS) Then
    Begin
      {$IFDEF RS110}
      SplashScreenBitMap := TBitMap.Create;
      SplashScreenBitMap.LoadFromResourceName(hInstance, strTPHelpSplashScreen);
      SSS.AddPluginBitmap(
        Format(str3rdPartyIDEHelpFor, [recVerInfo.FMajor, recVerInfo.FMinor, strRevisions[Succ(recVerInfo.FBugFix)], Application.Title]),
        [SplashScreenBitMap],
        {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
        Format(strSplashScreenBuild, [recVerInfo.FMajor, recVerInfo.FMinor, recVerInfo.FBugFix, recVerInfo.FBuild])
      );
      {$ELSE}
      bmSplashScreen := LoadBitmap(hInstance, strTPHelpSplashScreen);
      SSS.AddPluginBitmap(
        Format(str3rdPartyIDEHelpFor, [recVerInfo.FMajor, recVerInfo.FMinor, strRevisions[Succ(recVerInfo.FBugFix)], Application.Title]),
        bmSplashScreen,
        {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
        Format(strSplashScreenBuild, [recVerInfo.FMajor, recVerInfo.FMinor, recVerInfo.FBugFix, recVerInfo.FBuild])
      );
      {$ENDIF RS110}
    End;
End;

End.
