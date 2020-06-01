(**
  
  This module contains code to create a splash screen entry on the RAD Studio IDE splash screen.

  @Author  David Hoyle
  @Version 1.099
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
Unit TPIDEHelp.SplashScreen;

Interface

Type
  (** A record to encapsulate all of the splash screen methods. **)
  TTPHelpSplashScreen = Record
    Class Procedure AddSplashScreenItem(); Static;
  End;
  
Implementation

Uses
  ToolsAPI,
  System.SysUtils,
  VCL.Forms,
  WinAPI.Windows,
  TPIDEHelp.Functions,
  TPIDEHelp.ResourceStrings;

(**

  This method adds a splash screen entry to the RAD Studio IDE splash screen.

  @precon  None.
  @postcon The entry is added to the splashs screen.

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
  bmSplashScreen : HBITMAP;
  recVersionInfo : TTPHVersionInfo;

Begin
  bmSplashScreen := LoadBitmap(hInstance, strTPHelpSplashScreen);
  TTPHelpFunction.BuildNumber(recVersionInfo);
  If Supports(SplashScreenServices, IOTASplashScreenServices, SSS) Then
    SSS.AddPluginBitmap(
      Format(str3rdPartyIDEHelpFor, [recVersionInfo.FMajor, recVersionInfo.FMinor,
        strRevisions[Succ(recVersionInfo.FBugFix)], Application.Title]),
      bmSplashScreen,
      {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
      Format(strSplashScreenBuild, [recVersionInfo.FMajor, recVersionInfo.FMinor, recVersionInfo.FBugFix,
        recVersionInfo.FBuild])
    );
End;

End.
