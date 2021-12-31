(**

  This module defines a Open Tools API DLL for adding, editing and removing Custom HTML Help from
  the IDE.

  @Author  David Hoyle
  @Version 1.101
  @Date    11 Sep 2021

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

  @nocheck EmptyBeginEnd

**)
Library TPIDEHelp;

{$R 'ITHVerInfo.res' 'ITHVerInfo.RC'}
{$R 'TPIDEHelpImages.res' 'TPIDEHelpImages.rc'}

uses
  System.SysUtils,
  System.Classes,
  TPIDEHelp.Wizard in 'Source\TPIDEHelp.Wizard.pas',
  TPIDEHelp.SplashScreen in 'Source\TPIDEHelp.SplashScreen.pas',
  TPIDEHelp.Functions in 'Source\TPIDEHelp.Functions.pas',
  TPIDEHelp.AboutBox in 'Source\TPIDEHelp.AboutBox.pas',
  TPIDEHelp.ResourceStrings in 'Source\TPIDEHelp.ResourceStrings.pas',
  TPIDEHelp.AddinOptions in 'Source\TPIDEHelp.AddinOptions.pas',
  TPIDEHelp.IDEOptionsFrame in 'Source\TPIDEHelp.IDEOptionsFrame.pas' {frameTPIDEHelpOptions: TFrame},
  TPIDEHelp.Interfaces in 'Source\TPIDEHelp.Interfaces.pas',
  TPIDEHelp.HelpEntryForm in 'Source\TPIDEHelp.HelpEntryForm.pas' {frmHelpEntry},
  TPIDEHelp.ToolsAPIFunctions in 'Source\TPIDEHelp.ToolsAPIFunctions.pas',
  TPIDEHelp.CustomHelpList in 'Source\TPIDEHelp.CustomHelpList.pas',
  TPIDEHelp.UpdateCustomHelpActions in 'Source\TPIDEHelp.UpdateCustomHelpActions.pas',
  TPIDEHelp.ParentFrame in 'Source\TPIDEHelp.ParentFrame.pas' {fmTPIDEHelparentFrame: TFrame};

{$R *.res}

{$INCLUDE 'Source\CompilerDefinitions.inc'}
{$INCLUDE 'Source\LibrarySuffixes.inc'}

Begin
End.
