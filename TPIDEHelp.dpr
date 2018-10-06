(**

  This module defines a Open Tools API DLL for adding, editing and removing Custom HTML Help from
  the IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Oct 2018

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
  TPIDEHelp.UpdateCustomHelpActions in 'Source\TPIDEHelp.UpdateCustomHelpActions.pas';

{$R *.res}

{$INCLUDE 'Source\CompilerDefinitions.inc'}
{$INCLUDE 'Source\LibrarySuffixes.inc'}

Begin
End.
