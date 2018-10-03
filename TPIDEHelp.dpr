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
  TPIDEHelp.ResourceStrings in 'Source\TPIDEHelp.ResourceStrings.pas';

{$R *.res}

{$INCLUDE 'Source\CompilerDefinitions.inc'}
{$INCLUDE 'Source\LibrarySuffixes.inc'}

Begin
End.
