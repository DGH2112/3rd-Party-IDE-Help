(**

  This module contains a frame for the root node of the Options frame in the IDE.

  @Author  David Hoyle
  @Version 1.185
  @Date    01 Jun 2020

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
Unit TPIDEHelp.ParentFrame;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

Type
  (** A class to represent the options frame. **)
  TfmTPIDEHelparentFrame = Class(TFrame)
    lblBADI: TLabel;
    lblAuthor: TLabel;
    lblBuild: TLabel;
    lblPleaseSelect: TLabel;
    lblBuildDate: TLabel;
    lblInformation: TMemo;
  Strict Private
  Strict Protected
  Public
    Constructor Create(AOwner : TComponent); Override;
  End;

Implementation

{$R *.dfm}

Uses
  TPIDEHelp.Functions;

(**

  This method intialises the frame with build information.

  @precon  None.
  @postcon The frame is initialised.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TfmTPIDEHelparentFrame.Create(AOwner: TComponent);

Const
  strBugFix = ' abcdefghijklmnopqrstuvwxyz';
  str3rdPartyIDEHelp = '3rd Party IDE Help %d.%d%s';
  {$IFDEF DEBUG}
  strDEBUGBuild = 'DEBUG Build %d.%d.%d.%d';
  {$ELSE}
  strBuild = 'Build %d.%d.%d.%d';
  {$ENDIF}
  strBuildDateFmt = 'ddd dd/mmm/yyyy hh:nn';
  strBuildDate = 'Build Date: %s';
  strAuthor = 'Author: David Hoyle (c)  2020 GNU GPL 3';
  strInformation =
    '3rd Party Help is a RAD Studio IDE plug-in that allows you to easily install additional 3rd ' +
    'Party HTML Help files into the IDE to provide context sensitive help for libraries and ' +
    'components.'#13#10 +
    ''#13#10 +
    'Copyright (C) 2020  David Hoyle '#13#10 +
    '(https://github.com/DGH2112/3rd-Party-IDE-Help)'#13#10 +
    ''#13#10 +
    'This program is free software: you can redistribute it and/or modify it under the terms of the ' +
    'GNU General Public License as published by the Free Software Foundation, either version 3 of ' +
    'the License, or (at your option) any later version.'#13#10 +
    ''#13#10 +
    'This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; ' +
    'without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See ' +
    'the GNU General Public License for more details.'#13#10 +
    ''#13#10 +
    'You should have received a copy of the GNU General Public License along with this program.  If ' +
    'not, see <https://www.gnu.org/licenses/>.';

Var
  VerInfo : TTPHVersionInfo;
  dtDate : TDateTime;
  strModuleName : String;
  iSize : Integer;

Begin
  Inherited Create(AOwner);
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadSettings', tmoTiming);{$ENDIF}
  TTPHelpFunction.BuildNumber(VerInfo);
  lblBADI.Caption := Format(str3rdPartyIDEHelp, [VerInfo.FMajor, VerInfo.FMinor,
    strBugFix[Succ(VerInfo.FBugFix)]]);
  {$IFDEF DEBUG}
  lblBuild.Caption := Format(strDEBUGBuild, [VerInfo.FMajor, VerInfo.FMinor, VerInfo.FBugFix,
    VerInfo.FBuild]);
  lblBuild.Font.Color := clRed;
  {$ELSE}
  lblBuild.Caption := Format(strBuild, [VerInfo.iMajor, VerInfo.iMinor, VerInfo.iBugFix,
    VerInfo.iBuild]);
  {$ENDIF}
  SetLength(strModuleName, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strModuleName), MAX_PATH);
  SetLength(strModuleName, iSize);
  FileAge(strModuleName, dtDate);
  lblBuildDate.Caption := Format(strBuildDate, [FormatDateTime(strBuildDateFmt, dtDate)]);
  lblAuthor.Caption := strAuthor;
  lblInformation.Lines.Text := strInformation;
  {$IFDEF EUREKALOG}
  lblEurekaLog.Caption := Format(strEurekaLogStatus, [
    BoolToStr(IsEurekaLogInstalled, True),
    BoolToStr(IsEurekaLogActive, True)
  ]);
  lblEurekaLog.Font.Color := clGreen;
  {$ELSE}
  {$ENDIF}
End;

End.

