(**

  This module contains a class which implements the INTAAddinOptions interface so that we can add an
  options frame to the RAD Studio IDE options dialogue.

  @Author  David Hoyle
  @Version 1.276
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
Unit TPIDEHelp.AddinOptions;

Interface

Uses
  ToolsAPI,
  VCL.Forms,
  TPIDEHelp.Interfaces;

Type
  (** A class which implements the INTAAddinOptions interface for adding a frame to the IDEs options
      dialogue. **)
  TTPHelpAddinOptions = Class(TInterfacedObject, IUnknown, INTAAddInOptions)
  Strict Private
    FFrameClass        : TCustomFrameClass;
    FFrame             : TCustomFrame;
    FUpdateHelpActions : ITPHelpUpdateHelpAction;
    FCaption           : String;
  Strict Protected
    Procedure DialogClosed(Accepted: Boolean);
    Procedure FrameCreated(AFrame: TCustomFrame);
    Function  GetArea: String;
    Function  GetCaption: String;
    Function  GetFrameClass: TCustomFrameClass;
    Function  GetHelpContext: Integer;
    Function  IncludeInIDEInsight: Boolean;
    Function  ValidateContents: Boolean;
  Public
    Constructor Create(Const CustomFrameCls : TCustomFrameClass; Const strCaption : String;
      Const UpdateHelpActions : ITPHelpUpdateHelpAction);
  End;

Implementation

Uses
  System.SysUtils;

(**

  A constructor for the TTPHelpAddinOptions class.

  @precon  None.
  @postcon Stores a reference to the Update Help Action so they can be rebuilt is the options change.

  @param   CustomFrameCls    as a TCustomFrameClass as a constant
  @param   strCaption        as a String as a constant
  @param   UpdateHelpActions as an ITPHelpUpdateHelpAction as a constant

**)
Constructor TTPHelpAddinOptions.Create(Const CustomFrameCls : TCustomFrameClass;
  Const strCaption : String; Const UpdateHelpActions: ITPHelpUpdateHelpAction);

Begin
  Inherited Create;
  FFrameClass := CustomFrameCls;
  FCaption := strCaption;
  FUpdateHelpActions := UpdateHelpActions;
End;

(**

  This method is called then the IDE Options dialogue is called.

  @precon  None.
  @postcon Saves the changes in the option frame is the dialogue was accepted.

  @nocheck MissingCONSTInParam

  @param   Accepted as a Boolean

**)
Procedure TTPHelpAddinOptions.DialogClosed(Accepted: Boolean);

Var
  OpsFrame : ITPHelpOptionsFrame;

Begin
  If Accepted Then
    If Supports(FFrame, ITPHelpOptionsFrame, OpsFrame) Then
      Begin
        OpsFrame.FinaliseFrame;
        FUpdateHelpActions.CreateCustomHelpActions;
      End;
End;

(**

  This method is called when the IDE creates the options frame.

  @precon  None.
  @postcon Store the frame reference for later use.

  @nocheck MissingCONSTInParam

  @param   AFrame as a TCustomFrame

**)
Procedure TTPHelpAddinOptions.FrameCreated(AFrame: TCustomFrame);

Var
  OpsFrame : ITPHelpOptionsFrame;

Begin
  FFrame := AFrame;
  If Supports(FFrame, ITPHelpOptionsFrame, OpsFrame) Then
    OpsFrame.InitialiseFrame;
End;

(**

  This is a getter method for the Area property.

  @precon  None.
  @postcon Returns a null string to tell the IDE to put the below Caption under the 3rd Party items.

  @return  a String

**)
Function TTPHelpAddinOptions.GetArea: String;

Begin
  Result := '';
End;

(**

  This is a getter method for the Caption property.

  @precon  None.
  @postcon Returns the caption of the IDE options page in the left hand treeview under Third Party.

  @return  a String

**)
Function TTPHelpAddinOptions.GetCaption: String;

Begin
  Result := FCaption;
End;

(**

  This is a getter method for the Frame Class property.

  @precon  None.
  @postcon returns the class of the Frame the IDE should create for our options.

  @return  a TCustomFrameClass

**)
Function TTPHelpAddinOptions.GetFrameClass: TCustomFrameClass;

Begin
  Result := FFrameClass;
End;

(**

  This is a getter method for the Help Context property.

  @precon  None.
  @postcon Returns a help context of zero for no help context.

  @return  an Integer

**)
Function TTPHelpAddinOptions.GetHelpContext: Integer;

Begin
  Result := 0;
End;

(**

  This method returns true to signify that the contents of the options frame should be available
  in the IDE Insight dialogue.

  @precon  None.
  @postcon The contents of the frame are available in IDE Insight

  @return  a Boolean

**)
Function TTPHelpAddinOptions.IncludeInIDEInsight: Boolean;

Begin
  Result := True;
End;

(**

  This method is called to validate the options frame.

  @precon  None.
  @postcon We returns true to signify that the frame is validated.

  @return  a Boolean

**)
Function TTPHelpAddinOptions.ValidateContents: Boolean;

Begin
  Result := True;
End;

End.
