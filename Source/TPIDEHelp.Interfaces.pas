(**
  
  This module contains interfaces to be used through the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Oct 2018
  
**)
Unit TPIDEHelp.Interfaces;

Interface

Type
  (** An interface to be implemented by the IDE options frame. **)
  ITPHelpOptionsFrame = Interface
  ['{8AB2B3FE-0129-45FA-BD3C-32A986C07D33}']
    Procedure InitialiseFrame;
    Procedure FinaliseFrame;
  End;

Implementation

End.
