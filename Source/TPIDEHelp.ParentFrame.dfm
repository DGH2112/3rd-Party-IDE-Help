object fmTPIDEHelparentFrame: TfmTPIDEHelparentFrame
  Left = 0
  Top = 0
  Width = 491
  Height = 580
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object lblBADI: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 485
    Height = 29
    Align = alTop
    Caption = '3rd Party IDE Help'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    ExplicitWidth = 224
  end
  object lblAuthor: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 60
    Width = 485
    Height = 16
    Align = alTop
    Caption = 'Author'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 38
  end
  object lblBuild: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 38
    Width = 485
    Height = 16
    Align = alTop
    Caption = 'lblBuild'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 40
  end
  object lblPleaseSelect: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 561
    Width = 485
    Height = 16
    Align = alBottom
    Caption = 'Please select a sub-options category...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitWidth = 220
  end
  object lblBuildDate: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 82
    Width = 485
    Height = 16
    Align = alTop
    Caption = 'lblBuildDate'
    ExplicitWidth = 66
  end
  object lblInformation: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 104
    Width = 485
    Height = 451
    Align = alClient
    Lines.Strings = (
      '')
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
