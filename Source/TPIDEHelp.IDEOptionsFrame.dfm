object frameTPIDEHelpOptions: TframeTPIDEHelpOptions
  Left = 0
  Top = 0
  Width = 550
  Height = 329
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    550
    329)
  object btnAdd: TButton
    Left = 241
    Top = 301
    Width = 98
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Add'
    ImageIndex = 0
    ImageMargins.Left = 5
    Images = ilButtons
    TabOrder = 0
    OnClick = btnAddClick
  end
  object btnEdit: TButton
    Left = 345
    Top = 301
    Width = 98
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Edit'
    ImageIndex = 1
    ImageMargins.Left = 5
    Images = ilButtons
    TabOrder = 1
    OnClick = btnEditClick
  end
  object btnDelete: TButton
    Left = 449
    Top = 301
    Width = 98
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Delete'
    ImageIndex = 2
    ImageMargins.Left = 5
    Images = ilButtons
    TabOrder = 2
    OnClick = btnDeleteClick
  end
  object lvHelp: TListView
    Left = 3
    Top = 3
    Width = 544
    Height = 292
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Item'
        Width = 100
      end
      item
        AutoSize = True
        Caption = 'Filename'
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
    OnSelectItem = lvHelpSelectItem
  end
  object ilButtons: TImageList
    Left = 24
    Top = 240
    Bitmap = {
      494C010103000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF00000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000020A0000040E0000000FF000000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000808080000020A0000060E0000040E0000020
      A000000080000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000080000000800000008000FFFFFF00000000000020A0000060E0000000
      FF00000080000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF00FFFFFF008000000080000000800000008000000080000000FFFF
      FF00FFFFFF0000008000FFFFFF00FFFFFF0000000000000000000020A0000060
      E0000020A0000000800000000000000000000000000000000000000000000000
      0000000000000000FF0000008000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF008000000080000000FFFFFF00FFFFFF008000000080000000FFFF
      FF00FFFFFF0000008000FFFFFF00FFFFFF000000000000000000000000000020
      A0000040E0000000800000000000000000000000000000000000000000000000
      00000000FF000000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008000000080
      000000800000008000000080000000FF000000FF000000800000008000000080
      0000008000000080000000800000008000000000000000000000C0C0C000FFFF
      FF00FFFFFF008000000080000000FFFFFF00FFFFFF008000000080000000FFFF
      FF00FFFFFF0000008000FFFFFF00FFFFFF000000000000000000000000000000
      00000020A0000000FF0000008000000000000000000000000000000000000000
      FF00000080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000FF0000008000000000000000000000C0C0C000FFFF
      FF00FFFFFF00FFFFFF008000000080000000FFFFFF008000000080000000FFFF
      FF00FFFFFF0000008000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000020A0000000FF000000800000000000000000000000FF000000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF000000FF0000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0080000000800000008000000080000000FFFF
      FF00FFFFFF0000008000FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000020A0000000FF00000080000000FF00000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF008000000080000000FFFFFF00FFFFFF008000000080000000FFFF
      FF00FFFFFF0000008000FFFFFF00FFFFFF000000000000000000000000000000
      00000000000000000000000000000020A0000000FF0000008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF00FFFFFF0080000000800000008000000080000000FFFFFF00FFFF
      FF00FFFFFF0000008000FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000020A0000000FF00000080000020A000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000008000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000020A0000000FF000000800000000000000000000020A0000000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000080000000800000008000FFFFFF000000000000000000000000000020
      A0000020A0000000FF0000008000000000000000000000000000000000000020
      A000000080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000800000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000000000000020A0000020A0000060
      E0000000FF000000800000000000000000000000000000000000000000000000
      00000020A0000000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000020A0000060E0000040E0000000
      FF00000080000000000000000000000000000000000000000000000000000000
      0000000000000020A00000008000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000060E0000040E0000000FF000000
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000020A0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FF3FFFFF9FFF0000FE3FFFFF0FFF0000
      FE3FC00007FE0000FE3FC00087FD0000FE3FC000C3F90000FE3FC000E3F30000
      C000C000F1E700008000C000F8CF00008001C000FC1F0000FE3FC000FE3F0000
      FE3FC000FC1F0000FE3FC000F8CF0000FE3FC000E1E70000FE3FC00083F30000
      FE7FFFFF07F90000FFFFFFFF0FFE000000000000000000000000000000000000
      000000000000}
  end
end
