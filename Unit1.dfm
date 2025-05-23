object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 540
  ClientWidth = 789
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 65
    Width = 789
    Height = 264
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 789
    Height = 65
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    ExplicitTop = -6
    object lbTempo: TLabel
      Left = 8
      Top = 40
      Width = 40
      Height = 13
      Caption = 'lbTempo'
    end
    object Button1: TButton
      Left = 0
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Executar'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Memo2: TMemo
    Left = 0
    Top = 329
    Width = 789
    Height = 211
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=FB'
      'Database=C:\MFX\Dados\MFX.FDB'
      'Password=masterkey'
      'Port=3050'
      'Protocol=TCPIP'
      'Server=localhost'
      'User_Name=SYSDBA')
    LoginPrompt = False
    Left = 664
    Top = 160
  end
end
