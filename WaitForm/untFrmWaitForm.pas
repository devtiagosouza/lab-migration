unit untFrmWaitForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,Vcl.Imaging.GIFImg,
  Vcl.StdCtrls, System.NetEncoding;

type
  TFrmWait = class(TForm)
    Image1: TImage;
    lbMensagem: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    function DecodeBase64ToStream(const Base64: string): TMemoryStream;
  private
    GifStream: TMemoryStream;

    { Private declarations }
  public
     procedure Animate();
    { Public declarations }
  end;

var
  FrmWait: TFrmWait;

implementation

const Base64GIF: string =
    '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' +
    'jzz4rRDMHDfDAYo8FbSRaBww7oRRlFEAA==';


{$R *.dfm}

procedure TFrmWait.Animate;
var
    GIF: TGIFImage;
begin

  try
     GIF := TGIFImage.Create;
     GIF.LoadFromStream(GifStream);
     //GIF.LoadFromFile('C:\Imagens\wait2.gif');

     GIF.Animate := True;
     Image1.Picture.Graphic := GIF;
  except
    on E: Exception do
      ShowMessage('Erro ao carregar GIF: ' + E.Message);
  end;

end;

function TFrmWait.DecodeBase64ToStream(const Base64: string): TMemoryStream;
var
  Bytes: TBytes;
begin
  Result := TMemoryStream.Create;
  try
    Bytes := TNetEncoding.Base64.DecodeStringToBytes(Base64);
    Result.Write(Bytes[0], Length(Bytes));
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

procedure TFrmWait.FormCreate(Sender: TObject);

begin
  Image1.Picture.Assign(nil);
  BorderStyle := bsNone;

  GifStream := DecodeBase64ToStream(Base64GIF);

 // FormStyle := fsStayOnTop;
  Position := poScreenCenter;
  Color := clBlack;
  AlphaBlend := true;
  AlphaBlendValue := 250;

  WindowState := wsMaximized;

  Image1 := TImage.Create(Self);
  Image1.Parent := Self;



  Image1.Height := 105;
  Image1.Width := 97;
  Image1.Center := True;
  Image1.Transparent := true;


  //Animate;

end;

procedure TFrmWait.FormResize(Sender: TObject);
begin
  Image1.left := ( self.Width div 2 ) - ( Image1.Width div 2 );
  Image1.Top := ( self.Height div 2 ) - ( Image1.height div 2 );

  lbMensagem.Left := 0;
  lbMensagem.Top := Image1.Top + Image1.Height + 10;
  lbMensagem.Width := Self.Width;

end;

end.
