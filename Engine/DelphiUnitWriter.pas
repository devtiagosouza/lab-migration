unit DelphiUnitWriter;

interface

uses system.sysutils, system.strUtils, System.Classes, DCollections;

type TMethodType = (mehodProcedure, methodFunctioin);

type

  TMethod = record
        MethodType : TMethodType;
        Declaration : string;
        Code : string
  end;

  TDelphiUnitWriter  = class

  private
    FUnitName: string;
    FClassName: string;
    FProcedureList : TList<TMethod>;
    FUsesDeclaration: string;
    FConstructorBody: string;
    FMainClass: string;

  public
      property UnitName : string read FUnitName write FUnitName;
      property MainClass : string read FMainClass write FMainClass;
      property ClassName : string read FClassName write FClassName;
      property UsesDeclaration : string read FUsesDeclaration write FUsesDeclaration;
      property ConstructorBody : string read FConstructorBody write FConstructorBody;


      procedure AddMethod(const MethodType : TMethodType; Declaration, Code: string);

      procedure SaveUnitFile(Path : string);

      constructor Create();
      destructor Destroy; override;

  end;

implementation

{ TDelphiUnitWriter }

procedure TDelphiUnitWriter.AddMethod(const MethodType : TMethodType; Declaration, Code: string);
var
  proc : TMethod;
begin
    proc.MethodType := MethodType;
    proc.Declaration := Declaration;
    proc.Code := Code;

    FProcedureList.Add(proc);
end;

constructor TDelphiUnitWriter.Create();
begin
  FProcedureList :=  TList<TMethod>.Create;
  FUnitName := 'Unit1';
  FClassName := 'TMyClass';
  FUsesDeclaration := '';
  FConstructorBody := '';
end;

destructor TDelphiUnitWriter.Destroy;
begin
  FProcedureList.Free;
  inherited;
end;

procedure TDelphiUnitWriter.SaveUnitFile(Path: string);
var
  SL: TStringList;
  i: Integer;
  vProc : TMethod;
  line : string;
begin
    SL := TStringList.Create;
  try
    // unit header
    SL.Add('unit ' + FUnitName + ';');
    SL.Add('');
    SL.Add('interface');
    SL.Add('');

    // Uses
    if FUsesDeclaration.Trim <> '' then
      SL.Add('uses ' + FUsesDeclaration + ';')
    else
      SL.Add('uses System.Classes;');

    SL.Add('');

    // Declaração da classe
    SL.Add('type');
    SL.Add('  ' + FClassName + ' = class'+ifthen(string.isNullOrEmpty(FMainClass) = false, '('+FMainClass+')',''));
    SL.Add('  public');

    for i := 0 to FProcedureList.Count - 1 do
    begin
      vProc := FProcedureList[i];
      SL.Add('    '+ifthen(vProc.MethodType = mehodProcedure,'procedure','function')+' '+vProc.Declaration+';');
      SL.Add('');
    end;

    SL.Add('    constructor Create;');
    SL.Add('    destructor Destroy; override;');
    SL.Add('  end;');
    SL.Add('');
    SL.Add('implementation');
    SL.Add('');

    // Implementação do construtor
    SL.Add('constructor ' + FClassName + '.Create;');
    SL.Add('begin');
    if FConstructorBody.Trim <> '' then
      SL.Add('  ' + FConstructorBody)
    else
      SL.Add('');
    SL.Add('end;');
    SL.Add('');

    // Implementação do destrutor
    SL.Add('destructor ' + FClassName + '.Destroy;');
    SL.Add('begin');
    SL.Add('  inherited;');
    SL.Add('end;');
    SL.Add('');

    for i := 0 to FProcedureList.Count - 1 do
    begin
      vProc := FProcedureList[i];
      SL.Add(ifthen(vProc.MethodType = mehodProcedure,'procedure','function')+ ' ' +FClassName+'.'+vProc.Declaration+';');
      SL.Add('begin');

      for line in vProc.Code.split([sLineBreak]) do begin
          SL.Add('  '+line);
      end;

      SL.Add('end;');
      SL.Add('');
    end;

    SL.Add('end.');


    SL.SaveToFile(Path+'\'+UnitName+'.pas', TEncoding.ANSI);
  finally
    SL.Free;
  end;
end;

end.
