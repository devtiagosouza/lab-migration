unit Sql.Query.Builder;

interface

uses
  FirebirdKeywords, System.Classes, Sql.Builder.Interfaces, System.SysUtils;


 type TQueryBuilder = class(TInterfacedObject, IQueryBuilder)
   private
    FIndentLevel : integer;
    FStrings: TStringBuilder;

  public

    function Start(initText: string = ''): IQueryBuilder;
    function Finalize(endText: string = ';'): IQueryBuilder;

    function Append(text: string): IQueryBuilder; overload;
    function Append(list: TStringList; const separator: string) : IQueryBuilder; overload;
    function AppendLine(text: string): IQueryBuilder; overload;
    function AppendLine(list: TStringList; const separator: string) : IQueryBuilder; overload;
    function AppendWithIndent(level : integer; text: string): IQueryBuilder;
    function AppendEmptyLine: IQueryBuilder;

    function IncIndent : IQueryBuilder;
    function DecIndent : IQueryBuilder;
    function ResetIndent: IQueryBuilder;

    constructor Create;
    destructor Destroy; override;
    function AsString: string;
  end;

implementation

{ TQueryBuilder }

constructor TQueryBuilder.Create;
begin
  inherited Create;
  ResetIndent;
  FStrings := TStringBuilder.Create;
end;

function TQueryBuilder.DecIndent: IQueryBuilder;
begin
  Dec(FIndentLevel);
  result := Self;
end;

destructor TQueryBuilder.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TQueryBuilder.Append(text: string): IQueryBuilder;
begin
  Result := AppendWithIndent(FIndentLevel,text);
end;

function TQueryBuilder.Append(list: TStringList;
  const separator: string): IQueryBuilder;
var
  text: string;
  i : integer;
begin
   Result := Self;
   for I := 0 to list.Count - 1 do begin
       if i < list.Count - 1 then
          Append(list[i] + separator)
       else Append(list[i]);
   end;
   Result := self;
end;

function TQueryBuilder.AppendEmptyLine: IQueryBuilder;
begin
  FStrings.AppendLine;
  Result := Self;
end;

function TQueryBuilder.AppendLine(list: TStringList;
  const separator: string): IQueryBuilder;
  var
  text: string;
  i : integer;
begin
   FStrings.AppendLine;
   Result := Append(list,separator);
end;

function TQueryBuilder.AppendLine(text: string): IQueryBuilder;
begin
   FStrings.AppendLine;
   Result := AppendWithIndent(FIndentLevel,text);
end;

function TQueryBuilder.AppendWithIndent(level: integer;
  text: string): IQueryBuilder;
begin
 if (level >= 2) then
    FStrings.Append(StringOfChar(' ', (level - 1) * 4) + text)
 else FStrings.Append(text);

 Result := Self;

end;

function TQueryBuilder.Finalize(endText: string): IQueryBuilder;
begin
  FStrings.Append(endText);
  Result := Self;
end;

function TQueryBuilder.IncIndent: IQueryBuilder;
begin
  Inc(FIndentLevel);
  Result := Self;
end;

function TQueryBuilder.ResetIndent: IQueryBuilder;
begin
  FIndentLevel := 1;
end;

function TQueryBuilder.Start(initText: string): IQueryBuilder;
begin
  Result := TQueryBuilder.Create;
  if initText <> '' then
    FStrings.Append(initText + sLineBreak);
end;

function TQueryBuilder.AsString: string;
begin
  Result := FStrings.ToString;
end;

end.

