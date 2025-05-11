unit Sql.Builder;

interface


uses
  FirebirdKeywords, System.Classes,  System.SysUtils, Sql.Builder.SqlTemplate;


 type


   ISQLBuilder = interface
   ['{976BC7DB-9ED1-495A-A22F-07C72998BEC4}']

      function Append(text: string): ISQLBuilder; overload;
      function Append(list: TStringList; const separator: string) : ISQLBuilder; overload;
      function AppendLine(text: string): ISQLBuilder; overload;
      function AppendLine(list: TStringList; const separator: string) : ISQLBuilder; overload;
      function AppendWithIndent(level : integer; text: string): ISQLBuilder;
      function AppendEmptyLine: ISQLBuilder;

      function IncIndent : ISQLBuilder;
      function DecIndent : ISQLBuilder;
      function ResetIndent: ISQLBuilder;

      function AsTemplate() : ISQLTemplate;

      function AsString: string;
   end;


 TSQLBuilder = class(TInterfacedObject, ISQLBuilder)
   private
    FIndentLevel : integer;
    FStrings: TStringBuilder;
  public

    function Append(text: string): ISQLBuilder; overload;
    function Append(list: TStringList; const separator: string) : ISQLBuilder; overload;
    function AppendLine(text: string): ISQLBuilder; overload;
    function AppendLine(list: TStringList; const separator: string) : ISQLBuilder; overload;
    function AppendWithIndent(level : integer; text: string): ISQLBuilder;
    function AppendEmptyLine: ISQLBuilder;

    function IncIndent : ISQLBuilder;
    function DecIndent : ISQLBuilder;
    function ResetIndent: ISQLBuilder;


     function AsTemplate() : ISQLTemplate;

    constructor Create;
    destructor Destroy; override;
    function AsString: string;
  end;

implementation

{ TQueryBuilder }

constructor TSQLBuilder.Create;
begin
  inherited Create;
  ResetIndent;
  FStrings := TStringBuilder.Create;
end;

function TSQLBuilder.DecIndent: ISQLBuilder;
begin
  Dec(FIndentLevel);
  result := Self;
end;

destructor TSQLBuilder.Destroy;
begin
  FStrings.Free;
  inherited;
end;



function TSQLBuilder.Append(text: string): ISQLBuilder;
begin
  Result := AppendWithIndent(FIndentLevel,text);
end;

function TSQLBuilder.Append(list: TStringList;
  const separator: string): ISQLBuilder;
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

function TSQLBuilder.AppendEmptyLine: ISQLBuilder;
begin
  FStrings.AppendLine;
  Result := Self;
end;

function TSQLBuilder.AppendLine(list: TStringList;
  const separator: string): ISQLBuilder;
  var
  text: string;
  i : integer;
begin
   FStrings.AppendLine;
   Result := Append(list,separator);
end;

function TSQLBuilder.AppendLine(text: string): ISQLBuilder;
begin
   FStrings.AppendLine;
   Result := AppendWithIndent(FIndentLevel,text);
end;

function TSQLBuilder.AppendWithIndent(level: integer;
  text: string): ISQLBuilder;
begin
 if (level >= 2) then
    FStrings.Append(StringOfChar(' ', (level - 1) * 4) + text)
 else FStrings.Append(text);

 Result := Self;

end;



function TSQLBuilder.IncIndent: ISQLBuilder;
begin
  Inc(FIndentLevel);
  Result := Self;
end;

function TSQLBuilder.ResetIndent: ISQLBuilder;
begin
  FIndentLevel := 1;
end;


function TSQLBuilder.AsString: string;
begin
   Result := FStrings.ToString;
end;

function TSQLBuilder.AsTemplate: ISQLTemplate;
begin
   result := TSQLTemplate.Create(AsString);
end;

end.

