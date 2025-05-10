unit Sql.Script.Builder;

interface

 uses FirebirdKeywords,Sql.Builder.Interfaces.IScriptBuilder,
 sql.Query.Builder,
  System.Classes, System.SysUtils, Sql.Builder.Interfaces.IQueryBuilder;

type TScriptBuilder = class(TInterfacedObject, IScriptBuilder)
 private
    FStrings: TStringBuilder;

 public
    function Append(aScriptBuilder: IScriptBuilder) : IScriptBuilder; overload;
    function Append(aText : string) : IScriptBuilder; overload;
    function AppendLine(aText : string = '') : IScriptBuilder;

    function NewSql : TQueryBuilder;

    function AsString: string;
    constructor Create();

 end;

implementation


  var FQueryBuilder : TQueryBuilder;


{ TScriptBuilder }

function TScriptBuilder.Append(aScriptBuilder: IScriptBuilder): IScriptBuilder;
begin
 FStrings.Append(aScriptBuilder.AsString);
 Result := Self;
end;

function TScriptBuilder.Append(aText: string): IScriptBuilder;
begin
   FStrings.Append(aText);
   Result := Self;
end;

function TScriptBuilder.AppendLine(aText : string = ''): IScriptBuilder;
begin
   FStrings.AppendLine;
   if (String.IsNullOrEmpty(aText) = false) then
       FStrings.Append(aText);

   Result := Self;
end;

function TScriptBuilder.AsString: string;
begin
    Result := FStrings.ToString;
end;

constructor TScriptBuilder.Create();
begin
   FStrings := TStringBuilder.Create;
end;



function TScriptBuilder.NewSql: TQueryBuilder;
begin
  FQueryBuilder := TQueryBuilder.Create;
  Result := FQueryBuilder;
end;

end.
