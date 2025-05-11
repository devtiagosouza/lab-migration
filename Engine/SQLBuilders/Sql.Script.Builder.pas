unit Sql.Script.Builder;

interface

 uses FirebirdKeywords,System.Classes, System.SysUtils, Sql.Builder;



type IScriptBuilder = interface
    ['{76D99902-75A6-4609-AD75-B4324D78E417}']
    function Append(aScriptBuilder : IScriptBuilder) : IScriptBuilder; overload;
    function Append(aText : string) : IScriptBuilder; overload;
    function AppendLine(aText : string = '') : IScriptBuilder;
    function AsString: string;
  end;



TScriptBuilder = class(TInterfacedObject, IScriptBuilder)
 private
    FStrings: TStringBuilder;

 public
    function Append(aScriptBuilder: IScriptBuilder) : IScriptBuilder; overload;
    function Append(aText : string) : IScriptBuilder; overload;
    function AppendLine(aText : string = '') : IScriptBuilder;

    function AsString: string;
    constructor Create();

 end;

implementation



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





end.
