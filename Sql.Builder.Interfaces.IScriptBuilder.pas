unit  Sql.Builder.Interfaces.IScriptBuilder;

interface

uses System.Classes, System.SysUtils, Sql.Builder.Interfaces.IQueryBuilder;

type IScriptBuilder = interface
    ['{76D99902-75A6-4609-AD75-B4324D78E417}']
    function Append(aScriptBuilder : IScriptBuilder) : IScriptBuilder; overload;
    function Append(aText : string) : IScriptBuilder; overload;
    function AppendLine(aText : string = '') : IScriptBuilder;

     function NewSql() : IQueryBuilder;

    function AsString: string;
  end;


implementation



end.
