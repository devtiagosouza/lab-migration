unit Sql.Builder.Interfaces.IQueryBuilder;

interface

uses System.Classes, System.SysUtils;

type
  IQueryBuilder = interface
    ['{976BC7DB-9ED1-495A-A22F-07C72998BEC4}']

      function Append(text: string): IQueryBuilder; overload;
      function Append(list: TStringList; const separator: string) : IQueryBuilder; overload;
      function AppendLine(text: string): IQueryBuilder; overload;
      function AppendLine(list: TStringList; const separator: string) : IQueryBuilder; overload;
      function AppendWithIndent(level : integer; text: string): IQueryBuilder;
      function AppendEmptyLine: IQueryBuilder;

      function IncIndent : IQueryBuilder;
      function DecIndent : IQueryBuilder;
      function ResetIndent: IQueryBuilder;



      function AsString: string;

    end;




implementation

end.
