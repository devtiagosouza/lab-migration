unit Parser.Interfaces;

interface
 uses Model.DBObject, Model.DBTable, Model.DBProcedure;

  type IFBDDLParser = interface
    ['{549012C1-2C32-4C67-A97C-6BEC5240CC70}']
    function Parse(const ASQL: string): TDBObject;
  end;

  ITableParser = interface(IFBDDLParser)
    ['{5F77774A-6DA9-40C7-B2F5-06AFFDA284FD}']
    function ParseTable(const ASQL: string): TDBTable;
  end;

  IProcedureParser = interface(IFBDDLParser)
  ['{142E3EFD-DE1B-4FC6-9973-47FFA0D62A99}']
    function ParseProcedure(const ASQL: string): TDBProcedure;
  end;





implementation

end.
