unit Database.Interfaces;

interface

uses DCollections, Model.DBTable, Model.DBView, Model.DBProcedure,Model.DBFunction, Model.DBTrigger,
    Model.DBGenerator, Model.DBIndex, Model.DBField;

type IDatabase = interface
   ['{BEB0C102-2A32-42D0-94EA-FDA1F12E9FCF}']
    procedure LoadMetadata;

    function GetTables: TList<TDBTable>;
    function GetViews: TList<TDBView>;
    function GetProcedures: TList<TDBProcedure>;
    function GetFunctions: TList<TDBFunction>;
    function GetTriggers: TList<TDBTrigger>;
    function GetGenerators: TList<TDBGenerator>;
    function GetAllGenerators: TList<TDBGenerator>;
    function GetIndices: TList<TDBIndex>;

    procedure AddOrSetTable(obj : TDBTable);
    procedure AddOrSetField(obj: TDBField);
    procedure AddOrSetView(obj : TDBView);
    procedure AddOrSetProcedure(obj : TDBProcedure);
    procedure AddOrSetFunction(obj : TDBFunction);
    procedure AddOrSetTrigger(obj : TDBTrigger);
    procedure AddOrSetGenerator(obj : TDBGenerator);
    procedure AddOrSetIndex(obj : TDBIndex);

end;

implementation

end.
