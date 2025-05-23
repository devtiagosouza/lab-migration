unit Database.Interfaces;

interface

uses DCollections, Model.DBTable, Model.DBView, Model.DBProcedure,Model.DBFunction, Model.DBTrigger,
    Model.DBGenerator, Model.DBIndex;

type IDatabase = interface
   ['{BEB0C102-2A32-42D0-94EA-FDA1F12E9FCF}']
    procedure LoadMetadata;

    function GetTables: TList<TDBTable>;
    function GetViews: TList<TDBView>;
    function GetProcedures: TList<TDBProcedure>;
    function GetFunctions: TList<TDBFunction>;
    function GetTriggers: TList<TDBTrigger>;
    function GetGenerators: TList<TDBGenerator>;
    function GetIndices: TList<TDBIndex>;

end;

implementation

end.
