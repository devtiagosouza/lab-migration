unit Model.DBField;

interface
  uses Model.DBObject, system.SysUtils, system.strUtils;

 type TDBField = class(TDBObject)

 private
    FFieldType: string;
    FDefaultValue: string;
    FNotNull: boolean;
    FCharset: string;
    FCollate: string;
    FTableName: string;
    FFieldSet: string;

 public

    property TableName : string read FTableName write FTableName;
    property FieldType : string read FFieldType write FFieldType;
    property FieldSet : string read FFieldSet write FFieldSet;
    property NotNull : boolean read FNotNull write FNotNull;
    property DefaultValue : string read FDefaultValue write FDefaultValue;
    property Charset : string read FCharset write FCharset;
    property Collate : string read FCollate write FCollate;


   function CreateCommand: string; override;

 end;

implementation

{ TDBField }


function TDBField.CreateCommand: string;
begin
   result := CommandBuilder(':FIELD_NAME :FIELD_TYPE :FIELD_SET')
       .AddPar('FIELD_NAME', Name, true)
       .AddPar('FIELD_TYPE', FieldType)
       .AddPar('FIELD_SET', FieldSet)
       .asString;
end;

end.
