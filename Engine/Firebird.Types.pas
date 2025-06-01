unit Firebird.Types;

interface

uses
  System.Generics.Collections, System.RegularExpressions;

   type TDDLType = (comUnknown, comCreateIndexOnFields, comCreateIndexComputed,
                   comCreateConstraintPK, comCreateConstraintUnique, comCreateConstraintCheck, comCreateConstraintFK, comCreateTable,
                   comCreateField, comCreateOrAlterView, comCreateGenerator,comCreateOrAlterTrigger, comCreateOrAlterProcedure,
                   comCreateOrAlterFunction);

    type TFieldType = (ftUnknown, ftSmallInt, ftInteger, ftBigInt, ftBoolean, ftFloat, ftDoublePrecision, ftNumeric, ftDecimal, ftDate, ftTime,
    ftTimestamp, ftChar, ftVarchar, ftBlob );

    type TFieldTypeArray = array of TFieldType;

    type TFieldTypeMatch = record
       FieldType: TFieldType;
       Regex : string;
       Match : TMatch;
    end;

   type TDDLMatch = record
     DDLType: TDDLType;
     Regex : string;
     Match : TMatch;
  end;

  type TTypeConversion = (conversionIsNotSupported, conversionNatural, conversionDataTransfer);



  procedure InitializeFirebirdTypes;
  function IsValidFirebirdType(const FieldType: string): Boolean;
  function MatchFirebirdType(const Text: string) : TFieldTypeMatch;

//  function MatchFirebirdType(const Text: string): TMatch;

  function MatchDDL(const AText: string) : TDDLMatch;
  function GetPattern(ADDLCommand : TDDLType) : string; overload;
  function GetPattern(AFieldType : TFieldType) : string; overload;
  function ConversionTypeSupported(newType,oldType : string) : TTypeConversion;

  implementation

  uses  System.SysUtils;

type
  TFirebirdTypePattern = record
    TypeName: string;
    RegexPattern: string;
  end;


var
  FirebirdTypes: TDictionary<string, string>;
  DDLPatterns : TDictionary<TDDLType, string>;
  FieldTypePatterns : TDictionary<TFieldType, string>;
  NaturalTypeConversionMap : TDictionary<TFieldType,TFieldTypeArray>;
  TypeConversionExportDataMap : TDictionary<TFieldType,TFieldTypeArray>;

procedure InitializeNaturalTypeConversionMap;
begin
   NaturalTypeConversionMap := TDictionary<TFieldType,TFieldTypeArray>.Create;
   TypeConversionExportDataMap := TDictionary<TFieldType,TFieldTypeArray>.Create;

   with NaturalTypeConversionMap do begin
       Add(ftSmallInt,[ftInteger,ftBigInt,ftChar,ftVarchar]);
       Add(ftInteger,[ftBigInt,ftChar,ftVarchar]);
       Add(ftBigInt,[ftChar,ftVarchar]);
       Add(ftBoolean,[]);
       Add(ftFloat,[ftDoublePrecision,ftChar,ftVarchar]);
       Add(ftDoublePrecision,[ftChar,ftVarchar]);  //24+ chars
       Add(ftNumeric,[ftNumeric,ftBigInt,ftDecimal,ftChar,ftVarchar  ]); //18+ chars
       Add(ftDecimal,[ftDecimal,ftNumeric,ftBigInt,ftChar,ftVarchar]);
       Add(ftDate,[ftTimestamp,ftChar,ftVarchar]); //10+ chars
       Add(ftTime,[ftChar,ftVarchar]); //13+ chars
       Add(ftTimestamp,[ftChar,ftVarchar]); //25+ chars
       Add(ftChar,[ftChar,ftVarchar]);
       Add(ftVarchar,[ftVarchar,ftChar]);
       Add(ftBlob,[]);
   end;

   with TypeConversionExportDataMap do begin
       Add(ftFloat,[ftNumeric]);
       Add(ftDoublePrecision,[ftNumeric]);
   end;

end;

function ConversionTypeSupported(newType,oldType : string) : TTypeConversion;
          function FieldTypeInArray(const AValue: TFieldType; const AArray: TFieldTypeArray): Boolean;
          var
            Item: TFieldType;
          begin
            Result := False;
            for Item in AArray do
            begin
              if Item = AValue then
              begin
                Result := True;
                Exit;
              end;
            end;
          end;
var
  NewTypeMatch : TFieldTypeMatch;  //Modelo
  OldTypeMatch : TFieldTypeMatch; //Banco MFX
  supportedTypesList : TFieldTypeArray;
begin
  Result := conversionIsNotSupported;

  NewTypeMatch := MatchFirebirdType(NewType);
  OldTypeMatch := MatchFirebirdType(OldType);


  if (NewTypeMatch.Match.Success) and (OldTypeMatch.Match.Success) then begin
      if (NaturalTypeConversionMap.TryGetValue(OldTypeMatch.FieldType,supportedTypesList)) then begin
         if (FieldTypeInArray(NewTypeMatch.FieldType, supportedTypesList)) then
            result := conversionNatural;
      end;


      if (result = conversionIsNotSupported) and (TypeConversionExportDataMap.TryGetValue(OldTypeMatch.FieldType,supportedTypesList))  then begin
         if (FieldTypeInArray(NewTypeMatch.FieldType, supportedTypesList)) then
            result := conversionDataTransfer;
      end;
  end;

end;



procedure InitializeDDLPatterns;
begin
   DDLPatterns := TDictionary<TDDLType, string>.Create;


   DDLPatterns.Add(comCreateTable,'^\s*CREATE\s+TABLE\s+(\w+)');
   DDLPatterns.Add(comCreateOrAlterView,'(?is)^\s*CREATE\s+OR\s+ALTER\s+VIEW\s+(\w+)\s*\(\s*([\w\s,]+?)\s*\)\s+AS\s+(.*);?\s*$');

   DDLPatterns.Add(comCreateConstraintPK,'^\s*ALTER\s+TABLE\s+(\w+)\s+ADD\s+CONSTRAINT\s+(\w+)\s+PRIMARY\s+KEY\s*\(\s*([^)]+?)\s*\)(?:\s+USING\s+INDEX\s+(\w+))?\s*;?\s*$');
   DDLPatterns.Add(comCreateConstraintUnique,'^\s*ALTER\s+TABLE\s+(\w+)\s+ADD\s+CONSTRAINT\s+(\w+)\s+UNIQUE\s*\(\s*("?[\w\s]+"?(?:\s*,\s*"?[\w\s]+"?)*)\s*\)(?:\s+USING\s+DESCENDING\s+INDEX\s+(\w+))?\s*;?\s*$');
   DDLPatterns.Add(comCreateConstraintCheck,'^\s*ALTER\s+TABLE\s+(\w+)\s+ADD\s+CONSTRAINT\s+(\w+)\s+CHECK\s*\((.*)\)\s*;?\s*$');
   DDLPatterns.Add(comCreateConstraintFK,'(?im)^\s*ALTER\s+TABLE\s+(\w+)\s+ADD\s+CONSTRAINT\s+(\w+)\s+FOREIGN\s+KEY\s*\(\s*([^)]+?)\s*\)\s+REFERENCES\s+(\w+)\s*\(\s*([^)]+?)\s*\)\s*(ON\s+DELETE\s+\w+(?:\s+ON\s+UPDATE\s+\w+)?)?\s*(USING\s+DESCENDING\s+INDEX\s+\w+)?\s*;?\s*$');

   DDLPatterns.Add(comCreateIndexOnFields,'^\s*CREATE\s+(UNIQUE DESCENDING|UNIQUE|DESCENDING)?\s*INDEX\s+(\w+)\s+ON\s+(\w+)\s*\(\s*([^)]+?)\s*\)');
   DDLPatterns.Add(comCreateIndexComputed,'^\s*CREATE\s+(UNIQUE DESCENDING|UNIQUE|DESCENDING)?\s*INDEX\s+(\w+)\s+ON\s+(\w+)\s+COMPUTED\s+BY\s*\((.*)\)\s*$');

   DDLPatterns.Add(comCreateGenerator,'^\s*CREATE\s+GENERATOR\s(\w+)?\s*;?\s*$');
   DDLPatterns.Add(comCreateOrAlterTrigger,'(?is)^\s*CREATE\s+OR\s+ALTER\s+TRIGGER\s+(\w+)\s+FOR\s+(\w+)\s+(ACTIVE|INACTIVE)\s+(.*?)POSITION\s+(\d+)\s+AS\s+(.*END\s*\^?\s*)$');

   DDLPatterns.Add(comCreateOrAlterProcedure,'(?is)^\s*CREATE\s+OR\s+ALTER\s+PROCEDURE\s+(\w+)(?:\s*\((.*?)\))?(?:\s*RETURNS\s*\((.*?)\))?\s*AS\s+(.*END\s*\^?\s*)$');
   DDLPatterns.Add(comCreateOrAlterFunction,'(?is)^\s*CREATE\s+OR\s+ALTER\s+FUNCTION\s+(\w+)(?:\s*\((.*?)\))?\s+RETURNS\s+([\w\s\(\),]+?)\s+AS\s+(.*END\s*\^?\s*)$');
   DDLPatterns.Add(comCreateField,'^\s*ALTER\s+TABLE\s+(\w+)\s+ADD\s+(?!CONSTRAINT\s)(\w+)\s+(.+)$');
end;

procedure InitializeFieldTypePatterns;
begin
   FieldTypePatterns := TDictionary<TFieldType, string>.Create;

   with FieldTypePatterns do begin
       Add(ftSmallInt,'\b(SMALLINT)\b');
       Add(ftInteger,'\b(INTEGER)\b');
       Add(ftBigInt,'\b(BIGINT)\b');
       Add(ftBoolean,'\b(BOOLEAN)\b');
       Add(ftFloat,'\b(FLOAT)\b');
       Add(ftDoublePrecision,'\b(DOUBLE\s+PRECISION)\b');
       Add(ftNumeric,'\b(NUMERIC)\s*\(\s*(\d+)\s*,\s*(\d+)\s*\)');
       Add(ftDecimal,'\b(DECIMAL)\s*\(\s*(\d+)\s*,\s*(\d+)\s*\)');
       Add(ftDate,'\b(DATE)\b');
       Add(ftTime,'\b(TIME)\b');
       Add(ftTimestamp,'\b(TIMESTAMP)\b');
       Add(ftChar,'\b(CHAR)\s*\(\s*(\d+)\s*\s*\)');
       Add(ftVarchar,'\b(VARCHAR)\s*\(\s*(\d+)\s*\s*\)');
       Add(ftBlob,'\b(BLOB)\s+(SUB_TYPE)\s+(BINARY|TEXT)\s+(SEGMENT\s+SIZE)\s+(\d+)');
   end;


end;

procedure InitializeFirebirdTypes;
begin
  FirebirdTypes := TDictionary<string, string>.Create;

  FirebirdTypes.Add('BIGINT', '\bBIGINT\b');
  FirebirdTypes.Add('BLOB SUB_TYPE BINARY SEGMENT SIZE', '\bBLOB\s+SUB_TYPE\s+BINARY\s+SEGMENT\s+SIZE\s+\d+\b');
  FirebirdTypes.Add('BLOB SUB_TYPE TEXT SEGMENT SIZE', '\bBLOB\s+SUB_TYPE\s+TEXT\s+SEGMENT\s+SIZE\s+\d+\b');
  FirebirdTypes.Add('BOOLEAN', '\bBOOLEAN\b');
  FirebirdTypes.Add('CHAR', '\bCHAR\(\d+\)\b');
  FirebirdTypes.Add('CHARACTER', '\bCHARACTER\(\d+\)\b');
  FirebirdTypes.Add('DATE', '\bDATE\b');
  FirebirdTypes.Add('DECIMAL', '\bDECIMAL\(\d+,\d+\)\b');
  FirebirdTypes.Add('DOUBLE PRECISION', '\bDOUBLE\s+PRECISION\b');
  FirebirdTypes.Add('FLOAT', '\bFLOAT\b');
  FirebirdTypes.Add('INTEGER', '\bINTEGER\b');
  FirebirdTypes.Add('INT', '\bINT\b');
  FirebirdTypes.Add('NUMERIC', '\bNUMERIC\(\d+,\d+\)\b');
  FirebirdTypes.Add('SMALLINT', '\bSMALLINT\b');
  FirebirdTypes.Add('TIME', '\bTIME\b');
  FirebirdTypes.Add('TIMESTAMP', '\bTIMESTAMP\b');
  FirebirdTypes.Add('VARCHAR', '\bVARCHAR\(\d+\)');
end;

function MatchDDL(const AText: string) : TDDLMatch;
var
  Pair: TPair<TDDLType, string>;
  Regex: TRegEx;
  M: TMatch;
begin
  Result.DDLType := comUnknown;
  Result.Regex := '';
  Result.Match := Default(TMatch);

  for Pair in DDLPatterns do
  begin
    Regex := TRegEx.Create(Pair.Value, [roIgnoreCase, roSingleLine]);
    M := Regex.Match(AText);
    if M.Success then
    begin
      Result.DDLType := Pair.Key;
      Result.Regex := Pair.Value;
      Result.Match := M;
      Exit;
    end;
  end;
end;

function GetPattern(ADDLCommand : TDDLType) : string;
var
  pair : TPair<TDDLType,String>;
begin
  pair := DDLPatterns.ExtractPair(ADDLCommand);
  Result := pair.Value;
end;

function GetPattern(AFieldType : TFieldType) : string;
var
  pair : TPair<TFieldType,String>;
begin
  pair := FieldTypePatterns.ExtractPair(AFieldType);
  Result := pair.Value;
end;


function MatchFirebirdType(const Text: string) : TFieldTypeMatch;
var
  Pair: TPair<TFieldType, string>;
  Regex: TRegEx;
  M: TMatch;
begin
  Result.FieldType := ftUnknown;
  Result.Regex := '';
  Result.Match := Default(TMatch);

  for Pair in FieldTypePatterns do
  begin
    Regex := TRegEx.Create(Pair.Value, [roIgnoreCase, roSingleLine]);
    M := Regex.Match(Text);
    if M.Success then
    begin
      Result.FieldType := Pair.Key;
      Result.Regex := Pair.Value;
      Result.Match := M;
      Exit;
    end;
  end;
end;

//function MatchFirebirdType(const Text: string): TMatch;
//var
//  Pair: TPair<string, string>;
//  Regex: TRegEx;
//  M: TMatch;
//begin
//  Result := Default(TMatch);
//  for Pair in FirebirdTypes do
//  begin
//    Regex := TRegEx.Create(Pair.Value, [roIgnoreCase]);
//    M := Regex.Match(Text);
//    if M.Success then
//    begin
//      Result := M;
//      Exit;
//    end;
//  end;
//end;


function IsValidFirebirdType(const FieldType: string): Boolean;
var
  Pair: TPair<string, string>;
  Regex: TRegEx;
begin
  Result := False;
  for Pair in FirebirdTypes do
  begin
    Regex := TRegEx.Create(Pair.Value, [roIgnoreCase]);
    if Regex.IsMatch(FieldType) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;


initialization
  InitializeFirebirdTypes;
  InitializeDDLPatterns;
  InitializeFieldTypePatterns;
  InitializeNaturalTypeConversionMap;

finalization
  FirebirdTypes.Free;
  DDLPatterns.Free;
  FieldTypePatterns.Free;
  NaturalTypeConversionMap.Free;

End.

