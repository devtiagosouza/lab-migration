unit DCollections.ClientDataSet;

interface

uses
    System.SysUtils, System.Classes, System.Generics.Collections, Data.DB, VCL.DBGrids,
    Datasnap.DBClient, System.RTTI, System.TypInfo, System.StrUtils, DCollections.Attributes,DBGrids.Helper;

type
  TClientDataSetHelper = class helper for TClientDataSet
  public
    procedure LoadFromList<T>(AList: TList<T>; DBGrid: TDBGrid);
    function Current<T>: T;
  end;




implementation

{ TClientDataSetHelper }

function TClientDataSetHelper.Current<T>: T;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  CDSField: TField;
  Value: TValue;
  Obj: TObject;
  IsRecord: Boolean;
  InstanceType: TRttiInstanceType;
begin
  Result := Default(T);

  if IsEmpty then
    Exit;

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(TypeInfo(T));
    IsRecord := RttiType.IsRecord;

    if IsRecord then
    begin
      Value := TValue.From<T>(Result);
      for Field in RttiType.GetFields do
      begin
        CDSField := FindField(Field.Name);
        if Assigned(CDSField) then
        begin
          case Field.FieldType.TypeKind of
            tkInteger: Field.SetValue(Value.GetReferenceToRawData, CDSField.AsInteger);
            tkFloat: Field.SetValue(Value.GetReferenceToRawData, CDSField.AsExtended);
            tkUString, tkLString, tkWString, tkString: Field.SetValue(Value.GetReferenceToRawData, CDSField.AsString);
            tkInt64: Field.SetValue(Value.GetReferenceToRawData, CDSField.AsLargeInt);
            tkEnumeration: Field.SetValue(Value.GetReferenceToRawData, CDSField.AsBoolean);
          end;
        end;
      end;
      Result := Value.AsType<T>;
    end
    else
    begin
      InstanceType := RttiType.AsInstance;
      Obj := InstanceType.MetaclassType.Create;

      for Field in RttiType.GetFields do
      begin
        CDSField := FindField(Field.Name);
        if Assigned(CDSField) then
        begin
          case Field.FieldType.TypeKind of
            tkInteger: Field.SetValue(Obj, CDSField.AsInteger);
            tkFloat: Field.SetValue(Obj, CDSField.AsExtended);
            tkUString, tkLString, tkWString, tkString: Field.SetValue(Obj, CDSField.AsString);
            tkInt64: Field.SetValue(Obj, CDSField.AsLargeInt);
            tkEnumeration: Field.SetValue(Obj, CDSField.AsBoolean);
          end;
        end;
      end;
      Result := TValue.From(Obj).AsType<T>;
    end;
  finally
    Context.Free;
  end;
end;



procedure TClientDataSetHelper.LoadFromList<T>(AList: TList<T>; DBGrid: TDBGrid);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  CDSField: TField;
  Item: T;
  FieldValue: TValue;
  IsRecord: Boolean;
  DisplayAttr: DisplayName;
  FieldName, DisplayLabel: string;
  Attribute : TCustomAttribute;
  i: Integer;
  createColumn : boolean;
begin
  if not Assigned(AList) or (AList.Count = 0) then
    Exit;

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(PTypeInfo(TypeInfo(T)));
    IsRecord := RttiType.IsRecord;

    Close;
    FieldDefs.Clear;
    for Field in RttiType.GetFields do
    begin
      FieldName := Field.Name;
      DisplayLabel := FieldName;

      createColumn := True;

      for Attribute in Field.GetAttributes do begin
        if Attribute is DisplayName then
        begin
          DisplayAttr := DisplayName(Attribute);
          DisplayLabel := DisplayAttr.Value;
          Break;
        end
        else if Attribute is Hide then begin
            createColumn := false;

        end;
      end;

      if (createColumn) then
      begin
        case Field.FieldType.TypeKind of
          tkInteger: FieldDefs.Add(FieldName, ftInteger, 0, False);
          tkFloat: FieldDefs.Add(FieldName, ftFloat, 0, False);
          tkUString, tkLString, tkWString, tkString: FieldDefs.Add(FieldName, ftString, 255, False);
          tkInt64: FieldDefs.Add(FieldName, ftLargeint, 0, False);
          tkEnumeration: FieldDefs.Add(FieldName, ftString, 10, False);
        else
          Continue;
        end;
      end;
    end;

    CreateDataSet;
    Open;

    for Item in AList do
    begin
      Append;
      for Field in RttiType.GetFields do
      begin
        IF (FieldDefList.Find(Field.Name) = NIL) THEN BEGIN
          CONTINUE;
        END;



        CDSField := FieldByName(Field.Name);
        if Assigned(CDSField) then
        begin
          if IsRecord then
            FieldValue := Field.GetValue(@Item)
          else
            FieldValue := Field.GetValue(TValue.From<T>(Item).AsObject);

          case Field.FieldType.TypeKind of
            tkInteger: CDSField.AsInteger := FieldValue.AsInteger;
            tkFloat: CDSField.AsFloat := FieldValue.AsExtended;
            tkUString, tkLString, tkWString, tkString: CDSField.AsString := FieldValue.AsString;
            tkInt64: CDSField.AsLargeInt := FieldValue.AsInt64;
            tkEnumeration: CDSField.AsString := ifthen(FieldValue.AsBoolean,'Sim','Não');
          end;
        end;
      end;
      Post;
    end;

    first;

    for i := 0 to FieldDefs.Count - 1 do
    begin
      CDSField := FindField(FieldDefs[i].Name);
      if Assigned(CDSField) then
      begin
        for Field in RttiType.GetFields do
          if SameText(Field.Name, CDSField.FieldName) then
            for Attribute in Field.GetAttributes do
              if Attribute is DisplayName then begin
                 DisplayAttr := DisplayName(Attribute);
                 CDSField.DisplayLabel := DisplayAttr.Value;
              end;
      end;
    end;


  finally
    Context.Free;
  end;


end;

end.
