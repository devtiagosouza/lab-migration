unit DCollections;

interface

 uses
  System.Generics.Collections, System.SysUtils, System.Rtti,System.Generics.Defaults;


  type
  TList<T> = class(System.Generics.Collections.TList<T>)
  private
   function CompareNumbers(const A, B: Extended): Integer;

  public
    function Where(Predicate: TFunc<T, Boolean>): TList<T>;
    function Exists(Predicate: TFunc<T, Boolean>) : Boolean;
    function First(Predicate: TFunc<T, Boolean>) : T;
    function OrderBy(const FieldName: string; Ascending: Boolean = true): TList<T>;
    function Select<TResult>(Selector: TFunc<T, TResult>): TList<TResult>;
  end;



implementation

{ TListHelper<T> }

function TList<T>.CompareNumbers(const A, B: Extended): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

function TList<T>.Exists(Predicate: TFunc<T, Boolean>): Boolean;
var
  Item: T;
begin
    for Item in Self do
    if Predicate(Item) then
      Exit(True);
   Result := False;
end;

function TList<T>.First(Predicate: TFunc<T, Boolean>): T;
var
  Item: T;
begin
   result := Default(T);

   for Item in Self do
   if Predicate(Item) then
      exit(item);

end;


function TList<T>.OrderBy(const FieldName: string; Ascending: Boolean): TList<T>;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
begin
  Result := TList<T>.Create(Self);

  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(TypeInfo(T));
    Field := RttiType.GetField(FieldName);

    if not Assigned(Field) then
      Exit;

    Result.Sort(TComparer<T>.Construct(
      function(const Left, Right: T): Integer
      var
        LeftValue, RightValue: TValue;
      begin
        LeftValue := Field.GetValue(@Left);
        RightValue := Field.GetValue(@Right);

        case LeftValue.Kind of
          tkInteger, tkInt64:
            Result := LeftValue.AsInteger - RightValue.AsInteger;
          tkFloat:
            Result := CompareNumbers(LeftValue.AsExtended, RightValue.AsExtended);
          tkUString:
            Result := CompareStr(LeftValue.AsString, RightValue.AsString);
        else
          Result := 0;
        end;

        if not Ascending then
          Result := -Result;
      end
    ));
  finally
    Ctx.Free;
  end;
end;

function TList<T>.Select<TResult>(Selector: TFunc<T, TResult>): TList<TResult>;
var
  Item: T;
begin
  Result := TList<TResult>.Create;
  for Item in Self do
    Result.Add(Selector(Item));
end;

function TList<T>.Where(Predicate: TFunc<T, Boolean>): TList<T>;
var
  Item: T;
begin
  Result := TList<T>.Create;
  for Item in Self do
    if Predicate(Item) then
      Result.Add(Item);
end;


end.

