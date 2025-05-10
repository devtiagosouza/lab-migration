unit MegaNews.Collections.Extensions;

interface

 uses
  System.Generics.Collections, System.SysUtils;


  type
  TList<T> = class(System.Generics.Collections.TList<T>)
  public
    function Where(Predicate: TFunc<T, Boolean>): TList<T>;
  end;



implementation

{ TListHelper<T> }

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
