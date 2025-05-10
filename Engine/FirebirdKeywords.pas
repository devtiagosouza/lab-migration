unit FirebirdKeywords;

interface

uses
  System.SysUtils, System.Classes;

type
  TFirebirdKeywords = class
  private
    class var FKeywords: TStringList;
    class constructor Create;
    class destructor Destroy;
  public
    class function IsReservedWord(const AWord: string): Boolean;
  end;

implementation

{ TFirebirdKeywords }

class constructor TFirebirdKeywords.Create;
begin
  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := False;
  FKeywords.Sorted := True;

  FKeywords.AddStrings([
    'ACTIVE', 'ADD','TYPE', 'ADMIN', 'ALL', 'ALTER',
    'AND', 'ANY', 'AS', 'ASC', 'ASCENDING',
    'AT', 'AUTO', 'AUTONOMOUS', 'AVG', 'BEFORE',
    'BEGIN', 'BETWEEN', 'BIGINT', 'BIT_LENGTH', 'BLOB',
    'BOTH', 'BY', 'CASE', 'CAST', 'CHAR',
    'CHAR_LENGTH', 'CHARACTER_LENGTH', 'CHECK', 'COLLATE', 'COLUMN',
    'COMMIT', 'CONNECT', 'CONSTRAINT', 'COUNT', 'CREATE',
    'CROSS', 'CURRENT', 'CURRENT_CONNECTION', 'CURRENT_DATE', 'CURRENT_ROLE',
    'CURRENT_TIME', 'CURRENT_TIMESTAMP', 'CURRENT_TRANSACTION', 'CURRENT_USER', 'DATABASE',
    'DATE', 'DAY', 'DEC', 'DECIMAL', 'DECLARE',
    'DEFAULT', 'DELETE', 'DESC', 'DESCENDING', 'DISTINCT',
    'DO', 'DOMAIN', 'DOUBLE', 'DROP', 'ELSE',
    'END', 'ENTRY_POINT', 'ESCAPE', 'EXCEPTION', 'EXECUTE',
    'EXISTS', 'EXIT', 'EXTERNAL', 'EXTRACT', 'FETCH',
    'FILTER', 'FLOAT', 'FOR', 'FOREIGN', 'FROM',
    'FULL', 'FUNCTION', 'GDSCODE', 'GENERATOR', 'GRANT',
    'GROUP', 'HAVING', 'IF', 'IN', 'INDEX',
    'INNER', 'INPUT_TYPE', 'INSERT', 'INT', 'INTEGER',
    'INTO', 'IS', 'JOIN', 'LEADING', 'LEFT',
    'LIKE', 'LONG', 'MERGE', 'MONTH', 'NATIONAL',
    'NATURAL', 'NCHAR', 'NO', 'NOT', 'NULL',
    'NUMERIC', 'OCTET_LENGTH', 'OF', 'ON', 'ONLY',
    'OPEN', 'OR', 'ORDER', 'OUTER', 'OUTPUT_TYPE',
    'OVER', 'PLAN', 'POSITION', 'POST_EVENT', 'PRECISION',
    'PRIMARY', 'PROCEDURE', 'PROTECTED', 'RDB$DB_KEY', 'REAL',
    'RECORD_VERSION', 'RECREATE', 'RECURSIVE', 'REFERENCES', 'RELEASE',
    'RETURNING', 'RETURNS', 'REVOKE', 'RIGHT', 'ROLLBACK',
    'ROW_COUNT', 'ROWS', 'SAVEPOINT', 'SECOND', 'SELECT',
    'SET', 'SHADOW', 'SHARE', 'SINGULAR', 'SMALLINT',
    'SOME', 'SQLCODE', 'START', 'STDDEV', 'SUM',
    'SUSPEND', 'TABLE', 'THEN', 'TIME', 'TIMESTAMP',
    'TO', 'TRAILING', 'TRANSACTION', 'TRIGGER', 'TRIM',
    'UNION', 'UNIQUE', 'UPDATE', 'USER', 'USING',
    'VALUE', 'VALUES', 'VARCHAR', 'VARIABLE', 'VARYING',
    'VIEW', 'WHEN', 'WHERE', 'WHILE', 'WITH',
    'WORK', 'WRITE', 'YEAR'
  ]);
end;

class destructor TFirebirdKeywords.Destroy;
begin
  FKeywords.Free;
end;

class function TFirebirdKeywords.IsReservedWord(const AWord: string): Boolean;
begin
  Result := FKeywords.IndexOf(AWord) >= 0;
end;

end.

