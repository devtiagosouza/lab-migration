unit TaskTimer;

interface

uses
  System.Diagnostics, System.SysUtils;

type
  TTaskTimer = class
  public
    class procedure Execute(const AProcedure: TProc; var result: string);
  end;

implementation

class procedure TTaskTimer.Execute(const AProcedure: TProc; var result: string);
var
  Stopwatch: TStopwatch;
  Minutos, Segundos, Milissegundos: Integer;
begin
  Stopwatch := TStopwatch.StartNew;
  AProcedure;
  Stopwatch.Stop;

  Minutos := Stopwatch.ElapsedMilliseconds div 60000; // 1 minuto = 60.000 ms
  Segundos := (Stopwatch.ElapsedMilliseconds mod 60000) div 1000;
  Milissegundos := Stopwatch.ElapsedMilliseconds mod 1000;

  result := Format('%s:%s:%s',
    [Minutos.ToString.PadLeft(2,'0'), Segundos.ToString.PadLeft(2,'0'), Milissegundos.ToString.PadLeft(2,'0')]);
end;

end.

