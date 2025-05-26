unit WaitScreen;

interface
  uses untFrmWaitForm, System.Classes, VCL.Forms, System.SysUtils;


 Type TWaitScreen = class

 private

 public
    class procedure Show(const proc : TProc); static;


 end;

implementation

{ TWaitScreen }


class procedure TWaitScreen.Show(const proc : TProc);

begin

   if not Assigned(FrmWait) then
    FrmWait := TFrmWait.Create(nil);

    FrmWait.Show;
    FrmWait.Update;


    TThread.CreateAnonymousThread(
      procedure
      begin
        try
            try
                proc;
           except on e: exception do begin
               raise e;
           end;
           end;
        finally
          TThread.Synchronize(nil,
            procedure
            begin
              FrmWait.Close;
            end);
        end;
      end).Start;

end;

end.
