program DeviceInfo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form9},
  FMX.DeviceInfo in 'FMX.DeviceInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
